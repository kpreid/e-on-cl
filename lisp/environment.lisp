; Copyright 2005-2008 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

;; This file is named "environment.lisp" because the SCOPE class is to be renamed to that eventually.

(in-package :e.knot)

; --- Scope objects ---

(defclass scope ()
  ((fqn-prefix :initarg :fqn-prefix 
               :initform "__unnamed_outer"
               :type string
               :reader scope-fqn-prefix)
   (table :initarg table
          :type hash-table
          :initform (error "table unspecified")
          :reader %scope-table)
   (local-definitions :initarg local-definitions
                      :type hash-table
                      :initform (error "local-definitions unspecified")
                      :accessor local-definitions)
   (slot-ordering-cache :initform nil :type (or null (vector string))
                        :accessor %slot-ordering-cache)))

(defmethod print-object ((scope scope) stream)
  (print-unreadable-object (scope stream :type t :identity t)
    (format stream "~S, ~S bound, ~S locals"
      (scope-fqn-prefix scope)
      (hash-table-count (%scope-table scope))
      (hash-table-count (local-definitions scope)))))

(def-fqn scope "org.erights.e.elang.scope.scope") ; XXX should we have a non-elang fqn?

(defun scope-noun-ordering (scope)
  (with-accessors ((table %scope-table)
                   (slot-ordering-cache %slot-ordering-cache))
                  scope
    (or slot-ordering-cache
        (setf slot-ordering-cache
          (sort (map-from-hash 'vector
                  (lambda (noun binding) (declare (ignore binding)) noun)
                  table)
                #'string<)))))

(defun make-binding (slot guard)
  (make-instance 'coerced-slot :value slot :guard guard))

(defun make-scope (fqn-prefix init-list &optional local-definitions)
  (let ((table (make-hash-table :test #'equal
                                :size (length init-list)))
        (l-d-table (make-hash-table :test #'equal
                                    :size (if local-definitions
                                            (length local-definitions)
                                            (length init-list)))))
    (loop for (varspec value opt-guard) in init-list do
      (multiple-value-bind (noun slot)
          (if (eql 0 (position #\& varspec))
            (values (subseq varspec 1) value)
            (values varspec (make-instance 'e-simple-slot :value value)))
        (setf (gethash noun table) (make-binding slot (or opt-guard +the-any-guard+)))
        (unless local-definitions
          (setf (gethash noun l-d-table) t))))
    (when local-definitions
      (loop for noun across local-definitions do
        (setf (gethash noun l-d-table) t)))
  (make-instance 'scope
    :fqn-prefix fqn-prefix
    'table table
    'local-definitions l-d-table)))

(defun make-exposing-scope (fqn-prefix init-list)
  (make-scope fqn-prefix
    (loop for record in init-list collect
      (destructuring-bind (varspec value) record
        (list varspec value 
              (make-instance 'same-guard :allowed
                (if (eql 0 (position #\& varspec))
                  value
                  (make-instance 'e-simple-slot :value value))))))))

(defun with-slot-exposed (scope noun slot)
  (with-binding scope noun
                (make-instance 'coerced-slot
                  :value slot
                  :guard (make-instance 'same-guard :allowed slot))))

(defobject +the-make-scope+ "org.erights.e.elang.scope.makeScope"
    (:stamped +deep-frozen-stamp+
     :stamped +standard-graph-exit-stamp+)
  (:|asType| ()
    (type-specifier-to-guard 'scope))
  (:|run| ((fqn-prefix 'string)
           nonlocal-bindings
           local-bindings)
    "Make a Scope from its FQN prefix and maps of rebindable nouns and local nouns to bindings."
    (let* ((l-size (e. local-bindings |size|))
           (size (+ (e. nonlocal-bindings |size|) l-size))
           (binding-table (make-hash-table :test #'equal :size size))
           (l-d-table     (make-hash-table :test #'equal :size l-size)))
      (e. nonlocal-bindings |iterate| (efun ((k 'string) (v 'coerced-slot))
        (setf (gethash k binding-table) v)))
      (e. local-bindings |iterate| (efun ((k 'string) (v 'coerced-slot))
        (setf (gethash k l-d-table) t
              (gethash k binding-table) v)))
      (make-instance 'scope
        :fqn-prefix fqn-prefix
        'table binding-table
        'local-definitions l-d-table)))
  (:|fromState| ((state +the-any-map-guard+)
                 (fqn-prefix 'string))
    "XXX document"
    (make-scope fqn-prefix
                (loop with (keys values) = (coerce (e. state |getPair|) 'list)
                      for key across keys
                      for value across values
                      collect (list (e-coerce key 'string) value)))))

(declaim (inline copy-hash-table-entries))
(defun copy-hash-table-entries (source destination &key (test (constantly t)))
  (loop for noun being each hash-key of source using (hash-value slot) 
        when (funcall test noun)
        do (setf (gethash noun destination) slot)))

(defglobal +empty-equal-hash-table+ (make-hash-table :test #'equal))

(defun with-binding (scope new-noun new-binding)
  (when (gethash new-noun (local-definitions scope))
    (error "~A already in scope" (e-quote new-noun)))
  ; xxx support efficient accumulation?
  (let ((table (%scope-table scope))
        (local-definitions (local-definitions scope)))
    (make-instance 'scope 
      :fqn-prefix (scope-fqn-prefix scope)
      'table
        (let ((new-table (make-hash-table :test #'equal 
                                          :size (1+ (hash-table-count table)))))
          (copy-hash-table-entries table new-table)
          (setf (gethash new-noun new-table) new-binding)
          new-table)
      'local-definitions
        (let ((new-table (make-hash-table :test #'equal 
                                          :size (1+ (hash-table-count local-definitions)))))
          (copy-hash-table-entries local-definitions new-table)
          (setf (gethash new-noun new-table) t)
          new-table))))

(def-vtable scope
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (or (eql auditor +selfless+)
        (eql auditor +transparent-stamp+)))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |print| "<scope " (scope-fqn-prefix this) ">")
    nil)
  (:|__optUncall| (this)
    (let ((bindings (%scope-table this))
          (nl-map +empty-const-map+)
          (l-map +empty-const-map+))
      (loop for noun across (scope-noun-ordering this) do
        (if (gethash noun (local-definitions this))
          (setf l-map  (e. l-map  |with| noun (gethash noun bindings)))
          (setf nl-map (e. nl-map |with| noun (gethash noun bindings)))))
      `#(,+the-make-scope+ "run" #(,(scope-fqn-prefix this) ,nl-map ,l-map))))
  (:|or| (inner (outer 'scope))
    "Return a scope which maps all nouns either scope does, preferring this scope's bindings. The FQN prefix will be that of this scope."
    (flet ((overlay (accessor)
             (let ((new-table (make-hash-table 
                                :test #'equal 
                                :size (hash-table-count (funcall accessor outer)))))
          (loop for old-scope in (list outer inner)
                for old-table = (funcall accessor old-scope)
                do
            (copy-hash-table-entries old-table new-table))
          new-table)))
    (make-instance 'scope
      :fqn-prefix        (scope-fqn-prefix inner)
      'table             (overlay '%scope-table)
      'local-definitions (overlay 'local-definitions))))
  (:|fetchSlot| (this (noun 'string) absent-thunk)
    "Return this scope's slot for the given noun string, or the result of absent-thunk if it has no slot."
    (multiple-value-bind (binding present) (gethash noun (%scope-table this))
      (if present
        (eelt binding)
        (efuncall absent-thunk))))
  (:|fetch| (scope (noun 'string) absent-thunk)
    ;; nonprimitive; could be sugar, except that it's used by the knot's
    ;; loader setup before there is a vat for sugar-cache-call to use
    "Return this scope's slot for the given noun string, or throw if it has no slot."
    (block nil
      (e. (e. scope |fetchSlot| noun 
            (efun () (return (efuncall absent-thunk))))
          |get|)))
  (:|iterate| (scope afunc)
    "Iterate over the bindings in this scope, as \"&\" + noun => slot. Tentatively deprecated in favor of #slots/0."
    (let ((bindings (%scope-table scope)))
      (loop for noun across (scope-noun-ordering scope) do
        (efuncall afunc (concatenate 'string "&" noun) 
                        (eelt (gethash noun bindings))))
      nil))
  (:|bindings| (scope)
    (e-lambda "$scopeBindings" () 
      (:|fetch| ((noun 'string) absent-thunk)
        "Return this scope's binding for the given noun string, or the result of absent-thunk if it has no such binding."
        (multiple-value-bind (binding present) (gethash noun (%scope-table scope))
          (if present
            binding
            (efuncall absent-thunk))))
      (:|iterate| (f)
        (loop with table = (%scope-table scope)
              for noun across (scope-noun-ordering scope)
              do (efuncall f noun (gethash noun table))))))
  (:|nestOuter| (scope)
    (make-instance 'scope 
      :fqn-prefix (scope-fqn-prefix scope)
      'table (%scope-table scope)
      'local-definitions (make-hash-table :test #'equal)))
  (:|withSlot| (scope (new-noun 'string) new-slot)
    "Return a scope which has 'new-slot' bound to 'new-noun', and this scope's other bindings and FQN prefix."
    (with-binding scope new-noun (make-binding new-slot +the-any-guard+)))
  (:|withBinding| (scope (new-noun 'string) (new-binding 'coerced-slot))
    "Return a scope which has 'new-binding' bound to 'new-noun', and this scope's other bindings and FQN prefix."
    (with-binding scope new-noun new-binding))
  (:|withPrefix| (scope (new 'string))
    "Return a scope which is identical to this scope, except for having the given FQN prefix."
    (make-instance 'scope 
      :fqn-prefix new
      'table (%scope-table scope)
      'local-definitions (local-definitions scope)))
  (:|getFQNPrefix/0| 'scope-fqn-prefix)
  
  (:|without| (scope (removed-noun 'string))
    "Same as ConstMap#without/1. Added to support using Scopes in map-patterns."
    (let ((table (%scope-table scope)))
      (make-instance 'scope 
        :fqn-prefix (scope-fqn-prefix scope)
        'table
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count table)))))
            (copy-hash-table-entries table new-table
              :test (lambda (noun) (string/= noun removed-noun)))
            new-table)
        'local-definitions
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count table)))))
            (copy-hash-table-entries (local-definitions scope) new-table
              :test (lambda (noun) (string/= noun removed-noun)))
            new-table)))))

(defmethod e-call-match (fail (rec scope) mverb &rest args)
  (apply #'sugar-cache-call fail rec mverb 'scope "org.erights.e.elang.scope.scopeSugar" args))

;;; --- ENode/scope consistency verification ---

;; XXX all scope stuff should probably be moved to a different package; its presence in e.knot is historical
(defun require-node-fits-scope (node scope ejector)
  (let ((ss (e. node |staticScope|)))
    (e. (e. ss |namesUsed|) |iterate| (efun (k v)
      (e. scope |fetchSlot| k (efun ()
        ;; XXX message to be revised
        (ejerror ejector "undefined variable: ~A~/e.tables:~span/" k (e. v |getOptSpan|))))
      nil))
    (e. (e. ss |namesSet|) |iterate| (efun (k v)
      ;; XXX isFinal is possibly too loose a check. review.
      (when (e-is-true (e. (e. scope |fetchSlot| k +the-thrower+) |isFinal|))
        ;; XXX message to be revised
        (ejerror ejector "~A is not an assignable variable~/e.tables:~span/" k (e. v |getOptSpan|)))))))

