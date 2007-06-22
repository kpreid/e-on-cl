; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.knot)

; --- Scope objects ---

(defclass scope () 
  ((fqn-prefix :initarg :fqn-prefix 
               :initform "__unnamed_outer"
               :type string
               :reader scope-fqn-prefix)
   (slot-table :initarg :slot-table
               :type hash-table
               :initform (error "slot-table unspecified")
               :reader slot-table)
   (slot-ordering-cache :initform nil :type (or null (vector string))
                        :accessor slot-ordering-cache)
   (local-definitions :initarg :local-definitions
                      :type hash-table
                      :initform (error "local-definitions unspecified")
                      :reader local-definitions)))

(def-fqn scope "org.erights.e.elang.scope.scope") ; XXX should we have a non-elang fqn?

(defun scope-slot-ordering (scope)
  (with-accessors ((slot-table slot-table) (slot-ordering-cache slot-ordering-cache)) scope
    (or slot-ordering-cache
        (setf slot-ordering-cache
          (sort (map-from-hash 'vector
                  (lambda (noun slot) (declare (ignore slot)) noun)
                  slot-table)
                #'string<)))))

(defun make-scope (fqn-prefix init-list &optional local-definitions)
  (make-instance 'scope
    :fqn-prefix fqn-prefix
    :slot-table
      (let ((table (make-hash-table :test #'equal 
                                    :size (length init-list))))
        (loop for (varspec value) in init-list do
          (if (eql 0 (position #\& varspec))
            (setf (gethash (subseq varspec 1) table) 
                  value)
            (setf (gethash varspec table) 
                  (make-instance 'elib:e-simple-slot :value value))))
        table)
    :local-definitions
      (if local-definitions
        (let ((table (make-hash-table :test #'equal 
                                      :size (length local-definitions))))
          (loop for noun across local-definitions do
            (setf (gethash noun table) t))
          table)
        (let ((table (make-hash-table :test #'equal 
                                      :size (length init-list))))
          (loop for (varspec value) in init-list do
            (if (eql 0 (position #\& varspec))
              (setf (gethash (subseq varspec 1) table) t)
              (setf (gethash varspec table) t)))
          table))))

(defobject +the-make-scope+ "org.erights.e.elang.scope.makeScope"
    (:stamped +deep-frozen-stamp+
     :stamped +standard-graph-exit-stamp+)
  (:|asType| ()
    (type-specifier-to-guard 'scope))
  (:|_make| ((state +the-any-map-guard+)
             (fqn-prefix 'string)
             (local-definitions 'vector))
    "XXX this is the uncall constructor, in need of a better interface"
    (make-scope fqn-prefix
                (loop with (keys values) = (coerce (e. state |getPair|) 'list)
                      for key across keys
                      for value across values
                      collect (list (e-coerce key 'string) value))
                (map 'vector (lambda (k) (e-coerce k 'string)) 
                             local-definitions)))
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

(def-vtable scope
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +selfless-stamp+))

  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |print| "<scope " (scope-fqn-prefix this) ">")
    nil)
  (:|__optUncall| (this)
    (let ((slot-table (slot-table this)))
      `#(,+the-make-scope+
         "_make" 
         #(,(e. +the-make-const-map+ |fromPairs|
              (map 'vector
                (lambda (noun &aux (slot (gethash noun slot-table)))
                  (if (and (typep slot 'e-simple-slot) 
                           (not (e-is-true (e. noun |startsWith| "&"))))
                    (vector noun (e. slot |getValue|))
                    (vector (concatenate 'string "&" noun) slot)))
                (scope-slot-ordering this)))
           ,(scope-fqn-prefix this)
           ,(sort (map-from-hash 'vector
                    (lambda (noun junk) (declare (ignore junk)) noun)
                    (local-definitions this))
                  #'string<)))))
  (:|or| (inner (outer 'scope))
    "Return a scope which maps all nouns either scope does, preferring this scope's slots. The FQN prefix will be that of this scope."
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
      :slot-table        (overlay 'slot-table)
      :local-definitions (overlay 'local-definitions))))
  (:|fetchSlot| (this (noun 'string) absent-thunk)
    "Return this scope's slot for the given noun string, or the result of absent-thunk if it has no slot."
    (multiple-value-bind (slot present) (gethash noun (slot-table this))
      (if present
        slot
        (efuncall absent-thunk))))
  (:|fetch| (scope (noun 'string) absent-thunk)
    ;; nonprimitive; could be sugar, except that it's used by the knot's
    ;; loader setup before there is a vat for sugar-cache-call to use
    "Return this scope's slot for the given noun string, or throw if it has no slot."
    (block nil
      (e. (e. scope |fetchSlot| noun 
            (efun () (return (efuncall absent-thunk))))
          |getValue|)))
  (:|iterate| (scope afunc)
    "Iterate over the bindings in this scope, as \"&\" + noun => slot."
    (let ((slot-table (slot-table scope)))
      (loop for noun across (scope-slot-ordering scope) do
        (efuncall afunc (concatenate 'string "&" noun) 
                        (gethash noun slot-table)))
      nil))
  (:|nestOuter| (scope)
    (make-instance 'scope 
      :fqn-prefix (scope-fqn-prefix scope)
      :slot-table (slot-table scope)
      :local-definitions (make-hash-table :test #'equal)))
  (:|withSlot| (scope (new-noun 'string) new-slot)
    "Return a scope which has 'new-slot' bound to 'new-noun', and this scope's other bindings and FQN prefix."
    (when (gethash new-noun (local-definitions scope))
      (error "~A already in scope" (e-quote new-noun)))
    ; xxx support efficient accumulation?
    (let ((slot-table (slot-table scope)))
      (make-instance 'scope 
        :fqn-prefix (scope-fqn-prefix scope)
        :slot-table
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries slot-table new-table)
            (setf (gethash new-noun new-table) new-slot)
            new-table)
        :local-definitions
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries (local-definitions scope) new-table)
            (setf (gethash new-noun new-table) t)
            new-table))))
  (:|withPrefix| (scope (new 'string))
    "Return a scope which is identical to this scope, except for having the given FQN prefix."
    (make-instance 'scope 
      :fqn-prefix new
      :slot-table (slot-table scope)
      :local-definitions (local-definitions scope)))
  (:|getFQNPrefix/0| 'scope-fqn-prefix)
  
  (:|without| (scope (removed-noun 'string))
    "Same as ConstMap#without/1. Added to support using Scopes in map-patterns."
    (let ((slot-table (slot-table scope)))
      (make-instance 'scope 
        :fqn-prefix (scope-fqn-prefix scope)
        :slot-table
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries slot-table new-table
              :test (lambda (noun) (string/= noun removed-noun)))
            new-table)
        :local-definitions
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries (local-definitions scope) new-table
              :test (lambda (noun) (string/= noun removed-noun)))
            new-table)))))

(defmethod e-call-match (fail (rec scope) mverb &rest args)
  (declare (ignore fail))
  (apply #'sugar-cache-call rec mverb 'scope "org.erights.e.elang.scope.scopeSugar" args))

;;; --- ENode/scope consistency verification ---

;; XXX once we have explicit Kernel-E verification, it should check for *internal* var/:= consistency, which this can't catch.

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

;;; --- structured classless exceptions ---

;; xxx this section to be moved?

(define-condition e-structure-exception (error)
  ((types :initarg :types :reader se-types)
   (properties :initarg :properties :reader se-properties)
   (printer :initarg :printer :reader se-printer))
  (:report (lambda (condition stream)
             (let ((tw (make-text-writer-to-cl-stream stream
                         :autoflush nil
                         :should-close-underlying nil)))
               (efuncall (se-printer condition) tw condition)))))

(defun property-name-to-get-verb (name)
  (if (string= name "")
    "get"
    (concatenate 'string "get" 
                         (string (aref name 0)) 
                         (subseq name 1))))

(def-vtable e-structure-exception
  (:|__printOn| (condition (tw +the-text-writer-guard+))
    (e. tw |write| "problem: ")
    (efuncall (se-printer condition) tw condition))
  (:|__getAllegedType| (condition)
    (efuncall +the-make-type-desc+
      "StructureException instance type" 
      nil
      (map 'vector
           (lambda (type)
             (efuncall +the-make-type-desc+
               "StructureException autodefined supertype" type #() #() #()))
           (se-types condition))
      #()
      (map 'vector
           (lambda (name)
             (e-coercef name 'string)
             (efuncall +the-make-message-desc+
               "" (property-name-to-get-verb name) #() nil))
           (e. (se-properties condition) |getKeys|))))
  (:|_getProperties/0| 'se-properties))

(defmethod e-call-match (fail (rec e-structure-exception) mverb &rest args)
  (declare (ignore fail))
  (let ((name (without-prefix (unmangle-verb mverb) "get")))
    (if (and name (null args))
      (let ((pname (concatenate 'string
                     (string (char-downcase (aref name 0)))
                     (subseq name 1))))
        (e. (se-properties rec) |fetch| pname (efun () (return-from e-call-match (call-next-method)))))
      (call-next-method))))

(defobject +the-make-exception+ "org.cubik.cle.prim.makeException" 
    (:stamped +deep-frozen-stamp+)
  (:|run| ((types 'vector) 
           (properties +the-map-guard+) 
           (printer (eelt (vat-safe-scope *vat*) "DeepFrozen")))
    (setf types (map 'vector (lambda (x) (e-coercef x 'string)) types))
    (make-condition
      'e-structure-exception
      :types types
      :properties properties
      :printer printer)))


; --- standard scope definitions ---

(defobject +the-looper+ "org.erights.e.elang.interp.loop" 
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| ((tw +the-text-writer-guard+))
    (e. tw |print| "<__loop>")
    nil)
  (:|run| (body)
    "Call body.run(), which must return a boolean, until it returns false."
    (loop while (e-is-true (efuncall body)))))

(defun split-fqn-prefix (fqn)
  ; xxx consider replacing with SPLIT-SEQUENCE
  ; XXX write tests for this particular function
  "Return a list of the components of this FQN prefix. XXX need to document precise empty-element and empty-string behavior."
  (let ((pos (position #\. fqn)))
    (if pos
      (cons (subseq fqn 0 pos) (split-fqn-prefix (subseq fqn (1+ pos))))
      (progn
        (assert (string= fqn ""))
        nil))))

(defobject +make-first-char-splitter+
    "org.quasiliteral.text.makeFirstCharSplitter"
    (:stamped +deep-frozen-stamp+)
  ;; In the future, this might become part of something for working with character sets in general, and Unicode character categories. Consider Cocoa's NSCharacterSet and NSScanner.
  (:|run| ((specials 'string))
    (flet ((match (ch) (position ch specials)))
      (e-lambda "org.quasiliteral.text.makeFirstCharSplitter$firstCharSplitter" 
          (:stamped +deep-frozen-stamp+)
        (:|__printOn| ((out +the-text-writer-guard+))
          (e. out |write| "<finds any of ")
          (e. out |quote| specials)
          (e. out |write| ">"))
        (:|findIn| ((str 'string))
          "Equivalent to .findInFrom(str, 0)."
          (or (position-if #'match str)
              -1))
        (:|findInFrom| ((str 'string) (start `(integer 0 ,(length str))))
          ; XXX write tests
          "Return the first index greater than 'start' of a character of 'str' which is one of the special characters of this splitter, or -1 if no such index exists."
          (or (position-if #'match str :start start)
              -1))))))

(defobject +the-get-character+ "org.cubik.cle.prim.getCharacter" ()
  (:|run| (codepoint)
    ;; XXX Unicode assumption
    (or (code-char codepoint)
        (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
          (error "character U+~16,4,'0R not supported")))))

;;; --- Additional E/Lisp bridging ---

(defun e-to-lisp-function (e-function)
  "Wrap an E function (object with run/* methods) as an equivalent Lisp function."
  (lambda (&rest args) (apply #'efuncall e-function args)))

(defun e-to-lisp-mv-function (e-function)
  "Wrap an E function (object with run/* methods) as an equivalent Lisp function, treating a returned ConstList as multiple values."
  (lambda (&rest args) (values-list (coerce (e-coerce (apply #'efuncall e-function args) 'vector) 'list))))
