; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.knot)

; --- Scope objects ---

(defclass scope () 
  ((fqn-prefix :initarg :fqn-prefix 
               :initform "__unnamed_outer"
               :type string)
   (slot-table :initarg :slot-table
               :type hash-table
               :initform (error "slot-table unspecified"))
   (slot-ordering-cache :initform nil :type (or null (vector string)))
   (local-definitions :initarg :local-definitions
                      :type hash-table
                      :initform (error "local-definitions unspecified"))))

(def-fqn scope "org.erights.e.elang.scope.Scope") ; XXX should we have a non-elang fqn?

(defun scope-slot-ordering (scope)
  (with-slots (slot-table slot-ordering-cache) scope
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

(defglobal +the-make-scope+ (e-lambda "org.erights.e.elang.scope.makeScope"
    (:stamped +deep-frozen-stamp+)
  (:|asType| ()
    (make-instance 'cl-type-guard :type-specifier 'scope))
  (:|_make| (state fqn-prefix local-definitions)
    "XXX this is the uncall constructor, in need of a better interface"
    (e-coercef state +the-any-map-guard+)
    (e-coercef fqn-prefix 'string)
    (e-coercef local-definitions 'vector)
    (make-scope fqn-prefix
                (loop with (keys values) = (coerce (e. state |getPair|) 'list)
                      for key across keys
                      for value across values
                      collect (list (e-coerce key 'string) value))
                (map 'vector (lambda (k) (e-coerce k 'string)) 
                             local-definitions)))
  (:|fromState| (state fqn-prefix)
    "XXX document"
    (e-coercef state +the-any-map-guard+)
    (e-coercef fqn-prefix 'string)
    (make-scope fqn-prefix
                (loop with (keys values) = (coerce (e. state |getPair|) 'list)
                      for key across keys
                      for value across values
                      collect (list (e-coerce key 'string) value))))))

; XXX reduce code duplication among get/fetch methods

(declaim (inline copy-hash-table-entries))
(defun copy-hash-table-entries (source destination &key (test (constantly t)))
  (loop for noun being each hash-key of source using (hash-value slot) 
        when (funcall test noun)
        do (setf (gethash noun destination) slot)))

(def-vtable scope
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<scope " (slot-value this 'fqn-prefix) ">")
    nil)
  (:|__optUncall| (this)
    (with-slots (slot-table fqn-prefix local-definitions) this
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
           ,fqn-prefix
           ,(sort (map-from-hash 'vector
                    (lambda (noun junk) (declare (ignore junk)) noun)
                    local-definitions)
                  #'string<)))))
  (:|or| (inner outer)
    "Return a scope which maps all nouns either scope does, preferring this scope's slots. The FQN prefix will be that of this scope."
    (e-coercef outer 'scope)
    (flet ((overlay (slot-name)
             (let ((new-table (make-hash-table 
                                :test #'equal 
                                :size (hash-table-count (slot-value outer slot-name)))))
          (loop for old-scope in (list outer inner)
                for old-table = (slot-value old-scope slot-name)
                do
            (copy-hash-table-entries old-table new-table))
          new-table)))
    (make-instance 'scope
      :fqn-prefix        (slot-value inner 'fqn-prefix)
      :slot-table        (overlay 'slot-table)
      :local-definitions (overlay 'local-definitions))))
  (:|maps| (this noun)
    "Return whether this scope has a slot for the given noun string."
    (e-coercef noun 'string)
    (with-slots (slot-table) this
      (as-e-boolean (nth-value 1 (gethash noun slot-table)))))
  (:|get| (scope noun)
    "Return the value of this scope's slot for the given noun string, or throw if it has no slot."
    (e. (e. scope |getSlot| noun) |getValue|))
  (:|getSlot| (this noun)
    "Return this scope's slot for the given noun string, or throw if it has no slot."
    (e-coercef noun 'string)
    (with-slots (slot-table) this
      (multiple-value-bind (slot present) (gethash noun slot-table)
        (if present
          slot
          (error "binding not in scope: ~A" (e-quote noun))))))
  (:|fetch| (this noun absent-thunk)
    "Return the value of this scope's slot for the given noun string, or the result of absent-thunk if it has no slot."
    (e-coercef noun 'string)
    (with-slots (slot-table) this
      (multiple-value-bind (slot present) (gethash noun slot-table)
        (if present
          (e. slot |getValue|)
          (e. absent-thunk |run|)))))
  (:|put| (this noun value)
    "Set the value of this scope's slot for the given noun string, or throw if it has no slot."
    (e. (e. this |getSlot| noun) |setValue| value))
  (:|getState| (this)
    "Return a ConstMap containing the bindings in this scope, as \"&\" + noun => slot."
    (e. +the-make-const-map+ |fromIteratable| this +e-true+))
  (:|iterate| (scope afunc)
    "Iterate over the bindings in this scope, as \"&\" + noun => slot."
    (with-slots (slot-table) scope
      (loop for noun across (scope-slot-ordering scope) do
        (e. afunc |run| (concatenate 'string "&" noun) 
                        (gethash noun slot-table)))
      nil))
  (:|nestOuter| (scope)
    (make-instance 'scope 
      :fqn-prefix (slot-value scope 'fqn-prefix)
      :slot-table (slot-value scope 'slot-table)
      :local-definitions (make-hash-table :test #'equal)))
  (:|with| (scope noun value)
    "Return a scope which has an immutable slot for 'value' bound to 'noun', and this scope's other bindings and FQN prefix."
    (e. scope |withSlot| noun (make-instance 'e-simple-slot :value value)))
  (:|withSlot| (scope new-noun new-slot)
    "Return a scope which has 'new-slot' bound to 'new-noun', and this scope's other bindings and FQN prefix."
    (e-coercef new-noun 'string)
    (when (gethash new-noun (slot-value scope 'local-definitions))
      (error "~A already in scope" (e-quote new-noun)))
    ; xxx support efficient accumulation?
    (with-slots (fqn-prefix slot-table local-definitions) scope
      (make-instance 'scope 
        :fqn-prefix fqn-prefix
        :slot-table
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries slot-table new-table)
            (setf (gethash new-noun new-table) new-slot)
            new-table)
        :local-definitions
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries local-definitions new-table)
            (setf (gethash new-noun new-table) t)
            new-table))))
  (:|withPrefix| (scope new)
    "Return a scope which is identical to this scope, except for having the given FQN prefix."
    (e-coercef new 'string)
    (with-slots (slot-table local-definitions) scope
      (make-instance 'scope 
        :fqn-prefix new
        :slot-table slot-table
        :local-definitions local-definitions)))
  (:|getFQNPrefix| (scope)
    (slot-value scope 'fqn-prefix))
  
  (:|optExtract| (this key)
    "Same as ConstMap#optExtract/1. Added to support using Scopes in map-patterns."
    (block nil
      (vector
        (e. this |fetch| key (efun () (return nil)))
        (e. this |without| key))))
  (:|extract| (this key default)
    "Same as ConstMap#extract/1. Added to support using Scopes in map-patterns."
    (vector
      (e. this |fetch| key (efun () default))
      (e. this |without| key)))

  (:|without| (scope removed-noun)
    "Same as ConstMap#without/1. Added to support using Scopes in map-patterns."
    (e-coercef removed-noun 'string)
    (with-slots (slot-table fqn-prefix local-definitions) scope
      (make-instance 'scope 
        :fqn-prefix fqn-prefix
        :slot-table
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries slot-table new-table
              :test (lambda (noun) (string/= noun removed-noun)))
            new-table)
        :local-definitions
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (copy-hash-table-entries local-definitions new-table
              :test (lambda (noun) (string/= noun removed-noun)))
            new-table)))))

(defmethod eeq-is-transparent-selfless ((a scope))
  (declare (ignore a))
  t)

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
               (e. (se-printer condition) |run| tw condition)))))

(defun property-name-to-get-verb (name)
  (if (string= name "")
    "get"
    (concatenate 'string "get" 
                         (string (aref name 0)) 
                         (subseq name 1))))

(def-vtable e-structure-exception
  (:|__printOn| (condition tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |write| "problem: ")
    (e. (se-printer condition) |run| tw condition))
  (:|__getAllegedType| (condition)
    (e. +the-make-type-desc+ |run|
      "StructureException instance type" 
      nil
      (map 'vector
           (lambda (type)
             (e. +the-make-type-desc+ |run|
               "StructureException autodefined supertype" type #() #() #()))
           (se-types condition))
      #()
      (map 'vector
           (lambda (name)
             (e-coercef name 'string)
             (e. +the-make-message-desc+ |run|
               "" (property-name-to-get-verb name) #() nil))
           (e. (se-properties condition) |getKeys|))))
  (:|_getProperties/0| 'se-properties))

(defmethod e-call-match ((rec e-structure-exception) mverb &rest args)
  (let ((name (without-prefix (unmangle-verb mverb) "get")))
    (if (and name (null args))
      (let ((pname (concatenate 'string
                     (string (char-downcase (aref name 0)))
                     (subseq name 1))))
        (e. (se-properties rec) |fetch| pname (efun () (return-from e-call-match (call-next-method)))))
      (call-next-method))))

(defglobal +the-make-exception+ (e-lambda "org.cubik.cle.prim.makeException" 
    (:stamped +deep-frozen-stamp+)
  (:|run| (types properties printer)
    (setf types (map 'vector (lambda (x) (e-coercef x 'string)) (e-coerce types 'vector)))
    (e-coercef properties +the-map-guard+)
    (e-coercef printer (e. (vat-safe-scope *vat*) |get| "DeepFrozen"))
    (make-condition
      'e-structure-exception
      :types types
      :properties properties
      :printer printer))))


; --- standard scope definitions ---

(defglobal +the-looper+ (e-lambda "org.erights.e.elang.interp.loop" 
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<__loop>")
    nil)
  (:|run| (body)
    "Call body.run(), which must return a boolean, until it returns false."
    (loop while (e-is-true (e. body |run|))))))

(defglobal +the-thrower+ (e-lambda "org.erights.e.elib.prim.throw"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "throw")
    nil)
  (:|run| (problem)
    (error (e-problem-to-condition (e-coerce problem 'condition))))
  (:|eject| (opt-ejector problem)
    (elib:eject-or-ethrow opt-ejector (e-problem-to-condition problem)))
  (:|free| (problem)
    ; XXX there should be a function for this
    (error (if *compatible-catch-leakage*
             problem
             (make-condition 'elib::free-problem :value problem))))))

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

(defglobal +make-first-char-splitter+ (e-lambda "org.quasiliteral.text.makeFirstCharSplitter" ()
  (:|run| (specials)
    (e-coercef specials 'string)
    (flet ((match (ch) (position ch specials)))
      (e-lambda "org.quasiliteral.text.FirstCharSplitter" ()
        (:|findIn| (str)
          "Equivalent to .findInFrom(str, 0)."
          (e-coercef str 'string)
          (or (position-if #'match str)
              -1))
        (:|findInFrom| (str start) ; XXX write tests
          "Return the first index greater than 'start' of a character of 'str' which is one of the special characters of this splitter, or -1 if no such index exists."
          (e-coercef str 'string)
          (e-coercef start `(integer 0 ,(length str)))
          (or (position-if #'match str :start start)
              -1)))))))

(defglobal +the-get-character+ (e-lambda "org.cubik.cle.prim.getCharacter" ()
  (:|run| (codepoint)
    ;; XXX Unicode assumption
    (or (code-char codepoint)
        (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
          (error "character U+~16,4,'0R not supported"))))))

(defun uncall-to-unget (loader portrayal)
  (e-coercef portrayal '(or null vector))
  ; xxx I do not understand the justification in the doc-comment of Java-E baseLoader#optUnget.
  "Converts an optional uncall value into an optional unget value. That is, returns 'name' if 'portrayal' matches [==loader, ==\"get\", [name]]; otherwise null."
  (when (and portrayal 
             (=               (length portrayal) 3)
             (eeq-is-same-yet (aref portrayal 0) loader)
             (eeq-is-same-yet (aref portrayal 1) "get"))
    (let ((args (e-coerce (aref portrayal 2) 'vector)))
      (when (= (length args) 1)
        (aref args 0)))))

(defun unget-to-uncall (loader name)
  "Converts an optional unget value into an optional uncall value. Currently does not check whether 'name' is a string."
  (if name
    `#(,loader "get" #(,name))))

;;; --- Additional E/Lisp bridging ---

(defun e-to-lisp-function (e-function)
  "Wrap an E function (object with run/* methods) as an equivalent Lisp function."
  (lambda (&rest args) (e-call e-function "run" args)))

(defun e-to-lisp-mv-function (e-function)
  "Wrap an E function (object with run/* methods) as an equivalent Lisp function, treating a returned ConstList as multiple values."
  (lambda (&rest args) (values-list (coerce (e-coerce (e-call e-function "run" args) 'vector) 'list))))

;;; --- scope construction utilities ---

; XXX #+eventually-frozen-path-loader is not expected to be in *features* - because this code isn't working yet - it's just a descriptive commenting-out 

(defglobal +the-make-path-loader+ (e-lambda "org.cubik.cle.prim.makePathLoader"
    (:stamped +deep-frozen-stamp+)
  (:|run| (name fetchpath
      &aux #+eventually-frozen-path-loader (eventually-deep-frozen (e. (e. (vat-safe-scope *vat*) |get| "DeepFrozen") |eventually|)))
    (e-coercef name 'string)
    (e-coercef fetchpath 'vector)
    (e-lambda |loader| ()
      ; :stamped (deep-frozen-if-every fetchpath)
      #+eventually-frozen-path-loader :stamped
      #+eventually-frozen-path-loader eventually-deep-frozen
      (:|__printOn| (tw)
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |print| "<" name ":*>")
        nil)
      #+eventually-frozen-path-loader (:|__optSealedDispatch| (brand)
        ; XXX this implementation of the EventuallyDeepFrozen state check is not really *correct* per the auditor's rules, but will handle the correct cases, since we know that the parts of this which are not actually DeepFrozen 
        (cond
          ((eeq-is-same-ever brand (e. eventually-deep-frozen |getPeekBrand|))
            (e. (e. eventually-deep-frozen |getPeekSealer|) 
                |seal|
                (e. +the-make-const-map+ |fromPairs|
                  `#(#("&fetchpath" ,(make-instance 'e-simple-slot :value fetchpath))))))))
      (:|fetch| (fqn absent-thunk)
        (e-coercef fqn 'string)
        (if (string= ".*" fqn :start2 (- (length fqn) 2))
          (e. (e-import "org.erights.e.elang.interp.makePackageLoader") |run| |loader| (concatenate 'string name ":") fqn)
          (loop for sub across fetchpath
                do (block continue (return (e. sub |fetch| fqn (e-lambda "$continueSearchThunk" () (:|run| () (return-from continue))))))
                finally (e. absent-thunk |run|))))
      (:|get| (fqn) 
        (e. |loader| |fetch| fqn 
          (e-lambda "$getFailureThunk" () (:|run| () (error "~A can't find ~A" (e-quote |loader|) (e-quote fqn))))))
      (:|optUncall| (specimen)
        (loop for sub across fetchpath thereis
          (and (e-is-true (e. sub |__respondsTo| "optUnget" 1))
               (progn
                 ;(format t "~&; ~A for optUnget of ~A querying sub ~A~%" (e-quote |loader|) (e-quote specimen) (e-quote sub))
                 (unget-to-uncall |loader| (e. sub |optUnget| specimen))))))
      (:|optUnget| (specimen)
        ; xxx this is how Java-E does it, and claims a justification, but *what*?
        (uncall-to-unget |loader| (e. |loader| |optUncall| specimen)))))))
