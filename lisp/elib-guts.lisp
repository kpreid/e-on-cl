; This Common Lisp code is Copyright 2005-2007 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; Currently, this file is just defined as 'stuff moved from elib.lisp that doesn't actually need to be loaded before other files, so as to reduce dependencies requiring recompiling'.

; --- Auditing ---

(defobject +the-audit-checker+ "org.erights.e.elib.slot.auditChecker"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| ((tw +the-text-writer-guard+))
    (e. tw |print| "__auditedBy")) ; XXX move to e.syntax?
  (:|run| (auditor specimen)
    (as-e-boolean (approvedp auditor specimen))))

;;; --- Basic slots ---


;;; XXX review the actual uses of these various slot implementations; also consider moving them to e-lambdas
;;; XXX look for ways to reduce the duplication of these common makers

(defun import-uncall (fqn)
  `#(,(eelt (vat-safe-scope *vat*) "import__uriGetter") "get" #(,fqn)))

(defobject +the-make-simple-slot+ "org.erights.e.elib.slot.makeFinalSlot"
    (:stamped +deep-frozen-stamp+
     :stamped +pass-by-construction+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeFinalSlot"))
  (:|asType| () (type-specifier-to-guard 'e-simple-slot))
  (:|run| (value)
    (make-instance 'e-simple-slot :value value)))

(defobject +the-make-var-slot+ "org.erights.e.elib.slot.makeVarSlot"
    (:stamped +deep-frozen-stamp+
     :stamped +pass-by-construction+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeVarSlot"))
  (:|asType| () (type-specifier-to-guard 'e-var-slot))
  (:|run| (value)
    (make-instance 'e-var-slot :value value)))

(defobject +the-make-guarded-slot+ "org.erights.e.elib.slot.makeGuardedSlot"
    (:stamped +deep-frozen-stamp+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeGuardedSlot"))
  (:|asType| () (type-specifier-to-guard 'e-guarded-slot))
  (:|run| (guard value opt-ejector)
    (make-instance 'e-guarded-slot :value value
                                   :guard guard
                                   :opt-ejector opt-ejector)))
    
(defclass e-simple-slot () 
  ((value :initarg :value :reader simple-slot-value))
  (:documentation "A normal immutable slot."))

(defmethod print-object ((slot e-simple-slot) stream)
  ; xxx make readable under read-eval?
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "& ~W" (simple-slot-value slot))))

(def-vtable e-simple-slot
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +selfless-stamp+))
  (:|__optUncall| (this)
    `#(,+the-make-simple-slot+ "run" #(,(simple-slot-value this))))
  (:|__printOn| (this (tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (e. tw |print| "<& ")
    (e. tw |quote| (simple-slot-value this))
    (e. tw |print| ">")
    nil)
  (:|getValue| (this)
    "Returns the constant value of this slot."
    (simple-slot-value this))
  (:|setValue| (this new-value)
    "Always fails."
    (declare (ignore new-value))
    (error "not an assignable slot: ~A" (e-quote this)))
  (:|isFinal| (this)
    "Returns true."
    (declare (ignore this))
    +e-true+))

(defmethod make-load-form ((a e-simple-slot) &optional environment)
  (make-load-form-saving-slots a :environment environment))  

(defclass base-closure-slot (vat-checking)
  ((getter :initarg :getter
           :type (function () t)
           :accessor closure-slot-getter)
   (setter :initarg :setter
           :type (function (t) *)
           :accessor closure-slot-setter)))

(def-vtable base-closure-slot
  (:|getValue| (this)
    (funcall (closure-slot-getter this)))
  (:|isFinal| (this)
    (declare (ignore this))
    +e-false+))

(defmethod shared-initialize :after ((slot base-closure-slot) slot-names &key (value nil value-supplied) &allow-other-keys)
  (declare (ignore slot-names))
  (with-accessors ((getter closure-slot-getter) 
                   (setter closure-slot-setter))
                  slot
    (when value-supplied
      (assert (not (slot-boundp slot 'getter)))
      (assert (not (slot-boundp slot 'setter)))
      (setf getter (lambda () value)
            setter (lambda (new) (setf value new))))))
  

(defclass e-var-slot (base-closure-slot) 
  ())

(def-vtable e-var-slot
  (:|__printOn| (this (tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (e. tw |print| "<var ")
    (e. tw |quote| (e. this |getValue|))
    (e. tw |print| ">"))
  (:|__optUncall| (this)
    `#(,+the-make-var-slot+ "run" #(,(e. this |getValue|))))
  (:|setValue| (this new-value)
    (funcall (closure-slot-setter this) new-value)
    nil))

(defmethod print-object ((slot e-var-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W" (e. slot |getValue|))))

(defclass e-guarded-slot (base-closure-slot) 
  ((guard :initarg :guard :reader guarded-slot-guard)))

(defmethod shared-initialize :around ((slot e-guarded-slot) slot-names &rest initargs &key (value nil value-supplied) guard opt-ejector &allow-other-keys)
  "if this slot is initialized with a value, it is coerced; if initialized with a getter and setter, it is assumed to be correct"
  (declare (ignore slot-names))
  (if value-supplied
    (apply #'call-next-method slot slot-names
      :value (e. guard |coerce| value opt-ejector)
      initargs)
    (call-next-method)))

(def-vtable e-guarded-slot
  (:|__printOn| (this (tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (with-accessors ((guard guarded-slot-guard) 
                     (getter closure-slot-getter))
                    this
      (e. tw |print| "<var ")
      (e. tw |quote| (funcall getter))
      (e. tw |print| " :")
      (e. tw |quote| guard)
      (e. tw |print| ">")))
  (:|setValue| (this new-value)
    (funcall (closure-slot-setter this) 
      (e. (guarded-slot-guard this) |coerce| new-value nil))
    nil))

(defmethod print-object ((slot e-guarded-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W :~W" 
      (if (slot-boundp slot 'getter) 
        (e. slot |getValue|)
        '#:<unbound-getter>)
      (ignore-errors (guarded-slot-guard slot)))))

;; XXX this is duplicated with information in the maker functions
(def-fqn e-simple-slot  "org.erights.e.elib.slot.FinalSlot")
(def-fqn e-var-slot     "org.erights.e.elib.slot.VarSlot")
(def-fqn e-guarded-slot "org.erights.e.elib.slot.GuardedSlot")

;;; --- e-boolean ---

(def-vtable e-boolean
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    "Booleans are atomic."
    (or (eql auditor +deep-frozen-stamp+)
        (eql auditor +standard-graph-exit-stamp+)
        (eql auditor +thread-sharable-stamp+)))

  (:|__printOn| (this (tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (e. tw |print| (if (e-is-true this)
                     "true"
                     "false")))
  (:|and| (this (other 'e-boolean))
    "Boolean and."
    (if (e-is-true this) other +e-false+))
  (:|not| (this)
    "Boolean negation."
    (if (eql this +e-false+) +e-true+ +e-false+))
  (:|or| (this (other 'e-boolean))
    "Boolean or."
    (if (e-is-true this) +e-true+ other))
  (:|pick| (this true-value false-value)
    "Return the first argument if this is true, otherwise return the second argument."
    (if (e-is-true this) true-value false-value))
  (:|xor| (this (other 'e-boolean))
    "Boolean exclusive or."
    (if (e-is-true this) (e. other |not|) other)))

; --- Throw and Ejector ---

(defobject +the-thrower+ "org.erights.e.elib.prim.throw"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| ((tw +the-text-writer-guard+))
    (e. tw |print| "throw")
    nil)
  (:|run| (problem)
    (error (e-problem-to-condition (e-coerce problem 'condition))))
  (:|eject| (opt-ejector problem)
    (eject-or-ethrow opt-ejector (e-problem-to-condition problem)))
  (:|free| (problem)
    ; XXX there should be a function for this
    ;; XXX this should be removed in accordance with the new sealer-based async exception plan
    (error (if *compatible-catch-leakage*
             problem
             (make-condition 'elib::free-problem :value problem)))))

(defun ejector-prethrow (ejector-spec value)
  "Implements elib:*break-on-ejections*, analogously to cl:*break-on-signals*."
  (when (typep value *break-on-ejections*)
    (break "About to exit via ~W with value ~W" ejector-spec value))
  nil)

(defun %ejector-throw (label function value)
  (ejector-prethrow label value)
  (handler-case (funcall function value)
    ;; XXX this is making assumptions about what the function will do out of scope
    ; xxx should define vtable for cant-throw-error instead of making a new condition?
    ;     (of course, we must make a new condition if the implementation doesn't have such a distinct condition type)
    ;; XXX why are we not restricting to control-error in general? some lisp that doesn't signal it when it should?
    (#+ccl ccl::cant-throw-error 
     #+(or sbcl allegro) control-error
     #-(or ccl sbcl) t
      ()
      (error "ejector ~S no longer in scope" label))))

(defun ejector (label fn)
  (e-lambda "org.erights.e.elib.base$ejector" ()
    (:|__printOn| ((tw +the-text-writer-guard+))
      (e. tw |print| "<" label " ejector>"))
    (:|run| ()      (%ejector-throw label fn nil))
    (:|run| (value) (%ejector-throw label fn value))))

; --- Type/FQN mapping miscellaneous ---

; - observable-type-of - 

(defmethod observable-type-of ((specimen ref)
    &aux (short (ref-shorten specimen)))
  "Ref-shortening case."
  (if (eql short specimen)
    't
    (observable-type-of short)))

(defmethod observable-type-of ((specimen t))
  (typecase specimen
    ; XXX we can't just specialize on float64/float32 because they aren't class names
    (float64 'float64)
    (float32 'float32)
    
    (otherwise (class-name (class-of specimen)))))

(defmethod observable-type-of ((a function))
  (declare (ignore a)) 
  't)

; - fqn/parameters base cases - 

(defmethod cl-type-fq-name ((type-sym t))
  (with-standard-io-syntax
    (format nil "org.cubik.cle.native.~A.~A" 
      (guess-lowercase-string (package-name (symbol-package type-sym)))
      (guess-lowercase-string (symbol-name type-sym)))))

(defmethod cl-type-parameters ((type-sym t) parameters)
  (if parameters
    (e-quote (coerce parameters 'vector))
    ""))

; - separating compound type specifiers - 

(defmethod cl-type-fq-name ((type cons))
  (cl-type-fq-name (car type)))
  
(defmethod cl-type-parameters ((type cons) non-parameters)
  (assert (null non-parameters))
  (cl-type-parameters (first type) (rest type)))

; - classes as type specifiers -

(defmethod cl-type-fq-name ((type-specifier class))
  (cl-type-fq-name (class-name type-specifier)))

(defmethod cl-type-parameters ((type-specifier class) parameters)
  (cl-type-parameters (class-name type-specifier) parameters))

; - misc fqns - 

; XXX move these to near the relevant def-vtable, when possible
(def-fqn t "org.cubik.cle.prim.any")
(def-fqn nil "org.cubik.cle.prim.none")

; XXX this is rather odd, but we do want the 'any' print
(def-fqn and "org.cubik.cle.prim.types.all")
(def-fqn or "org.cubik.cle.prim.types.any")

(def-fqn vicious-cycle-error "org.erights.e.elib.ref.ViciousCycleException")
(def-fqn insufficiently-settled-error "org.erights.e.elib.tables.InsufficientlySettledException")
(def-fqn not-settled-error "org.erights.e.elib.tables.NotSettledException")

(def-fqn string "org.erights.e.elib.tables.String")
(def-fqn float64 "org.cubik.cle.prim.float64")
(def-fqn float32 "org.cubik.cle.prim.float32")
(def-fqn condition "org.cubik.cle.native.Throwable") ; xxx Java name
(def-fqn e-boolean "org.cubik.cle.native.boolean")
(def-fqn null "org.cubik.cle.prim.Null")
(def-fqn character "org.cubik.cle.prim.char")
(def-fqn vector "org.erights.e.elib.tables.ConstList")
(def-fqn type-desc "org.erights.e.elib.base.TypeDesc")
(def-fqn message-desc "org.erights.e.elib.base.MessageDesc")
(def-fqn param-desc "org.erights.e.elib.base.ParamDesc")
(def-fqn local-throw-sealed-box "org.cubik.cle.prim.LocalThrowSealedBox")

(loop for group-type in '(and or) do
  (defmethod cl-type-parameters ((type (eql group-type)) parameters)
    (with-output-to-string (stream)
      (format stream "[" #|]|#)
      (loop for sep = "" then ", "
            for x in parameters
            do (format stream "~A~A" sep (cl-type-simple-expr x)))
      (format stream #|[|# "]"))))

(defmethod cl-type-parameters ((type (eql 'vector)) parameters)
  (destructuring-bind (&optional (element-type 't) (length '*)) parameters
    (setf element-type 
      (upgraded-array-element-type
        (if (eql element-type '*) 
          't
          element-type)))
    (cond
      ((not (eql length '*))
        (format nil "[~A, ~A]" (cl-type-simple-expr element-type) length))
      ((not (eql element-type 't))
        (format nil "[~A]" (cl-type-simple-expr element-type)))
      (t 
        ""))))

(defun cl-type-simple-expr (type)
  (concatenate 'string
    (simplify-fq-name (cl-type-fq-name type)) 
    (cl-type-parameters type nil)))

; --- priority queue ---

(defobject +the-make-priority-queue+ "org.cubik.cle.prim.makePriorityQueue" ()
  (:|run| ()
    (make-instance 'priority-queue)))

(def-vtable priority-queue
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |print| "<priority queue of " (priority-queue-length this) ">"))
  (:|peek| (this absent-thunk)
    (block nil
      (let ((p (priority-queue-peek this (lambda () (return (efuncall absent-thunk))))))
        (vector (car p) (cdr p)))))
  (:|pop| (this)
    (let ((p (priority-queue-pop this)))
      (vector (car p) (cdr p))))
  (:|put| (this (key 'real) value)
    (priority-queue-put this key value))
  (:|asList| (this)
    (map 'vector 
         #'(lambda (c) (vector (car c) (cdr c)))
         (priority-queue-snapshot this))))

; --- !!! SBCL-guts-specific optimization gimmicks ---

;;; If this code causes trouble in future SBCL versions, commenting it out should result in a working but possibly slower system.

#+sbcl
(defun sbcl-derive-dispatch-result (call) 
  (declare (type sb-c::combination call)
           (optimize speed))
  (let* ((dargs (sb-c::basic-combination-args call))
         (verb-lvar (nth 1 dargs)))
    (case (if (sb-c::constant-lvar-p verb-lvar)
            (sb-c::lvar-value verb-lvar)
            (return-from sbcl-derive-dispatch-result))
      (:|coerce/2|
        ;; once we know the mverb, we know the arg count
        (destructuring-bind (guardl verb-lvar specimenl ejectorl) dargs
          (declare (ignore verb-lvar specimenl ejectorl))
          (when (sb-c::constant-lvar-p guardl)
            (let ((guard (sb-c::lvar-value guardl)))
              (when (typep guard 'cl-type-guard)
                #+(or) (efuncall e.knot:+sys-trace+ (format nil "~&deriving guard call type ~S~%" (cl-type-specifier guard)))
                (sb-c::ir1-transform-specifier-type 
                  (cl-type-specifier guard))))))))))

#+sbcl 
(sb-c:defknown e-call-dispatch (t t &rest t) t
  (sb-c:any)
  :derive-type #'sbcl-derive-dispatch-result)

#+sbcl (sb-c:deftransform e-call-dispatch
    ((target mverb &rest args)
     (function t &rest t))
  "optimize e-call to function call"
  (let ((arg-names (mapcar (lambda (x)
                             (declare (ignore x))
                             (gensym "E-CALL-ARG"))
                           args)))
    `(lambda (target mverb ,@arg-names)
      (funcall target mverb ,@arg-names))))
