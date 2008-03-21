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

(define-condition ejector-extent-error (control-error)
  ((ejector-label :initarg :ejector-label
                  :reader ejector-error-ejector-label))
  (:report (lambda (condition stream)
             ;; XXX review wording of this error
             (format stream "ejector ~S no longer in scope"
               (ejector-error-ejector-label condition)))))

(declaim (inline %ejector-throw))
(defun %ejector-throw (label function value)
  (ejector-prethrow label value)
  (funcall function value)
  (error "ejector ~S implementation function ~S returned!" label function))

(defun ejector (label fn)
  "Make an ejector object with label LABEL with destination defined by FN. When out of extent, FN should signal EJECTOR-EXTENT-ERROR."
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

(def-fqn vicious-cycle-error "org.erights.e.elib.ref.viciousCycleException")
(def-fqn insufficiently-settled-error "org.erights.e.elib.tables.insufficientlySettledException")
(def-fqn not-settled-error "org.erights.e.elib.tables.notSettledException")

(def-fqn string "org.erights.e.elib.tables.string")
(def-fqn float64 "org.cubik.cle.prim.float64")
(def-fqn float32 "org.cubik.cle.prim.float32")
(def-fqn condition "org.cubik.cle.native.throwable") ; xxx Java name
(def-fqn e-boolean "org.cubik.cle.native.boolean")
(def-fqn null "org.cubik.cle.prim.null")
(def-fqn character "org.cubik.cle.prim.char")
(def-fqn vector "org.erights.e.elib.tables.constList")
(def-fqn type-desc "org.erights.e.elib.base.typeDesc")
(def-fqn message-desc "org.erights.e.elib.base.messageDesc")
(def-fqn param-desc "org.erights.e.elib.base.paramDesc")
(def-fqn local-throw-sealed-box "org.cubik.cle.prim.localThrowSealedBox")
(def-fqn doc-comment "org.erights.e.elib.base.docComment") ; XXX get approval from MarkM for erights namespace use

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
    (let ((name (simplify-fq-name (cl-type-fq-name type))))
      ;; XXX legacy type namings. todo: switch to never capitalizing; see also __printOn of cl-type-guard
      (if (member name '("any" "all" "float64" "int" "char" "boolean") :test #'string=)
        name
        (convention-capitalize name)))
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
                #+(or) (efuncall e.knot:+sys-trace+ (format nil "~&deriving guard call type ~S~%" (guard-to-type-specifier guard)))
                (sb-c::ir1-transform-specifier-type 
                  (guard-to-type-specifier guard))))))))))

#+sbcl 
(sb-c:defknown e-call-dispatch (t t &rest t) t
  (sb-c:any)
  :derive-type #'sbcl-derive-dispatch-result)

#+(or) ;; this is disabled because it triggers a sbcl optimizer bug (introduced in 0.9.16.42, according to Nikodemus Siivola) now that our CL gen makes the FUNCTIONness of E object expressions visible; see thread from http://article.gmane.org/gmane.lisp.steel-bank.devel/9878 "Compiler hang, sbcl 1.0.8/.9". XXX reenable this when bug is fixed
(sb-c:deftransform e-call-dispatch
    ((target mverb &rest args)
     (function t &rest t))
  "optimize e-call to function call"
  (let ((arg-names (mapcar (lambda (x)
                             (declare (ignore x))
                             (gensym "E-CALL-ARG"))
                           args)))
    `(lambda (target mverb ,@arg-names)
      (funcall target mverb ,@arg-names))))
