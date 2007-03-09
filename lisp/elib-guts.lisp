; This Common Lisp code is Copyright 2005-2007 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; Currently, this file is just defined as 'stuff moved from elib.lisp that doesn't actually need to be loaded before other files, so as to reduce dependencies requiring recompiling'.

; --- Auditing ---

(defobject +the-audit-checker+ "org.erights.e.elib.slot.auditChecker"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| (tw)
    (e-coercef tw +the-text-writer-guard+)
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
  ((value :initarg :value))
  (:documentation "A normal immutable slot."))

(defmethod print-object ((slot e-simple-slot) stream)
  ; xxx make readable under read-eval?
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "& ~W" (slot-value slot 'value))))

(def-vtable e-simple-slot
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +selfless-stamp+))
  (:|__optUncall| (this)
    `#(,+the-make-simple-slot+ "run" #(,(slot-value this 'value))))
  (:|__printOn| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<& ")
    (e. tw |quote| (slot-value this 'value))
    (e. tw |print| ">")
    nil)
  (:|getValue| (this)
    "Returns the constant value of this slot."
    (slot-value this 'value))
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
           :type (function () t))
   (setter :initarg :setter
           :type (function (t) *))))

(def-vtable base-closure-slot
  (:|getValue| (this)
    (funcall (slot-value this 'getter)))
  (:|isFinal| (this)
    (declare (ignore this))
    +e-false+))

(defmethod shared-initialize :after ((slot base-closure-slot) slot-names &key (value nil value-supplied) &allow-other-keys)
  (declare (ignore slot-names))
  (with-slots (getter setter) slot
    (when value-supplied
      (assert (not (slot-boundp slot 'getter)))
      (assert (not (slot-boundp slot 'setter)))
      (setf getter (lambda () value)
            setter (lambda (new) (setf value new))))))
  

(defclass e-var-slot (base-closure-slot) 
  ())

(def-vtable e-var-slot
  (:|__printOn| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<var ")
    (e. tw |quote| (e. this |getValue|))
    (e. tw |print| ">"))
  (:|__optUncall| (this)
    `#(,+the-make-var-slot+ "run" #(,(e. this |getValue|))))
  (:|setValue| (this new-value)
    (funcall (slot-value this 'setter) new-value)
    nil))

(defmethod print-object ((slot e-var-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W" (e. slot |getValue|))))

(defclass e-guarded-slot (base-closure-slot) 
  ((guard :initarg :guard)))

(defmethod shared-initialize :around ((slot e-guarded-slot) slot-names &rest initargs &key (value nil value-supplied) guard opt-ejector &allow-other-keys)
  "if this slot is initialized with a value, it is coerced; if initialized with a getter and setter, it is assumed to be correct"
  (declare (ignore slot-names))
  (if value-supplied
    (apply #'call-next-method slot slot-names
      :value (e. guard |coerce| value opt-ejector)
      initargs)
    (call-next-method)))

(def-vtable e-guarded-slot
  (:|__printOn| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (with-slots (guard getter) this
      (e. tw |print| "<var ")
      (e. tw |quote| (funcall getter))
      (e. tw |print| " :")
      (e. tw |quote| guard)
      (e. tw |print| ">")))
  (:|setValue| (this new-value)
    (with-slots (guard setter) this
      (funcall setter (e. guard |coerce| new-value nil)))
    nil))

(defmethod print-object ((slot e-guarded-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W :~W" 
      (if (slot-boundp slot 'getter) 
        (e. slot |getValue|)
        '#:<unbound-getter>)
      (ignore-errors (slot-value slot 'guard)))))

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
        (eql auditor +standard-graph-exit-stamp+)))

  (:|__printOn| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (if (e-is-true this)
                     "true"
                     "false")))
  (:|and| (this other)
    "Boolean and."
    (e-coercef other 'e-boolean)
    (if (e-is-true this) other +e-false+))
  (:|not| (this)
    "Boolean negation."
    (if (eql this +e-false+) +e-true+ +e-false+))
  (:|or| (this other)
    "Boolean or."
    (e-coercef other 'e-boolean)
    (if (e-is-true this) +e-true+ other))
  (:|pick| (this true-value false-value)
    "Return the first argument if this is true, otherwise return the second argument."
    (if (e-is-true this) true-value false-value))
  (:|xor| (this other)
    "Boolean exclusive or."
    (e-coercef other 'e-boolean)
    (if (e-is-true this) (e. other |not|) other)))

; --- Throw and Ejector ---

(defobject +the-thrower+ "org.erights.e.elib.prim.throw"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| (tw)
    (e-coercef tw +the-text-writer-guard+)
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
    (:|__printOn| (tw)
      (e-coercef tw +the-text-writer-guard+)
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
  
(defun cl-type-fq-expr (type)
  (concatenate 'string 
    (cl-type-fq-name type)
    (cl-type-parameters type nil)))
  
; --- far refs/proxies ---

(defclass handler-ref (ref)
  ((handler        :initarg :handler
                   :reader %proxy-ref-handler)
   (resolution-box :initarg :resolution-box
                   :reader %proxy-ref-resolution-box))
  (:documentation "A reference which has a handler and resolution promise, but doesn't necessarily use them for anything."))

(defclass handler-identity-ref (handler-ref) 
  ()
  (:documentation "A reference which gets its identity from a handler and resolution promise. Instances will be either PROXY-REF or DISCONNECTED-REF."))

(def-atomic-sameness handler-identity-ref
  (lambda (a b) (and (eql (class-of a)
                          (class-of b))
                     (samep (%proxy-ref-handler a)
                            (%proxy-ref-handler b))
                     (same-yet-p (%proxy-ref-resolution-box a)
                                 (%proxy-ref-resolution-box b))))
  (lambda (a) (same-hash (%proxy-ref-handler a))))

(defclass disconnected-ref (broken-ref handler-identity-ref)
  ())

(defclass proxy-ref (handler-ref)
  ()
  (:documentation "A Ref backed by a ProxyHandler."))

(defun bogus-resolution (&rest spec)
  (let ((condition (apply #'to-condition 'simple-error spec)))
    (e. e.knot:+sys-trace+ |run| condition)
    (make-unconnected-ref (transform-condition-for-e-catch condition))))

(defun verify-resolution-box (ref box)
  (handler-case 
    (progn
      (setf box (ref-shorten box))
      (check-type box e-simple-slot)
      (e. box |getValue|))
    (error ()
      (bogus-resolution "Resolution promise of a Far ref handled by ~A didn't resolve to a simple slot, but ~A."
        (e-quote (%proxy-ref-handler ref))
        (e-quote box)))))

(defun verify-broken-resolution (ref resolution-ref)
  (if (eql (ref-state resolution-ref) 'broken)
    resolution-ref
    (bogus-resolution "Attempt to resolve this Far ref handled by ~A to another identity (~A)." 
      (e-quote (%proxy-ref-handler ref)) 
      (e-quote resolution-ref))))

(defgeneric %resolve-proxy (proxy resolution))

(defun %maybe-resolve-proxy (ref)
  "Convert this proxy-ref into its resolution, if it has resolved further. Returns whether it did anything."
  (with-ref-transition-invariants (ref)
    (check-type ref proxy-ref)
    (let ((resolution-box (%proxy-ref-resolution-box ref)))
      (when (ref-is-resolved resolution-box)
        ;; If the resolution-box is resolved, it contains the resolution of this reference.
        (%resolve-proxy ref (verify-resolution-box ref resolution-box))
        ;; Inform the caller of the need to re-dispatch.
        t))))

(defmethod %ref-shorten ((ref proxy-ref))
  (if (%maybe-resolve-proxy ref)
    (ref-shorten ref)
    ref))

(defmethod ref-state :around ((ref proxy-ref))
  (if (%maybe-resolve-proxy ref)
    (ref-state ref)
    (call-next-method)))

(defmethod ref-state ((ref proxy-ref))
  (error "ref-state not overridden for a proxy-ref"))

(defmethod e-call-dispatch ((ref proxy-ref) mverb &rest args)
  (if (%maybe-resolve-proxy ref)
    (apply #'e-call-dispatch ref mverb args)
    (error 'synchronous-call-error :recipient ref :mverb mverb :args args)))

(defmethod e-send-dispatch ((ref proxy-ref) mverb &rest args)
  (with-slots (handler) ref
    (e<- handler |handleSendAll| (e-util:unmangle-verb mverb) (coerce args 'vector))))

(defmethod e-send-only-dispatch ((ref proxy-ref) mverb &rest args)
  (with-slots (handler) ref
    (e-send-only-dispatch handler :|handleSendAllOnly/2| (e-util:unmangle-verb mverb) (coerce args 'vector)))
  nil)

(defmethod weak-when-more-resolved ((ref proxy-ref) weak-reactor action)
  (declare (ignore weak-reactor action))
  ;; XXX implement this to invoke the handler. until then, multivat 
  ;; tail-recursive loops may leak forwarding-refs.
  (values))


(defclass far-ref (proxy-ref handler-identity-ref)
  ())

(defmethod %resolve-proxy ((proxy far-ref) resolution)
  "When resolving a far-ref, its identity must be preserved, and it can only become disconnected (broken); attempting to do otherwise is an error."
  (change-class proxy 'disconnected-ref
    :problem (ref-opt-problem (verify-broken-resolution proxy resolution))))

(defmethod ref-state ((ref far-ref))
  (values 'eventual t))

;; XXX remote promises *should* be handler-identity-refs, but the equalizer can't handle that yet. Not doing so merely results in remote promises not being same when they should be.
(defclass remote-promise (proxy-ref)
  ())

(defmethod ref-state ((ref remote-promise))
  (values 'eventual nil))

(defmethod %resolve-proxy ((proxy remote-promise) resolution)
  "If we're a remote promise, then we can just become a forwarder."
  (change-class proxy 'forwarding-ref :target resolution))


(defobject +the-make-proxy+ "org.erights.e.elib.ref.makeProxy"
    (:stamped +deep-frozen-stamp+)
  (:|run| (handler resolution-box resolved)
    (unless (settledp handler)
      (error 'not-settled-error :name "proxy handler" :value handler))
    (make-instance (if (e-is-true resolved) 'far-ref 'remote-promise)
        :handler handler
        :resolution-box resolution-box)))

; --- sorted queue ---

(defmethod sorted-queue-peek ((q sorted-queue) absent-thunk)
  (with-slots (elements) q
    (if elements
      (first elements)
      (funcall absent-thunk))))

(defmethod sorted-queue-pop ((q sorted-queue))
  (with-slots (elements) q
    (if elements
      (pop elements)
      (error "empty queue"))))
      
(defmethod sorted-queue-snapshot ((q sorted-queue))
  (with-slots (elements) q
    (copy-list elements)))
    
(defmethod sorted-queue-put ((q sorted-queue) key value)
  #-sbcl (declare (real key)) ; apparent PCL bug triggered
  (with-slots (elements) q
    ;XXX more efficient than linear?
    (if (or (null elements) (< key (car (first elements))))
      (push (cons key value) elements)
      (loop for prev = elements then (rest prev)
            while prev
            do (when (or (null (rest prev))
                         (< key (car (second prev))))
                 (push (cons key value) (rest prev))
                 (return))
            finally (error "fell off end of queue")))
    nil))

(defobject +the-make-sorted-queue+ "org.cubik.cle.prim.makeSortedQueue" ()
  (:|run| ()
    (make-instance 'sorted-queue)))

(def-vtable sorted-queue
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<sorted queue of " (length (slot-value this 'elements)) ">"))
  (:|peek| (this absent-thunk)
    (block nil
      (let ((p (sorted-queue-peek this (lambda () (return (efuncall absent-thunk))))))
        (vector (car p) (cdr p)))))
  (:|pop| (this)
    (let ((p (sorted-queue-pop this)))
      (vector (car p) (cdr p))))
  (:|put| (this key value)
    (e-coercef key 'real)
    (sorted-queue-put this key value))
  (:|asList| (this)
    (map 'vector 
         #'(lambda (c) (vector (car c) (cdr c)))
         (sorted-queue-snapshot this))))

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
