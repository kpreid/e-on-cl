; This Common Lisp code is Copyright 2005-2006 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; Currently, this file is just defined as 'stuff moved from elib.lisp that doesn't actually need to be loaded before other files, so as to reduce dependencies requiring recompiling'.

; --- native-type guards ---

(defglobal +trivial-value-lists+
  (mapcar #'list
          (list nil 
                +e-false+ 
                0 0d0 0.0 
                (code-char 0))))

(def-vtable cl-type-guard
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    ;; assuming that super behaves DeepFrozen
    (eql auditor +deep-frozen-stamp+))

  (:|coerce| (guard specimen opt-ejector)
    (e-coerce-native specimen (cl-type-specifier guard) opt-ejector guard))
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (cl-type-simple-expr (cl-type-specifier this))))
  (:|getFQName| (this)
    (cl-type-fq-name (cl-type-specifier this)))
  (:|getTheTrivialValue| (this)
    (with-slots (ts trivial-box) this
      (first (or trivial-box
                 (setf trivial-box
                   (find-if (lambda (v) (typep (first v) ts))
                            +trivial-value-lists+))
                 ;; xxx should there be an ejector?
                 (error "No trivial value available")))))
  (:|getDocComment| (this)
    (with-slots (ts) this
      (let ((documentation (documentation ts 'type)))
        ; The CONS case is a workaround for an apparent bug in OpenMCL 0.14.2-p1.
        ; If not for that, this would be (or documentation "").
        (typecase documentation
          (string documentation)
          (cons   (first documentation))
          (null   "")))))
  (:|getSupers| (this) 
    "Supertype information is not currently available for primitive types."
    (declare (ignore this))
    #())
  (:|getAuditors| (this)
    (declare (ignore this))
    #())
  (:|getMessageTypes| (this)
    ; xxx this is a bit inefficient
    (with-slots (ts) this
      (message-pairs-to-map-including-miranda-messages (vtable-message-types ts)))))

(def-atomic-sameness cl-type-guard
  (lambda (a b) (equal (cl-type-specifier a)
                       (cl-type-specifier b)))
  (lambda (a)   (sxhash (cl-type-specifier a))))

(defmethod e-call-match ((rec cl-type-guard) mverb &rest args)
  (with-slots (ts super) rec
    ; XXX we could make super initialized more lazily
    ; XXX super needs to be per-thread
    (unless super
      (setf super (e. (e. (vat-safe-scope *vat*) |get| "__makeGuard") |run| rec)))
    (if (eql ts 't) ; XXX bad OO
      (cond
        ((and (not (eql mverb :|get/0|))
              (eql mverb (e-util:mangle-verb "get" (length args))))
          (e. (e. super |_getMakeUnionGuard|) |run| (coerce args 'vector)))
        ((and (eql mverb :|__respondsTo/2|) 
              (string= (aref args 0) "get"))
          +e-true+)
        (t
          (apply #'e-call-dispatch super mverb args)))
      (apply #'e-call-dispatch super mverb args))))

(defmethod make-load-form ((object cl-type-guard) &optional environment)
  (declare (ignore environment))
  ;; must be custom in order to ignore allow super and trivial-box being restored as nil
  `(locally (declare (notinline make-instance))
     (make-instance ',(class-name (class-of object))
                    :type-specifier ',(cl-type-specifier object))))

; --- Auditing ---

(defglobal +the-audit-checker+ (e-lambda "org.erights.e.elib.slot.auditChecker"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "__auditedBy")) ; XXX move to e.syntax?
  (:|run| (auditor specimen)
    (as-e-boolean (approvedp auditor specimen)))))

;;; --- Basic slots ---


;;; XXX review the actual uses of these various slot implementations; also consider moving them to e-lambdas
;;; XXX look for ways to reduce the duplication of these common makers

(defun import-uncall (fqn)
  `#(,(e. (vat-safe-scope *vat*) |get| "import__uriGetter") "get" #(,fqn)))

(defglobal +the-make-simple-slot+ (e-lambda "org.erights.e.elib.slot.makeFinalSlot"
    (:stamped +deep-frozen-stamp+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeFinalSlot"))
  (:|asType| () (type-specifier-to-guard 'e-simple-slot))
  (:|run| (value)
    (make-instance 'e-simple-slot :value value))))

(defglobal +the-make-var-slot+ (e-lambda "org.erights.e.elib.slot.makeVarSlot"
    (:stamped +deep-frozen-stamp+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeVarSlot"))
  (:|asType| () (type-specifier-to-guard 'e-var-slot))
  (:|run| (value)
    (make-instance 'e-var-slot :value value))))

(defglobal +the-make-guarded-slot+ (e-lambda "org.erights.e.elib.slot.makeGuardedSlot"
    (:stamped +deep-frozen-stamp+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeGuardedSlot"))
  (:|asType| () (type-specifier-to-guard 'e-guarded-slot))
  (:|run| (guard value opt-ejector)
    (make-instance 'e-guarded-slot :value value
                                   :guard guard
                                   :opt-ejector opt-ejector))))
    
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
    (eql auditor +deep-frozen-stamp+))

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

; --- Ejector ---

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
  (e-lambda "$ejector" ()
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
      (string-downcase (package-name (symbol-package type-sym)))
      (string-downcase (symbol-name type-sym)))))

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

(def-fqn flex-list "org.erights.e.elib.tables.FlexList")
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

(defclass proxy-ref (ref)
  ((handler :initarg :handler))
  (:documentation "A Ref backed by a ProxyHandler."))

(defmethod %ref-shorten ((ref proxy-ref))
  ref)

(defmethod ref-state ((ref proxy-ref))
  (error "ref-state not overridden for a proxy-ref"))

(defmethod e-call-dispatch ((ref proxy-ref) mverb &rest args)
  (error 'synchronous-call-error :recipient ref :mverb mverb :args args))

(defmethod e-send-dispatch ((ref proxy-ref) mverb &rest args)
  (with-slots (handler) ref
    (e<- handler |handleSendAll| (e-util:unmangle-verb mverb) (coerce args 'vector))))


(defclass far-ref (proxy-ref identified-ref)
  ())

(defmethod ref-state ((ref far-ref))
  (values 'eventual t))


(defclass remote-promise (proxy-ref) 
  ())

(defmethod ref-state ((ref remote-promise))
  (values 'eventual nil))


(defglobal +the-make-proxy-resolver+ (e-lambda "org.erights.e.elib.ref.makeProxyResolver" 
    (:stamped +deep-frozen-stamp+)
  (:|run| (opt-handler opt-identity &aux ref-slot)
    ; XXX ref-slot will eventually be a weak reference
    (unless opt-handler
      (error "null is not allowed as the handler"))
    (unless (settledp opt-identity)
      (error 'not-settled-error :name "optIdentity" :value opt-identity))
    (e-lambda "$proxyResolver" ()
      (:|__printOn| (tw)
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |print| "<Proxy Resolver>"))
      (:|getProxy| ()
        "Return the Ref for this resolver, creating a new one if necessary."
        (unless (and ref-slot (e. ref-slot |getValue|))
          (setf ref-slot (make-instance 'e-simple-slot :value
            (if opt-handler
              (if opt-identity
                (make-instance 'far-ref
                    :handler opt-handler
                    :identity opt-identity)
                (make-instance 'remote-promise
                    :handler opt-handler))
              (error "this ProxyResolver is resolved and therefore its proxy is not available")))))
        (e. ref-slot |getValue|))
      (:|smash| (problem)
        (unless opt-handler
          (error "already resolved"))
        (let ((ref (and ref-slot (e. ref-slot |getValue|))))
          (when ref
            (with-ref-transition-invariants (ref)
              (change-class ref
                (if opt-identity
                  'disconnected-ref
                  'unconnected-ref)
                :problem problem))))
        (let ((handler opt-handler))
          (setf opt-handler nil
                ref-slot nil)
          ; XXX should we catch any problems arising here?
          (e. handler |handleResolution| (make-unconnected-ref problem)))
        nil)))))

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

(defglobal +the-make-sorted-queue+ (e-lambda "org.cubik.cle.prim.makeSortedQueue" ()
  (:|run| ()
    (make-instance 'sorted-queue))))

(def-vtable sorted-queue
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<sorted queue of " (length (slot-value this 'elements)) ">"))
  (:|peek| (this absent-thunk)
    (block nil
      (let ((p (sorted-queue-peek this (lambda () (return (e. absent-thunk |run|))))))
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
                #+(or) (e. e.knot:+sys-trace+ |run| (format nil "~&deriving guard call type ~S~%" (cl-type-specifier guard)))
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
