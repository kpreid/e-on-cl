; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib) ;; XXX maybe split package?

(defun deep-sharable-p (specimen)
  (setf specimen (ref-shorten specimen))
  (cond
    ((typep specimen '(and vector (not string)))
      (every #'deep-sharable-p specimen))
    ((approvedp +thread-sharable-stamp+ specimen)
      (let ((uncall (e. specimen |__optUncall|)))
        (if uncall
          (deep-sharable-p uncall)
          t)))
    (t
      nil)))

(defclass boot-ref ()
  ((vat :initarg :vat :reader vat :initform *vat*)
   (ref :initarg :ref :reader unwrap-boot-ref)
   (handler :initarg :handler :reader %boot-ref-handler)))

(defmethod print-object ((r boot-ref) stream)
  (print-unreadable-object (r stream :type t :identity nil)
    (ignore-errors
      (format stream "~S via ~S in ~S" (slot-value r 'ref) (slot-value r 'handler) (vat r)))))

(defun make-boot-ref (ref handler &optional (vat *vat*))
  (assert vat)
  (make-instance 'boot-ref :ref ref :vat vat :handler handler))

(defmethod unwrap-boot-ref :before ((r boot-ref))
  (assert (eql *vat* (vat r))))

(def-vtable boot-ref
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (or (eql auditor +thread-sharable-stamp+)))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |write| "<boot-ref of ")
    (e. tw |print| (vat this))
    (e. tw |write| ">")
    nil)
  (:|unwrap| (this fail)
    "If this boot-ref contains a reference which belongs to the vat in which this method is invoked, returns that reference; otherwise returns fail()."
    (if (samep *vat* (vat this))
      (unwrap-boot-ref this)
      (efuncall fail)))
  (:|run| (this message)
    (assert (deep-sharable-p message))
    ;; relies on enqueue-turn being safe to call from the wrong thread
    (enqueue-turn (vat this)
      (lambda () (e<- (%boot-ref-handler this) 
                      |receive|
                      (unwrap-boot-ref this) 
                      message)))))

(defun %enqueue-with-unsafe-response (vat function)
  "Eventually execute FUNCTION in VAT, returning a promise belonging to that vat for the result."
  ;; XXX this is a copy and modification of the default e-send-dispatch method
  (multiple-value-bind (promise resolver)
      ;; relies on make-promise being safe to call from the wrong thread
      (let ((*vat* vat)) (make-promise))
    ;; relies on enqueue-turn being safe to call from the wrong thread
    (enqueue-turn vat (lambda ()
      (e. resolver |resolve| 
        (handler-case-with-backtrace
          ;; XXX there is currently some bug being muffled at this point
          ;(handler-bind ((error #'invoke-debugger)) (funcall function))
          (funcall function)
          (error (p b)
            (efuncall e.knot:+sys-trace+ (format nil "in %enqueue-with-response thunk: ~A" p))
            (make-unconnected-ref (transform-condition-for-e-catch p :backtrace b)))))))
    promise))

(defun make-comm-handler-promise (vat)
  (%enqueue-with-unsafe-response vat
    (lambda ()
      (efuncall (e-import "org.cubik.cle.makeSharedRefLink")
        (type-specifier-to-guard '(satisfies deep-sharable-p))
        +pass-by-construction+
        (efun (ref)
          (make-boot-ref ref (vat-comm-handler vat)))))))

(defun make-vat-controller (vat)
  (e-lambda "org.cubik.cle.prim.vatController" ()
    (:|seedEval| (program)
      ;; XXX should be in vat-privileged scope?
      (assert (eq *vat* vat))
      (e. program |eval| (vat-safe-scope vat)))
    (:|shutdown| ((problem 'condition))
      (declare (ignore problem))
      (assert (eq *vat* vat))
      (error "unimplemented"))))

(defobject +the-make-vat+ "org.cubik.cle.prim.makeVat" ()
  (:|run| ((opt-runner '(or null runner)) (name 'string))
    "Passing null for opt-runner means to use the current runner."
    (let* ((new-vat (make-instance 'vat 
                      :label (format nil "user vat ~A" name)
                      :runner (or opt-runner *runner*)))
           (controller (make-vat-controller new-vat))
           (boot-controller (make-boot-ref controller
                          (%enqueue-with-unsafe-response new-vat
                            (lambda () (vat-comm-handler new-vat)))
                          new-vat)))
      (e. (vat-comm-handler *vat*) |proxy| boot-controller +e-false+))))
