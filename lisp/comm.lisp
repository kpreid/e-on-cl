; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
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
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |write| "<boot-ref of ")
    (e. tw |print| (vat this))
    (e. tw |write| ">")
    nil)
  (:|asProxyIdentity/0| 'identity)
  (:|run| (this message)
    (assert (deep-sharable-p message))
    (%send-in-vat (vat this)
      (efun () (e<- (%boot-ref-handler this) 
                    |receive|
                    (unwrap-boot-ref this) 
                    message)) 
      "run" #())))

(defun %send-in-vat (vat recipient verb args)
  "unsafe: yields a promise belonging to the other vat"
  (let ((*vat* vat))
    ;; XXX threading: will need locks
    (e-send recipient verb args)))

(defun make-comm-handler-promise (vat)
  (%send-in-vat vat
    (efun ()
      (efuncall (e-import "org.cubik.cle.makeSharedRefLink")
        +the-make-proxy-resolver+
        (type-specifier-to-guard '(satisfies deep-sharable-p))
        +pass-by-construction+
        (efun (ref)
          (make-boot-ref ref (vat-comm-handler vat)))))
    "run" #()))

(defun make-vat-controller (vat)
  (e-lambda "org.cubik.cle.prim.vatController" ()
    (:|seedEval| (program)
      ;; XXX should be in vat-privileged scope
      (assert (eq *vat* vat))
      (e. program |eval| (vat-safe-scope vat)))
    (:|shutdown| (problem)
      (assert (eq *vat* vat))
      (e-coercef problem 'condition)
      (error "unimplemented"))))

(defglobal +the-make-vat+ (e-lambda "org.cubik.cle.prim.makeVat" ()
  (:|run| (opt-runner name)
    "Passing null for opt-runner means to use the current runner."
    (e-coercef opt-runner '(or null runner))
    (e-coercef name 'string)
    (let* ((new-vat (make-instance 'vat 
                      :label (format nil "user vat ~A" name)
                      :runner (or opt-runner *runner*)))
           (controller (make-vat-controller new-vat))
           (boot-controller (make-boot-ref controller
                          (%send-in-vat new-vat
                            (efun () (vat-comm-handler new-vat))
                            "run" #())
                          new-vat)))
      (e. (vat-comm-handler *vat*) |proxy| boot-controller +e-false+)))))
