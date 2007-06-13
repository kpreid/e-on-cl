; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; *VAT* is declared in ref.lisp.

(defun call-with-turn (body vat &key (label t))
  "Execute the 'body' thunk, as a turn in the given vat."
  (assert label)
  (when (vat-in-turn vat)
    (error "~S is already executing a turn, ~S, so turn ~S may not execute" vat (vat-in-turn vat) label))
  (unless (or (null *vat*)
              (eq *vat* vat))
    (error "there is already a current vat, ~S, so ~S may not execute a turn" *vat* vat))
  (unwind-protect
    (let ((*vat* vat))
      (setf (vat-in-turn vat) label)
      (funcall body))
    (setf (vat-in-turn vat) nil)))

(defmacro with-turn ((&rest args) &body body)
  "Execute the body, as a turn in the given vat."
  `(call-with-turn (lambda () ,@body) ,@args))

(defclass vat ()
  ((runner :initarg :runner
           :initform (error "no runner specified in vat creation")
           :accessor vat-runner
           :type runner)
   (in-turn :initform nil
            :accessor vat-in-turn
            :type t
            :documentation "Whether some type of top-level turn is currently being executed in this vat; if not NIL, is a label for the turn. Used for consistency checking.")
   (safe-scope :initform (e.knot:make-safe-scope)
               :accessor vat-safe-scope)
   (label :initarg :label
          :initform nil
          :type (or null string)
          :reader label)
   (sugar-cache :initform (make-hash-table)
                :type hash-table
                :reader sugar-cache
                :documentation "Tentative: For looking up objects which are sugar-parents for objects shared between vats.")
   (e.rune::incorporated-files :initform nil
                               :type list
                               :accessor e.rune::incorporated-files)
   (vat-comm-handler :accessor vat-comm-handler)))

(defmethod initialize-instance :after ((vat vat) &rest initargs)
  (declare (ignore initargs))
  (setf (vat-comm-handler vat)
    (make-comm-handler-promise vat)))

(defmethod print-object ((vat vat) stream)
  (print-unreadable-object (vat stream :type t :identity t)
    (format stream "~S~:[~; (in turn)~]"
      (label vat)
      (vat-in-turn vat))))

(defmethod enqueue-turn ((vat vat) function)
  (enqueue-turn (vat-runner vat)
                (lambda ()
                  (call-with-turn function vat :label "basic turn")))
  (values))

(defmethod vr-add-io-handler ((vat vat) target direction function)
  (vr-add-io-handler (vat-runner vat)
                     target
                     direction
                     (lambda (target*)
                       (assert (eql target target*) () "buh?")
                       (with-turn (vat :label (format nil "IO handler ~A for ~A"
                                                      function
                                                      target)) 
                         (funcall (ref-shorten function) target*)))))

(defmethod e-send-dispatch (rec mverb &rest args)
;; NOTE: edit this in parallel with e-send-only-dispatch above
  (assert (eq (ref-state rec) 'near) () "inconsistency: e-send-dispatch default case was called with a non-NEAR receiver")
  (multiple-value-bind (promise resolver) (make-promise)
    (enqueue-turn *vat* (lambda ()
      ; XXX direct this into a *configurable* tracing system once we have one
      ;(efuncall e.knot:+sys-trace+ (format nil "running ~A ~A <- ~A ~A" (e. (e. rec |__getAllegedType|) |getFQName|) (e-quote rec) (symbol-name mverb) (e-quote (coerce args 'vector))))
      (e. resolver |resolve| 
        (handler-case-with-backtrace
          (apply #'e-call-dispatch rec mverb args)
          (error (problem backtrace)
            ;; XXX using e printing routines here is invoking objects outside a proper turn; do one of:
            ;;   (a) CL print instead
            ;;   (b) make printing the error done in yet another turn (possible object's-print-representation-has-changed problem)
            (efuncall e.knot:+sys-trace+ (format nil "problem in send ~A <- ~A ~A: ~A" (e-quote rec) (symbol-name mverb) (e-quote (coerce args 'vector)) problem))
            (make-unconnected-ref (transform-condition-for-e-catch problem :backtrace backtrace)))))))
    promise))

(defmethod e-send-only-dispatch (rec mverb &rest args)
  ;; NOTE: edit this in parallel with e-send-dispatch above
  (assert (eq (ref-state rec) 'near) () "inconsistency: e-send-only-dispatch default case was called with a non-NEAR receiver")
  (enqueue-turn *vat* (lambda ()
    (handler-case
      (apply #'e-call-dispatch rec mverb args)
      (error (p)
        (efuncall e.knot:+sys-trace+ (format nil "problem in send ~A <- ~A ~A: ~A" (e-quote rec) (symbol-name mverb) (e-quote (coerce args 'vector)) p))))))
  (values))

(defmethod enqueue-timed ((vat vat) time func)
  (enqueue-timed (vat-runner vat) time
                 (lambda ()
                   ;; XXX future improvement: human-formatted time
                   (call-with-turn func vat :label (format nil "~A at time ~A" func time)))))

(defun establish-vat (&rest initargs &key label &allow-other-keys)
  (assert (null *vat*))
  (assert (null *runner*))
  (setf *runner* (make-runner-for-this-thread :label label))
  (setf *vat* (apply #'make-instance 'vat :runner *runner* initargs)))

(declaim (inline sugar-cache-get))
(defun sugar-cache-get (eq-key fqn)
  (let ((cache (sugar-cache *vat*)))
    (gethash eq-key cache
      (setf (gethash eq-key cache)
        (e-import fqn)))))

(declaim (inline sugar-cache-call))
(defun sugar-cache-call (rec mverb key fqn &rest args)
  (apply #'e-call-dispatch
    (sugar-cache-get key fqn)
    (multiple-value-bind (verb arity) (unmangle-verb mverb)
      (mangle-verb verb (1+ arity)))
    rec
    args))

