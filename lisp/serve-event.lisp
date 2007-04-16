; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; Runner class for Lisp systems providing SERVE-EVENT.

;;; The remainder of this file is within this conditional.
#+(or sbcl cmu) (progn

(defclass serve-event-runner (runner)
  ((time-queue :type priority-queue
               :initform (make-instance 'priority-queue)
               :reader %ser-time-queue)))

(defclass serve-event-deferred-io-handler ()
  ((stopper :initarg :stopper :reader %sedih-stopper)))

(defmethod vr-add-io-handler ((runner serve-event-runner) target direction function)
  ;(format *trace-output* "~&vr-add-io-handler ~S" (list runner target direction function))
  (setf target (ref-shorten target))
  (setf direction (ref-shorten direction))
  (let ((active t)
        (handler nil))
    (labels ((install ()
               ;(format *trace-output* "~&vraih install ~S" (list runner target direction function))
               (setf handler
                 (add-fd-handler target direction (lambda (target2)
                   ;(format *trace-output* "~&vraih enqueueing ~S" (list runner target direction function))
                   (remove-fd-handler handler)
                   (setf handler nil)
                   (enqueue-turn runner (lambda ()
                     ;(format *trace-output* "~&vraih reporting ~S ~S" active (list runner target direction function))
                     (when active
                       (install)
                       (with-simple-restart (abort "Abort delivering IO notification for ~A on ~S to ~S." direction target function)
                         (funcall function target2))))))))))
      (install))
    (make-instance 'serve-event-deferred-io-handler 
      :stopper (lambda () 
                  ;(format *trace-output* "~&vraih stopper ~S" (list runner target direction function))
                  (setf active nil)
                  (when handler
                    (remove-fd-handler handler)
                    (setf handler nil))
                  (values)))))

(defmethod vr-remove-io-handler ((handler serve-event-deferred-io-handler))
  (funcall (%sedih-stopper handler))
  (values))

(defmethod enqueue-timed ((runner serve-event-runner) time func)
  (priority-queue-put (%ser-time-queue runner) time func))

(defun serve-event-with-time-queue (time-queue immediate-queue &optional (timeout nil))
  (destructuring-bind (&optional qtime &rest qfunc)
      (priority-queue-peek time-queue (lambda () nil))
    (when (and qtime (<= qtime (get-fine-universal-time)))
      (priority-queue-pop time-queue)
      (enqueue immediate-queue qfunc)
      (return-from serve-event-with-time-queue t))
    (if qtime
      (let ((delta (min (- qtime (get-fine-universal-time))
                        most-positive-fixnum)))
        (serve-event (if timeout (min timeout delta)
                                 delta)))
      (if timeout
        (serve-event timeout)
        (serve-event)))))

(defmethod runner-loop ((runner serve-event-runner))
  (assert (eql runner *runner*))
  (let ((sends (%runner-queue runner)) 
        (time-queue (%ser-time-queue runner)))
    (loop
      (if (queue-null sends)
        (serve-event-with-time-queue time-queue sends)
        (progn
          (funcall (dequeue sends))
          (serve-event-with-time-queue time-queue sends 0))))))

) ; end #+(or ...)
