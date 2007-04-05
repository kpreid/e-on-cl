; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;; This file currently just contains the serve-event-runner, not all of the serve-event related code.

;; XXX to finish current plans, move e.util serve-event extension code into here

(defclass serve-event-runner (runner)
  ((handler-group :type io-handler-exclusion-group
                  :initform (make-instance 'io-handler-exclusion-group)
                  :reader handler-exclusion-group)
   (time-queue :type sorted-queue
               :initform (make-instance 'sorted-queue))))

(defmethod vr-add-io-handler ((runner serve-event-runner) target direction function)
  (add-exclusive-io-handler (handler-exclusion-group runner)
                            (ref-shorten target)
                            (ref-shorten direction)
                            function))

(defmethod enqueue-timed ((runner serve-event-runner) time func)
  (with-slots (time-queue) runner
    (sorted-queue-put time-queue time func)))

(defun serve-event-with-time-queue (time-queue immediate-queue &optional (timeout nil))
  (destructuring-bind (&optional qtime &rest qfunc)
      (sorted-queue-peek time-queue (lambda () nil))
    (when (and qtime (<= qtime (get-fine-universal-time)))
      (sorted-queue-pop time-queue)
      (enqueue immediate-queue qfunc)
      (return-from serve-event-with-time-queue t))
    (if qtime
      (let ((delta (min (- qtime (get-fine-universal-time))
                        most-positive-fixnum)))
        (e-util:serve-event (if timeout (min timeout delta)
                                        delta)))
      (if timeout
        (e-util:serve-event timeout)
        (e-util:serve-event)))))

(defmethod runner-loop ((runner serve-event-runner))
  (assert (eql runner *runner*))
  (with-slots (sends time-queue) runner
    (loop
      (if (queue-null sends)
        (serve-event-with-time-queue time-queue sends)
        (progn
          (call-with-io-handler-exclusion
            (dequeue sends)
            (handler-exclusion-group runner)
            `(send-queue-in ,runner))
          (serve-event-with-time-queue time-queue sends 0))))))

(defmethod make-runner-for-this-thread ()
  ;; XXX todo: remove compatibility layers in e.util and make this implementation-dependent
  (make-instance 'serve-event-runner))
