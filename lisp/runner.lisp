; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; --- Runner protocol ---

(defgeneric enqueue-turn (context function)
  (:documentation "Must be safe for being called from the wrong thread."))

;;; XXX what does vr- stand for?
(defgeneric vr-add-io-handler (context target direction function))

(defgeneric vr-remove-io-handler (handler))

(defgeneric enqueue-timed (context time func))

(defgeneric runner-loop (runner))

(defgeneric make-runner-for-this-thread (&key label))

(defvar *runner* nil)

(defun top-loop ()
  (runner-loop *runner*))

;;; --- Basic runner ---

(defclass runner ()
  ((label :initarg :label
          :initform nil
          :type (or null string)
          :reader label)
   (sends :type queue
          :reader %runner-queue
          :initform (make-instance 'queue))))

(defmethod enqueue-turn ((runner runner) function)
  ;; relies on the send queue being thread-safe if applicable
  (enqueue (%runner-queue runner) function)
  (values))

(defmethod runner-loop ((runner runner))
  (assert (eql runner *runner*))
  (let ((queue (%runner-queue runner)))
    (loop
      (if (queue-null queue)
        (break "Ran out of things to do in ~S." runner)
        (funcall (dequeue queue))))))
