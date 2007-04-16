; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.util)

;;; --- plain queue implementation ---

; xxx since this is using mutable conses anyway, should we use a mutate-the-tail approach instead of the current mostly-functional queue?
;     is there a queue library available?

(defgeneric enqueue (queue value))
(defgeneric dequeue (queue))
(defgeneric dequeue-blocking (queue))
(defgeneric queue-null (queue))

(defclass queue ()
  ((lock :initform (make-recursive-lock) :reader %queue-lock)
   (nonempty-condition :initform (make-condition-variable)
                       :reader %queue-nonempty-condition)
   (in  :initform () :accessor queue-in)
   (out :initform () :accessor queue-out)))
   
(defmethod enqueue ((queue queue) value)
  (with-lock-held ((%queue-lock queue))
    (push value (queue-in queue)))
  (condition-notify (%queue-nonempty-condition queue))
  (values))

(defmethod dequeue ((queue queue))
  (with-lock-held ((%queue-lock queue))
    (unless (queue-out queue)
      (setf (queue-out queue) (reverse (queue-in queue))
            (queue-in queue)  '()))
    (pop (queue-out queue))))

(defmethod dequeue-blocking ((queue queue))
  (with-lock-held ((%queue-lock queue))
    (or (dequeue queue)
        (progn
          (condition-wait (%queue-nonempty-condition queue) 
                          (%queue-lock queue))
          (dequeue queue)))))

(defmethod queue-null ((queue queue))
  (with-lock-held ((%queue-lock queue))
    (not (or (queue-out queue) (queue-in queue)))))

;;; --- priority queue ---

(defclass priority-queue ()
  ((elements :type list 
             :initform nil
             :accessor %priority-queue-elements))
  (:documentation "A mutable priority queue with numeric priorities."))
  
(defgeneric priority-queue-peek (q absent-thunk))
(defgeneric priority-queue-snapshot (q))
(defgeneric priority-queue-pop (q))
(defgeneric priority-queue-put (q key value))
(defgeneric priority-queue-length (q))

(defmethod priority-queue-peek ((q priority-queue) absent-thunk)
  (let ((elements (%priority-queue-elements q)))
    (if elements
      (first elements)
      (funcall absent-thunk))))

(defmethod priority-queue-pop ((q priority-queue))
  (if (%priority-queue-elements q)
    (pop (%priority-queue-elements q))
    (error "empty queue")))
      
(defmethod priority-queue-snapshot ((q priority-queue))
  (copy-list (%priority-queue-elements q)))
    
(defmethod priority-queue-put ((q priority-queue) key value)
  #-sbcl (declare (real key)) ; apparent PCL bug triggered
  ;XXX more efficient than linear?
  (if (or (null (%priority-queue-elements q)) 
          (< key (car (first (%priority-queue-elements q)))))
    (push (cons key value) (%priority-queue-elements q))
    (loop for prev = (%priority-queue-elements q) then (rest prev)
          while prev
          do (when (or (null (rest prev))
                       (< key (car (second prev))))
               (push (cons key value) (rest prev))
               (return))
          finally (error "fell off end of queue")))
  nil)

(defmethod priority-queue-length ((q priority-queue))
  (length (%priority-queue-elements q)))
