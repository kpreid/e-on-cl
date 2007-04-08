; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.util)

;;; --- plain queue implementation ---

; xxx since this is using mutable conses anyway, should we use a mutate-the-tail approach instead of the current mostly-functional queue?
;     is there a queue library available?

(defgeneric enqueue (queue value))
(defgeneric dequeue (queue))
(defgeneric queue-null (queue))

(defclass queue ()
  ((lock :initform (make-lock) :reader %queue-lock)
   (in  :initform () :accessor queue-in)
   (out :initform () :accessor queue-out)))
   
(defmethod enqueue ((queue queue) value)
  (with-lock-held ((%queue-lock queue))
    (push value (queue-in queue)))
  (values))

(defmethod dequeue ((queue queue))
  (with-lock-held ((%queue-lock queue))
    (unless (queue-out queue)
      (setf (queue-out queue) (reverse (queue-in queue))
            (queue-in queue)  '()))
    (pop (queue-out queue))))

(defmethod queue-null ((queue queue))
  (with-lock-held ((%queue-lock queue))
    (not (or (queue-out queue) (queue-in queue)))))

;;; --- sorted queue ---

(defclass sorted-queue ()
  ((elements :type list :initform nil))
  (:documentation "A mutable queue in which entries have numeric keys and are inserted only in their sorted positions in the queue."))
  
(defgeneric sorted-queue-peek (q absent-thunk))
(defgeneric sorted-queue-snapshot (q))
(defgeneric sorted-queue-pop (q))
(defgeneric sorted-queue-put (q key value))
(defgeneric sorted-queue-length (q))

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

(defmethod sorted-queue-length ((q sorted-queue))
  (with-slots (elements) q
    (length elements)))
