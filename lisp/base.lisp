; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.elib)

(defvar *compatible-catch-leakage* nil
  "If true, allows thrown exceptions to be caught unsealed, for compatibility with the Java implementation.")

(defvar *break-on-ejections* nil
  "Analogous to CL:*BREAK-ON-SIGNALS*, but applies to ejection. When an ejector is invoked with a value whose type is a subtype of the type specified by this variable's value, (break) will be called.")


(defun unexternalizable-optimization-p (environment)
  "Whether it is OK for macros (or compiler macros, sb-c:deftransforms, etc) to insert unexternalizable objects such as functions in their expansions in the given environment."
  (macroexpand '(%allow-unexternalizable-optimization) environment))

(defmacro %allow-unexternalizable-optimization ()
  nil)
  
(defmacro with-unexternalizable-optimization (&body body)
  `(macrolet ((%allow-unexternalizable-optimization () t))
     ,@body))


;; xxx Assuming things about the implementation. Perhaps we should produce a warning if we don't know that the implementation uses these sizes.
(deftype float64 () 'double-float) 
(deftype float32 () 'single-float)


(eval-when (:compile-toplevel :load-toplevel :execute) 

  ; XXX these are the double-float versions - make this explicit
  (defconstant |NaN| 
    (or #+(and sbcl ppc)
          (with-appropriate-floating-point-rules
            (- sb-ext:double-float-positive-infinity sb-ext:double-float-positive-infinity))
        '|NaN|)
    "The double-float Not-a-Number value if available, or a symbol.")
  (defconstant |Infinity|
    #+sbcl sb-ext:double-float-positive-infinity
    #-sbcl '|Infinity|
    "The double-float positive infinity value if available, or a symbol.")
  (defconstant |-Infinity|
    #+sbcl sb-ext:double-float-negative-infinity
    #-sbcl '|-Infinity|
    "The double-float negative infinity value if available, or a symbol.")

  (define-modify-macro e-coercef (result-type &optional ejector)
    e-coerce))

; --- plain queue implementation ---

; used for vat queues

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

; --- sorted queue ---

(defclass sorted-queue ()
  ((elements :type list :initform nil))
  (:documentation "A mutable queue in which entries have numeric keys and are inserted only in their sorted positions in the queue."))
  
(defgeneric sorted-queue-peek (q absent-thunk))
(defgeneric sorted-queue-snapshot (q))
(defgeneric sorted-queue-pop (q))
(defgeneric sorted-queue-put (q key value))

; --- condition rules ---

(deftype e-catchable-condition () 'error)

; XXX do we want to use an official SealedBox/Brand module?

; XXX make this a defclass iff we relax the various E-level Throwable guards to not be cl:condition
(define-condition local-throw-sealed-box (error)
  ((value :initarg :value
          :reader local-throw-sealed-box-value)
   (backtrace :initarg :backtrace
              :reader local-throw-sealed-box-backtrace)))

(defmethod print-object ((c local-throw-sealed-box) stream)
  (print-unreadable-object (c stream :type t :identity nil)
    (write (local-throw-sealed-box-value c) :stream stream)))


(define-condition free-problem (error)
  ((value :initarg :value
          :reader free-problem-value)))
  

(defun transform-condition-for-e-catch (condition &key backtrace)
  (if *compatible-catch-leakage*
    condition
    (if (typep condition 'free-problem)
      (free-problem-value condition)
      (make-condition 'local-throw-sealed-box 
        :value condition
        :backtrace backtrace))))
  
(defgeneric e-problem-to-condition (problem))

(defmethod e-problem-to-condition ((problem t))
  problem)

(defmethod e-problem-to-condition ((problem local-throw-sealed-box))
  ;; XXX discarding backtrace
  (e-problem-to-condition (local-throw-sealed-box-value problem)))

; XXX e-problem-to-condition and e-problem-unseal are currently identical - will they ever be different?

(defgeneric e-problem-unseal (problem))

(defmethod e-problem-unseal ((problem t))
  problem)

(defmethod e-problem-unseal ((problem local-throw-sealed-box))
  (e-problem-unseal (local-throw-sealed-box-value problem)))

; --- fine-grain universal time ---

;; XXX this will break if we make an image after the time base is computed
;;   - add hook in rune::%revive-start to reset this
;;   - on lisps with adequate builtins or FFI, use a sane time-getting function (e.g. gettimeofday())

(defvar *apparent-internal-time-base*)

(loop with first-utime = (get-universal-time)
      for  next-utime  = (get-universal-time)
      until (/= first-utime next-utime)
      finally
        (setf *apparent-internal-time-base*
          (- next-utime (/ (get-internal-real-time) internal-time-units-per-second))))

(defun get-fine-universal-time ()
  "Return a real-number time value with the same epoch as universal time, but with potentially higher resolution."
  (+ *apparent-internal-time-base* (/ (get-internal-real-time) internal-time-units-per-second)))

; --- E booleans ---

;; E booleans are represented as a separate type because 'false' must be distinct from 'null'.

;; This code is placed early in order to get the compilation advantages of constants and inlining.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass e-boolean () ())

  (defvar *%e-true* nil)
  (defvar *%e-false* nil)
  
  (defun intern-boolean (value)
    (declare (boolean value)
             (notinline make-instance))
    (if value
      (or *%e-true*  (setf *%e-true*  (make-instance 'e-boolean)))
      (or *%e-false* (setf *%e-false* (make-instance 'e-boolean)))))

  (defmethod make-load-form ((b e-boolean) &optional environment)
    (declare (ignore environment))
    (cond
      ((eq b *%e-false*)
        '(intern-boolean nil))
      ((eq b *%e-true*)
        '(intern-boolean t))
      (t 
        (call-next-method)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +e-false+ (intern-boolean nil))
  (defconstant +e-true+  (intern-boolean t)))

(declaim (ftype (function (t) (member t nil)) e-is-true)
         (inline e-is-true))
(defun e-is-true (bool)
  "Convert an E boolean to a CL boolean."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (case bool
    (#.+e-false+ nil)
    (#.+e-true+  t)
    (otherwise 
      (eq (e-coerce bool 'e-boolean) +e-true+))))

(declaim (ftype (function (t) e-boolean) as-e-boolean)
         (inline as-e-boolean))
(defun as-e-boolean (x) 
  "Convert a CL generalized boolean to an E boolean."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (if x
    +e-true+
    +e-false+))

(defmethod print-object ((bool e-boolean) stream)
  (if (or *read-eval* (not *print-readably*))
    (format stream "#.~S" (if (e-is-true bool)
                            '+e-true+
                            '+e-false+))
    (error 'print-not-readable :object bool)))

