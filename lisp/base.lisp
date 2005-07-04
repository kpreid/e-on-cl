; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................


; --- fixing SBCL leak problem - code from jsnell - XXX REMOVE THIS CODE because mucking with sbcl internals just isn't right for distributed code - and it's been fixed equivalently, anyway ---

#+sbcl
(cl:in-package "SB-IMPL")

#+sbcl
(defun %eval (expr lexenv)
  (let ((fun (sb-c:compile-in-lexenv nil
                                     `(lambda () ,expr)
                                     lexenv)))
    (funcall fun)))

; --- ---

(cl:in-package "ELIB")

(defvar *java-e-compatible* t
  "Deprecated. If true, changes minor behaviors (such as some print representations) to match the Java implementation.

This variable is deprecated and will be replaced by more fine-grained and well-defined switches.")
(defvar *compatible-catch-leakage* nil
  "If true, allows thrown exceptions to be caught unsealed, for compatibility with the Java implementation.")

(defvar *break-on-ejections* nil
  "Equivalent to CL:*BREAK-ON-SIGNALS*, but applies to ejection. When an ejector is invoked with a value whose type is a subtype of the type specified by this variable's value, (break) will be called.")

(defvar *allow-unexternalizable-optimization* nil)

; xxx Assuming things about the implementation. Perhaps we should produce a warning if we don't know that the implementation uses these sizes.
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
; once we have thread dependencies, think about whether this can be optimized for locking purposes

; xxx since this is using mutable conses anyway, should we use a mutate-the-tail approach instead of the current mostly-functional queue?
;     is there a queue library available?

(defgeneric enqueue (queue value))
(defgeneric dequeue (queue))
(defgeneric queue-null (queue))

(defclass queue ()
  ((in  :initform () :accessor queue-in)
   (out :initform () :accessor queue-out)))
   
(defmethod enqueue ((queue queue) value)
  (push value (queue-in queue)))

(defmethod dequeue ((queue queue))
  (unless (queue-out queue)
    (setf (queue-out queue) (reverse (queue-in queue))
          (queue-in queue)  '()))
  (pop (queue-out queue)))

(defmethod queue-null ((queue queue))
  (not (or (queue-out queue) (queue-in queue))))

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
          :reader local-throw-sealed-box-value)))


(define-condition free-problem (error)
  ((value :initarg :value
          :reader free-problem-value)))
  

(defun transform-condition-for-e-catch (condition)
  (if *compatible-catch-leakage*
    condition
    (if (typep condition 'free-problem)
      (free-problem-value condition)
      (make-condition 'local-throw-sealed-box :value condition))))
  
(defgeneric e-problem-to-condition (problem))

(defmethod e-problem-to-condition ((problem t))
  problem)

(defmethod e-problem-to-condition ((problem local-throw-sealed-box))
  (e-problem-to-condition (local-throw-sealed-box-value problem)))

; XXX e-problem-to-condition and e-problem-unseal are currently identical - will they ever be different?

(defgeneric e-problem-unseal (problem))

(defmethod e-problem-unseal ((problem t))
  problem)

(defmethod e-problem-unseal ((problem local-throw-sealed-box))
  (e-problem-unseal (local-throw-sealed-box-value problem)))

; --- fine-grain universal time ---

(defvar *apparent-internal-time-base*)

; XXX will this be executed at all the right times?
(loop with first-utime = (get-universal-time)
      for  next-utime  = (get-universal-time)
      until (/= first-utime next-utime)
      finally
        (setf *apparent-internal-time-base*
          (- next-utime (/ (get-internal-real-time) internal-time-units-per-second))))

(defun get-fine-universal-time ()
  "Return a real-number time value with the same epoch as universal time, but with potentially higher resolution."
  (+ *apparent-internal-time-base* (/ (get-internal-real-time) internal-time-units-per-second)))

