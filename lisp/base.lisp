; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................


; --- fixing SBCL leak problem - code from jsnell - XXX REMOVE THIS CODE because mucking with sbcl internals just isn't right for distributed code ---

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

(defvar *java-e-compatible* t)
(defvar *compatible-catch-leakage* nil)

(defvar *break-on-ejections* nil)

; xxx Assuming things about the implementation. Perhaps we should produce a warning if we don't know that the implementation uses these sizes.
(deftype float64 () 'double-float) 
(deftype float32 () 'single-float)


(eval-when (:compile-toplevel :load-toplevel :execute) 

  (defconstant |NaN| 
    (or #+(and sbcl ppc)
	  (with-appropriate-floating-point-rules
	    (- sb-ext:double-float-positive-infinity sb-ext:double-float-positive-infinity))
        '|NaN|))
  (defconstant |Infinity|
    #+sbcl sb-ext:double-float-positive-infinity
    #-sbcl '|Infinity|)
  (defconstant |-Infinity|
    #+sbcl sb-ext:double-float-negative-infinity
    #-sbcl '|-Infinity|)

  (define-modify-macro e-coercef (result-type &optional ejector)
    e-coerce))

; --- adequate queue implementation ---

; used for vat queues
; once we have thread dependencies, think about whether this can be optimized for locking purposes

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


