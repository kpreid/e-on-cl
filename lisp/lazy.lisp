; Copyright 2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defclass lazy-ref (forwarding-ref)
  ()
  (:documentation "A ref which waits until it is used to determine its referent. Useful subclasses should have an EVALUATE-LAZY-REF method.

Also used as the result of operations likely to be chained, e.g. [].with(1).with(2).with(3), in which case the subclass overrides e-call-dispatch for the accumulation messages (with/1 in this example) and defines evaluate-lazy-ref to peek at the chain such that the result is computed without intermediate structures."))

(defmethod print-object ((ref lazy-ref) stream)
  ;; override method on REF, which performs ref-state and therefore would collapse the node unnecessarily
  (print-unreadable-object (ref stream :type t :identity t)))

(defgeneric evaluate-lazy-ref (ref))

(defmethod %ref-shorten ((ref lazy-ref))
  (let ((value (ref-shorten (evaluate-lazy-ref ref))))
    (change-class ref 'resolved-ref :target value)
    value))

(defclass with-node (lazy-ref)
  ((base :initarg :base :reader with-node-base)))
