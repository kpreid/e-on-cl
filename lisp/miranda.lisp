; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.elib)

(eval-when (:compile-toplevel)
  (declaim (optimize (safety 3))))

; --- Miranda methods ---

(defglobals +miranda-message-descs+)

(defmacro def-miranda (func-name descs-name fqn (self-var mverb-litem args-litem  matcher-sym verb-list-sym) &body smethods)
  "The reason for this macro's existence is to allow compiling the miranda-methods into a single function and simultaneously capturing the method names and comments for alleged-type purposes. I realize this is ugly, and I'd be glad to hear of better solutions."
  (multiple-value-bind (otherwises specifics) 
      (partition (lambda (x) (eq (first x) 'otherwise)) smethods)
    `(symbol-macrolet ((,verb-list-sym ',(mapcar #'(lambda (smethod) (smethod-mverb smethod 0)) specifics)))
      (defglobal ,descs-name 
        ',(mapcan (lambda (e) (smethod-maybe-describe-fresh #'smethod-message-desc e)) specifics))
      (defun ,func-name (,self-var ,mverb-litem ,args-litem ,matcher-sym)
        (case ,mverb-litem
          ,@(loop for smethod in specifics collect
              (smethod-case-entry smethod args-litem '() :type-name fqn))
          ,@(loop for desc in otherwises collect
              `(otherwise 
                 #+e.method-lambdas (funcall (named-lambda ,(make-symbol (format nil "~A#<match>" fqn)) () ,@(cdr desc)))
                 #-e.method-lambdas 
                   ,@(cdr desc))))))))
 
;(declaim (inline miranda))
(def-miranda miranda +miranda-message-descs+ "org.erights.e.elib.prim.MirandaMethods" (self mverb args matcher-func miranda-mverbs)
  
    (audited-by-magic-verb (auditor)
      (declare (ignore auditor))
      nil)
  
    (:|__printOn| ((tw +the-text-writer-guard+))
      ; FUNCTION-based E-objects must always implement their own __printOn/1.
      (assert (not (typep self 'function)))
      (e. tw |print| "<" (aan (simplify-fq-name (cl-type-fq-name (observable-type-of self)))) ">"))

    (:|__getAllegedType| ()
      ; FUNCTION-based E-objects must always implement their own __getAllegedType/1.
      (assert (not (typep self 'function)))
      (type-specifier-to-guard (observable-type-of self)))

    (:|__respondsTo| ((verb 'string) (arity '(integer 0)))
      ;; The object itself must handle returning true for those verbs which it implements.
      (if (member (mangle-verb verb arity) miranda-mverbs)
        +e-true+
        (funcall matcher-func (constantly +e-false+))))

    (:|__conformTo| (guard)
      (declare (ignore guard))
      self)

    (:|__optSealedDispatch| (brand)
      (declare (ignore brand))
      nil)

    (:|__optUncall| () nil)

    (:|__whenMoreResolved| (reactor)
      (e-send-only-dispatch reactor :|run/1| self)
      nil)

    (:|__whenBroken| (reactor)
      (declare (ignore reactor))
      nil)

    (:|__order| ((nested-verb 'string) (nested-args 'vector))
      ; XXX document further (original doc is Mozilla-licensed)
      "Returns a tuple of the result of immediately calling this.<nested-verb>(<nested-args>*) and this."
      (vector (e-call self nested-verb nested-args)
              self))

    (:|__reactToLostClient| (problem)
      (declare (ignore problem))
      nil)

    (otherwise
      (funcall matcher-func (lambda ()
        (no-such-method self mverb args)))))

(defun or-miranda-message-descs (descs)
  "Given a *list* of MessageDescs, add all Miranda messages not already in the list and return the result as a *vector*."
  (let* ((out  (reverse +miranda-message-descs+))
         (seen (make-hash-table)))
    (loop for mdc on out do
      (setf (gethash (message-desc-mverb (first mdc)) seen) mdc))
    (loop for md in descs do
      (if (gethash (message-desc-mverb md) seen)
        (setf (first (gethash (message-desc-mverb md) seen)) md)
        (push md out)))
    (coerce (reverse out) 'vector)))
