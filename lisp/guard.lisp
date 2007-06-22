; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;; defgeneric in elib.lisp
(def-shorten-methods guard-to-type-specifier 1)

(defmethod guard-to-type-specifier (guard)
  (let ((sym (make-symbol (e-quote guard)))) 
    (setf (symbol-function sym) 
      (lambda (specimen) 
        (block nil
          (eql specimen 
               (e. guard |coerce| specimen
                                  (efun (c) 
                                    (declare (ignore c)) 
                                    (return nil)))))))
    (setf (get sym 'satisfies-type-specifier-guard) guard)
    `(satisfies ,sym)))

;; type-specifier-to-guard is defined earlier, in elib.lisp

(defglobal +the-any-guard+    (type-specifier-to-guard 't))
(defglobal +the-nullok-guard+ (type-specifier-to-guard 'null))
(defglobal +the-exception-guard+ (type-specifier-to-guard 'condition))

(defglobal +trivial-value-lists+
  (mapcar #'list
          (list nil 
                +e-false+ 
                0 0d0 0.0 
                (code-char 0))))

(def-vtable cl-type-guard
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +deep-frozen-stamp+))

  (:|coerce| (guard specimen opt-ejector)
    (e-coerce-native specimen (guard-to-type-specifier guard) opt-ejector guard))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    ;; XXX once we remove the capitalization from cl-type-simple-expr, insert it here (without the legacy lowercase case)
    (e. tw |print| (cl-type-simple-expr (guard-to-type-specifier this))))
  
  (:|getFQName| (this)
    (cl-type-fq-name (guard-to-type-specifier this)))
  (:|getTheTrivialValue| (this)
    (let ((ts (guard-to-type-specifier this)))
      (first (or (find-if (lambda (v) (typep (first v) ts))
                          +trivial-value-lists+)
                 ;; xxx should there be an ejector?
                 (error "No trivial value available")))))
  (:|getDocComment| (this)
    (or (documentation (guard-to-type-specifier this) 'type) ""))
  (:|getSupers| (this) 
    "Supertype information is not currently available for primitive types."
    (declare (ignore this))
    #())
  (:|getAuditors| (this)
    (declare (ignore this))
    #())
  (:|getMessageTypes| (this)
    ; xxx this is a bit inefficient
    (message-pairs-to-map-including-miranda-messages (vtable-message-types (guard-to-type-specifier this)))))

(def-atomic-sameness cl-type-guard
  (lambda (a b) (equal (guard-to-type-specifier a)
                       (guard-to-type-specifier b)))
  (lambda (a)   (sxhash (guard-to-type-specifier a))))

(defmethod e-call-match (fail (rec cl-type-guard) mverb &rest args)
  (let ((ts (guard-to-type-specifier rec)))
    (if (eql ts 't) ; XXX bad OO
      (cond
        ((and (not (eql mverb :|get/0|))
              (eql mverb (e-util:mangle-verb "get" (length args))))
          (efuncall (e-import "org.erights.e.elib.slot.makeUnionGuard") (coerce args 'vector)))
        ((eql mverb :|of/1|)
          (efuncall (e-import "org.erights.e.elib.slot.makeUnionGuard") (first args)))
        ((eql mverb :|match__of/1/2|)
          (e. (e-import "org.erights.e.elib.slot.makeUnionGuard")
              |match__run/1|
              (first args)
              (second args)))
        ((and (eql mverb :|__respondsTo/2|) 
              (or (string= (elt args 0) "get")
                  (samep args '("of" 1))
                  (samep args '("match__of/1" 1))))
          +e-true+)
        (t (call-next-method)))
      (call-next-method))))

(defun type-specifier-root (type-specifier)
  (typecase type-specifier
    (cons (first type-specifier))
    (t    type-specifier)))

(defmethod make-load-form ((object cl-type-guard) &optional environment)
  (declare (ignore environment))
  `(locally (declare (notinline make-instance))
     (make-instance ',(class-name (class-of object))
                    :type-specifier ',(guard-to-type-specifier object))))

