; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defgeneric guard-to-type-specifier (guard))

(def-shorten-methods guard-to-type-specifier 1)

(defmethod guard-to-type-specifier ((guard cl-type-guard))
  (cl-type-specifier guard))

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

(defun type-specifier-to-guard (ts)
  (or
    (and (consp ts)
         (= (length ts) 2)
         (eq (first ts) 'satisfies)
         (get (second ts) 'satisfies-type-specifier-guard))
    (make-instance 'cl-type-guard :type-specifier ts)))

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
    (e-coerce-native specimen (cl-type-specifier guard) opt-ejector guard))
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (cl-type-simple-expr (cl-type-specifier this))))
  
  (:|getFQName| (this)
    (cl-type-fq-name (cl-type-specifier this)))
  (:|getTheTrivialValue| (this)
    (let ((ts (cl-type-specifier this)))
      (first (or (find-if (lambda (v) (typep (first v) ts))
                          +trivial-value-lists+)
                 ;; xxx should there be an ejector?
                 (error "No trivial value available")))))
  (:|getDocComment| (this)
    (with-slots (ts) this
      (let ((documentation (documentation ts 'type)))
        ; The CONS case is a workaround for an apparent bug in OpenMCL 0.14.2-p1.
        ; If not for that, this would be (or documentation "").
        (typecase documentation
          (string documentation)
          (cons   (first documentation))
          (null   "")))))
  (:|getSupers| (this) 
    "Supertype information is not currently available for primitive types."
    (declare (ignore this))
    #())
  (:|getAuditors| (this)
    (declare (ignore this))
    #())
  (:|getMessageTypes| (this)
    ; xxx this is a bit inefficient
    (with-slots (ts) this
      (message-pairs-to-map-including-miranda-messages (vtable-message-types ts)))))

(def-atomic-sameness cl-type-guard
  (lambda (a b) (equal (cl-type-specifier a)
                       (cl-type-specifier b)))
  (lambda (a)   (sxhash (cl-type-specifier a))))

(defmethod e-call-match ((rec cl-type-guard) mverb &rest args)
  (with-slots (ts) rec
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
                  (samep args '#("of" 1))
                  (samep args '#("match__of/1" 1))))
          +e-true+)
        (t (no-such-method rec mverb args)))
      (no-such-method rec mverb args))))

(defmethod make-load-form ((object cl-type-guard) &optional environment)
  (declare (ignore environment))
  `(locally (declare (notinline make-instance))
     (make-instance ',(class-name (class-of object))
                    :type-specifier ',(cl-type-specifier object))))

