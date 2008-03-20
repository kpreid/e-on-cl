; Copyright 2005-2008 Kevin Reid, under the terms of the MIT X license
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

(defun message-pairs-to-map-including-miranda-messages (pairs)
  "PAIRS may contain duplicate keys, in which case the later one takes precedence"
  (e. (e. +the-make-const-map+ |fromIteratable| 
        (e-lambda nil () (:|iterate| (f)
          (loop for pair in pairs do
            (efuncall f (aref pair 0) (aref pair 1)))))
        +e-false+)
      |or|
      (message-types-to-map +miranda-message-descs+)))

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
    (or (eql auditor +deep-frozen-stamp+)
        (eql auditor +selfless+)))

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
    (coerce (documentation (guard-to-type-specifier this) 'type) 'doc-comment))
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
  (apply #'sugar-cache-call
    fail rec mverb
    ;; hmm, unnecessary recomputation?
    `(cl-type-guard ,(type-specifier-root (guard-to-type-specifier rec)))
    (format nil "~AGuardSugar" (e. rec |getFQName|))
    args))

(defun type-specifier-root (type-specifier)
  (typecase type-specifier
    (cons (first type-specifier))
    (t    type-specifier)))

(defmethod make-load-form ((object cl-type-guard) &optional environment)
  (declare (ignore environment))
  `(locally (declare (notinline make-instance))
     (make-instance ',(class-name (class-of object))
                    :type-specifier ',(guard-to-type-specifier object))))

;;; --- Same guard ---

(defclass same-guard ()
  ((allowed :initarg :allowed
            :reader %same-guard-allowed)))
(def-fqn same-guard "org.erights.e.elib.slot.Same$Same1")

(def-vtable same-guard
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (or (eql auditor +selfless+)
        (eql auditor +transparent-stamp+)))
  (:|__optUncall| (this) (vector (e-import "org.erights.e.elib.slot.Same")
                                 "get"
                                 (vector (%same-guard-allowed this))))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    ;; XXX use E-syntax printer for get expr
    (e. tw |quote| (e-import "org.erights.e.elib.slot.Same"))
    (e. tw |write| "[")
    (e. tw |quote| (%same-guard-allowed this))
    (e. tw |write| "]")
    nil)
  (:|getAllowed/0| '%same-guard-allowed)
  (:|coerce| (this specimen ejector)
    (setf specimen (ref-shorten specimen))
    ;; XXX to be reviewed: does it make sense to have a samep version?
    (if (same-yet-p specimen (%same-guard-allowed this))
      specimen
      (eject-or-ethrow ejector (to-condition 'simple-error "~A is not ~A" (e-quote specimen) (e-quote (%same-guard-allowed this)))))))

(defobject +make-same-guard+ "org.cubik.cle.prim.makeSameGuard"
    (:stamped +deep-frozen-stamp+
     :stamped +standard-graph-exit-stamp+
     :doc "This is the internal primitive maker -- import org.erights.e.elib.slot.Same instead.")
  (:|asType| () (type-specifier-to-guard 'same-guard))
  (:|run| (x) (make-instance 'same-guard :allowed x)))
