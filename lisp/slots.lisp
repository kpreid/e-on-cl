; Copyright 2005-2008 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; XXX review the actual uses of these various slot implementations; also consider moving them to e-lambdas
;;; XXX look for ways to reduce the duplication of these common makers

(defun import-uncall (fqn)
  `#(,(eelt (vat-safe-scope *vat*) "import__uriGetter") "get" #(,fqn)))

(defobject +the-make-simple-slot+ "org.erights.e.elib.slot.makeFinalSlot"
    (:stamped +deep-frozen-stamp+
     :stamped +pass-by-construction+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeFinalSlot"))
  (:|asType| () (type-specifier-to-guard 'e-simple-slot))
  (:|run| (value)
    (make-instance 'e-simple-slot :value value)))

(defobject +the-make-var-slot+ "org.erights.e.elib.slot.makeVarSlot"
    (:stamped +deep-frozen-stamp+
     :stamped +pass-by-construction+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeVarSlot"))
  (:|asType| () (type-specifier-to-guard 'e-var-slot))
  (:|run| (value)
    (make-instance 'e-var-slot :value value)))

(defobject +the-make-guarded-slot+ "org.erights.e.elib.slot.makeGuardedSlot"
    (:stamped +deep-frozen-stamp+)
  (:|__optUncall| ()
    (import-uncall "org.erights.e.elib.slot.makeGuardedSlot"))
  (:|asType| () (type-specifier-to-guard 'e-guarded-slot))
  (:|run| (guard value opt-ejector)
    (make-instance 'e-guarded-slot :value value
                                   :guard guard
                                   :opt-ejector opt-ejector)))
    
(defclass e-simple-slot () 
  ((value :initarg :value :reader simple-slot-value))
  (:documentation "A normal immutable slot."))

(defmethod print-object ((slot e-simple-slot) stream)
  ; xxx make readable under read-eval?
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "& ~W" (simple-slot-value slot))))

(def-vtable e-simple-slot
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (or (eql auditor +selfless+)
        (eql auditor +transparent-stamp+)))
  (:|__optUncall| (this)
    `#(,+the-make-simple-slot+ "run" #(,(simple-slot-value this))))
  (:|__printOn| (this (tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (e. tw |print| "<& ")
    (e. tw |quote| (simple-slot-value this))
    (e. tw |print| ">")
    nil)
  (:|get| (this)
    "Returns the constant value of this slot."
    (simple-slot-value this))
  (:|put| (this new-value)
    "Always fails."
    (declare (ignore new-value))
    (error "not an assignable slot: ~A" (e-quote this)))
  (:|isFinal| (this)
    "Returns true."
    (declare (ignore this))
    +e-true+))

(defmethod make-load-form ((a e-simple-slot) &optional environment)
  (make-load-form-saving-slots a :environment environment))  

(defclass base-closure-slot (vat-checking)
  ((getter :initarg :getter
           :type (function () t)
           :accessor closure-slot-getter)
   (setter :initarg :setter
           :type (function (t) *)
           :accessor closure-slot-setter)))

(def-vtable base-closure-slot
  (:|get| (this)
    (funcall (closure-slot-getter this)))
  (:|isFinal| (this)
    (declare (ignore this))
    +e-false+))

(defmethod shared-initialize :after ((slot base-closure-slot) slot-names &key (value nil value-supplied) &allow-other-keys)
  (declare (ignore slot-names))
  (with-accessors ((getter closure-slot-getter) 
                   (setter closure-slot-setter))
                  slot
    (when value-supplied
      (assert (not (slot-boundp slot 'getter)))
      (assert (not (slot-boundp slot 'setter)))
      (setf getter (lambda () value)
            setter (lambda (new) (setf value new))))))
  

(defclass e-var-slot (base-closure-slot) 
  ())

(def-vtable e-var-slot
  (:|__printOn| (this (tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (e. tw |print| "<var ")
    (e. tw |quote| (e. this |get|))
    (e. tw |print| ">"))
  (:|__optUncall| (this)
    `#(,+the-make-var-slot+ "run" #(,(e. this |get|))))
  (:|put| (this new-value)
    (funcall (closure-slot-setter this) new-value)
    nil))

(defmethod print-object ((slot e-var-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W" (e. slot |get|))))

(defclass e-guarded-slot (base-closure-slot) 
  ((guard :initarg :guard :reader guarded-slot-guard)))

(defmethod shared-initialize :around ((slot e-guarded-slot) slot-names &rest initargs &key (value nil value-supplied) guard opt-ejector &allow-other-keys)
  "if this slot is initialized with a value, it is coerced; if initialized with a getter and setter, it is assumed to be correct"
  (declare (ignore slot-names))
  (if value-supplied
    (apply #'call-next-method slot slot-names
      :value (e. guard |coerce| value opt-ejector)
      initargs)
    (call-next-method)))

(def-vtable e-guarded-slot
  (:|__printOn| (this (tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (with-accessors ((guard guarded-slot-guard) 
                     (getter closure-slot-getter))
                    this
      (e. tw |print| "<var ")
      (e. tw |quote| (funcall getter))
      (e. tw |print| " :")
      (e. tw |quote| guard)
      (e. tw |print| ">")))
  (:|put| (this new-value)
    (funcall (closure-slot-setter this) 
      (e. (guarded-slot-guard this) |coerce| new-value nil))
    nil))

(defmethod print-object ((slot e-guarded-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W :~W" 
      (if (slot-boundp slot 'getter) 
        (e. slot |get|)
        '#:<unbound-getter>)
      (ignore-errors (guarded-slot-guard slot)))))

;; XXX this is duplicated with information in the maker functions
(def-fqn e-simple-slot  "org.erights.e.elib.slot.finalSlot")
(def-fqn e-var-slot     "org.erights.e.elib.slot.varSlot")
(def-fqn e-guarded-slot "org.erights.e.elib.slot.guardedSlot")

