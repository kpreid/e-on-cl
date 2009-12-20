; Copyright 2005-2008 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defmacro with-ref-transition-invariants ((ref-sym) &body body)
  "For a particular ref, which may be altered by 'change-class' or other means within the 'body', verify that only expected reference state transitions occur, signaling an (XXX vat-killing) error if, for example, a resolved far reference changes its identity, or a broken reference becomes unbroken. This macro is intended for implementation verification, not normal runtime use, and as such may have no effect at all."
  ; XXX add thorough checks
  (declare (ignore ref-sym))
  `(progn
    ,@body))

;; Not actually Ref-related, but used in this file.
(defmacro with-text-writer-to-string ((var) &body body &aux (stream-var (gensym)))
  `(with-output-to-string (,stream-var)
    (let ((,var (make-text-writer-to-cl-stream
                  ,stream-var
                  :autoflush nil) ))
      ,@body)))

;;; --- Ref protocol ---

(defgeneric e-call-dispatch (rec mverb &rest args)
  (:documentation "Main dispatch function for E calls and other call-like operations.

Shortens REC if needed. MVERB must be either a mangled verb as in E.UTIL:MANGLE-VERB, or a non-keyword symbol that is a magic verb (such as AUDITED-BY-MAGIC-VERB); if a mangled verb, its arity must be equal to the number of ARGS. A non-mangled-verb keyword symbol is erroneous.

Implementations must REF-SHORTEN the ARGS if they want shortened arguments, and not expose non-keyword mverbs to untrusted code. XXX This list of requirements is probably not complete.")
  (declare (optimize (speed 3))))

;; for optimization
#+sbcl
(defvar *sbcl-dispatch-result-derivers* nil)
#+sbcl
(defun sbcl-derive-dispatch-result (call)
  (some (lambda (f) (funcall f call)) *sbcl-dispatch-result-derivers*))
#+sbcl
(sb-c:defknown e-call-dispatch (t t &rest t) t
  (sb-c:any)
  :derive-type #'sbcl-derive-dispatch-result)

(defgeneric e-call-match (fail rec mverb &rest args)
  (:documentation "The last method of E-CALL-DISPATCH calls this method with the same arguments, if MVERB is not a Miranda method. This is used for implementing 'matchers'/inheritance.")
  (declare (optimize (speed 3))))

(defgeneric e-send-dispatch (rec mverb &rest args)
  (:documentation "Used only for REFs' various behaviors on sends; should not be specialized for non-REFs.")
  (declare (optimize (speed 3))))

(defgeneric e-send-only-dispatch (rec mverb &rest args)
  (:documentation "Used only for REFs' various behaviors on sends; should not be specialized for non-REFs. This is the no-return-value form, and always returns nil.")
  (declare (optimize (speed 3))))

(defgeneric same-hash-dispatch (a))
(defgeneric opt-same-dispatch (left right)
  (:documentation "e-boolean or nil; nil indicates no definition")
  (:method ((left t) (right t)) nil))

(defmacro def-atomic-sameness (type eq-func-form hash-func-form
    &aux (left (gensym)) (right (gensym)))
  `(progn
    (defmethod opt-same-dispatch ((,left ,type) (,right ,type))
      (as-e-boolean (,eq-func-form ,left ,right)))
    (defmethod opt-same-dispatch ((,left ,type) (,right t))
      +e-false+)
    (defmethod opt-same-dispatch ((,left t) (,right ,type))
      +e-false+)
    (defmethod same-hash-dispatch ((,left ,type))
      (,hash-func-form ,left))))

;; XXX move implementation to elib-guts? inline?
(declaim (ftype (function (t t) boolean) approvedp))
(defun approvedp (auditor specimen)
  ;; AUDITED-BY-MAGIC-VERB's argument is guaranteed to be shortened. XXX document this somewhere where implementors of AUDITED-BY-MAGIC-VERB will notice
  "Whether SPECIMEN has been approved by AUDITOR, as a CL boolean. AUDITOR and SPECIMEN will be shortened. AUDITOR must be near."
  (setf specimen (ref-shorten specimen))
  (setf auditor (ref-shorten auditor))
  (assert (eql (ref-state auditor) 'near)) ;; XXX better way to write this?
  (and (eql (ref-state specimen) 'near) ;; XXX is E auditing potentially applicable to non-near refs?
       (e-call-dispatch specimen
                        'audited-by-magic-verb
                        auditor)
       t))

(declaim (ftype (function (t (or string symbol character) (or list vector)) t) e-call e-send))

(defun e-call (rec verb args)
  (declare (string verb) (type (or list vector) args))
  (apply #'e-call-dispatch rec (mangle-verb verb (length args)) (coerce args 'list)))

(defun e-send (rec verb args)
  (declare (string verb) (type (or list vector) args))
  (apply #'e-send-dispatch rec (mangle-verb verb (length args)) (coerce args 'list)))

(defmacro e. (rec-form verb-des &rest args-forms)
  `(e-call-dispatch ,rec-form ,(mangle-verb (string verb-des) (length args-forms)) ,@args-forms))

(defmacro e<- (rec-form verb-des &rest args-forms)
  `(e-send-dispatch ,rec-form ,(mangle-verb (string verb-des) (length args-forms)) ,@args-forms))


(defgeneric %ref-shorten (ref)
  (:documentation "Implementation of reference shortening. Usage should call REF-SHORTEN instead, which optimizes common cases."))

;; REF-SHORTEN is defined *later* so that sbcl may "open-code test of type REF". This is here to declare the existence of the function.
(declaim (ftype (function (t) t) ref-shorten))


(defgeneric ref-state (ref)
  (:documentation "Returns 'near, (values 'eventual is-resolved), or (values 'broken problem)"))

(declaim (inline ref-opt-problem))
(defun ref-opt-problem (ref) ; XXX move this out of the protocol section
  "ref-state wrapper: returns the reference's problem iff the reference is broken."
  (multiple-value-bind (state tag) (ref-state ref)
    (when (eq state 'broken)
      (assert tag () "The ref ~S is broken but returned nil for its problem.")
      tag)))

(declaim (inline ref-is-resolved)
         (ftype (function (t) (member t nil)) ref-is-resolved))
(defun ref-is-resolved (ref) ; XXX move this out of the protocol section
  "ref-state wrapper: returns whether the reference is resolved."
  (multiple-value-bind (state tag) (ref-state ref)
    (if (eq state 'eventual)
      tag
      t)))

(defgeneric ref-opt-sealed-dispatch (ref brand)
  (:documentation "Provides access to the implementation of eventual references, and equivalent to __optSealedDispatch/1 on near references.")
  (:method ((near t) brand)
    (e-call-dispatch near :|__optSealedDispatch/1| brand)))

(define-condition message-condition (condition)
  ((recipient :initarg :recipient :initform (error "no recipient") :reader message-condition-recipient)
   (mverb :initarg :mverb :initform (error "no mverb") :reader message-condition-mverb)
   (args :initarg :args :initform (error "no args") :reader message-condition-args)))

(define-condition synchronous-call-error (error message-condition)
  ()
  (:documentation "Indicates an attempt to synchronously call an EVENTUAL ref (i.e. a promise or remote reference).")
  (:report (lambda (condition stream)
             (format stream "not synchronously callable: ~A.~A(~{~A~^, ~})"
               (e-quote (message-condition-recipient condition))
               (unmangle-verb (message-condition-mverb condition))
               (map 'list #'e-quote (message-condition-args condition))))))

(define-condition no-such-method-error (error message-condition)
  ()
  (:documentation "The recipient was called with a verb it does not have a method for.")
  (:report (lambda (condition stream)
             ;; xxx should report the args, since they're available
             (format stream "no such method: ~A#~A"
               (e-coerce
                 (e. (e. (message-condition-recipient condition)
                         |__getAllegedType|)
                     |getFQName|)
                 'string)
               (message-condition-mverb condition)))))

(declaim (ftype function no-such-method)) ; defined later

(define-condition vicious-cycle-error (error) ()
  (:documentation "Indicates that a promise resolved to itself.")
  (:report "Ref loop"))

(define-condition settling-error (error) ())

(define-condition insufficiently-settled-error (settling-error)
  ((values :initarg :values
           :reader insufficiently-settled-error-values
           :type list))
  (:documentation "Indicates an attempt to compare two references which have not yet settled sufficiently to be certain of their identity.

The 'values slot contains a list of the unsettled references.")
  (:report (lambda (condition stream
               &aux (values (insufficiently-settled-error-values condition)))
             (if (= 2 (length values))
               (format stream "not sufficiently settled: ~A == ~A"
                 (e-quote (first values))
                 (e-quote (second values)))
               (format stream "not sufficiently settled: ~A"
                 (with-text-writer-to-string (tw)
                   (e. (coerce values 'vector) |printOn|
                     "" ", " "" tw)))))))

(define-condition not-settled-error (settling-error)
  ((name :initarg :name
         :type string
         :reader not-settled-error-name)
   (value :initarg :value
          :reader not-settled-error-value))
  (:documentation "Indicates an attempt to use an unsettled reference where a settled one is required, e.g. as a map key or Far-ref identity.

The 'name slot gives a label for the value which was not settled, such as the name of the parameter. The 'value slot gives the actual unsettled reference.")
  (:report (lambda (condition stream)
             (format stream "not settled: ~A ~A"
               (not-settled-error-name condition)
               (e-quote (not-settled-error-value condition))))))

; --- defining some vat pieces early so that vat-checking does not come after local-resolver which subclasses it, which ABCL doesn't like ---

(defvar *vat* nil)

(defclass vat-checking ()
  ((vat-checking-expected-vat :type vat
                              :accessor vat-checking-expected-vat))
  (:documentation "Remembers the current vat and checks that it is correct upon later opportunities (currently e-call-dispatch). Should not be relied upon, as it may be made a noop in an optimized mode.

If there is no current vat at initialization time, captures the current vat at the next check time, so that vat-checking instances may be used during pre-vat-creation setup code."))

(defmethod shared-initialize ((this vat-checking) slot-names &key &allow-other-keys)
  (when *vat*
    (setf (vat-checking-expected-vat this) *vat*))
  (call-next-method))

(defmethod e-call-dispatch :before ((rec vat-checking) mverb &rest args)
  (with-accessors ((expected-vat vat-checking-expected-vat)) rec
    (if (slot-boundp rec 'vat-checking-expected-vat)
      (assert (eq *vat* expected-vat) ()
        "~S, which belongs to vat ~S, was called (~S ~S) from vat ~S."
        rec expected-vat mverb args *vat*)
      (when *vat*
        (setf expected-vat *vat*)))))

; --- Ref implementation ---

(defclass ref () ())

(defmethod %ref-shorten ((x t))
  ; NOTE: this is also implemented in REF-SHORTEN
  x)
(defmethod ref-state ((x t))
  (declare (ignore x))
  'near)

(macrolet ((defunimplemented (symbol (&rest other-args))
             `(defmethod ,symbol ((x ref) ,@other-args)
                (declare (ignore ,@(set-difference other-args
                                                   lambda-list-keywords)))
                (error "~S not implemented for ~W" ',symbol (type-of x)))))
  (defunimplemented %ref-shorten ())
  (defunimplemented ref-state ())
  (defunimplemented ref-opt-sealed-dispatch (brand))
  (defunimplemented e-call-dispatch (mverb &rest args))
  (defunimplemented e-send-dispatch (mverb &rest args)))

(defmethod print-object ((ref ref) stream)
  (let ((state (ref-state ref))
        (*print-circle* t))      ; !!
    (ccase state
      ((eventual)
        (print-unreadable-object (ref stream :type t :identity t)
          (format stream "Eventual")))
      ((broken) ; XXX multiple-value-bind the state
        (print-unreadable-object (ref stream :type t)
          (format stream "broken by ~A" (ref-opt-problem ref))))
      ((near)
        (call-next-method)))))

; XXX declare this inline
(defun make-unconnected-ref (problem)
  (make-instance 'unconnected-ref
    :problem problem))


; XXX should weakly reference buffer
(defclass promise-ref (ref)
  ((buffer :initform (make-array 0 :fill-pointer 0 :adjustable t)
           :type (array list)
           :documentation "Messages are pushed on the end of this vector."
           :reader promise-ref-buffer)
   (weak-when-more-resolved
     :initform (make-weak-hash-table :weakness :key :test #'eq)
     :type hash-table
     :documentation "Stores weak when-more-resolved messages for reference shortening."
     :reader %weak-when-more-resolved-table)
   (log-id :initform (log-unique-id)
           :reader promise-ref-log-id)))

(defmethod %ref-shorten ((ref promise-ref))
  ref)

(defmethod ref-state ((ref promise-ref))
  (values 'eventual nil))

(defmethod ref-opt-sealed-dispatch ((ref promise-ref) brand)
  nil)

(defmethod e-call-dispatch ((ref promise-ref) mverb &rest args)
  (error 'synchronous-call-error :recipient ref :mverb mverb :args args))

(declaim (inline e-send-promise-either))
(defun e-send-promise-either (opt-resolver ref mverb args)
  (let ((id (log-unique-id)))
    (log-event '("org.ref_send.log.SentIf" "org.ref_send.log.Sent" "org.ref_send.log.Event")
               `(("condition" . ,(promise-ref-log-id ref))
                 ("message"   . ,id)))
    (vector-push-extend (list opt-resolver mverb args id)
                        (promise-ref-buffer ref))))

(defmethod e-send-dispatch ((ref promise-ref) mverb &rest args)
  (multiple-value-bind (promise resolver) (make-promise)
    (e-send-promise-either resolver ref mverb args)
    promise))

(defmethod e-send-only-dispatch ((ref promise-ref) mverb &rest args)
  (e-send-promise-either nil ref mverb args)
  nil)

(defgeneric weak-when-more-resolved (ref weak-reactor action))

(defmethod weak-when-more-resolved ((ref t) weak-reactor action)
  (declare (ignore weak-reactor action))
  (values))

(defmethod weak-when-more-resolved ((ref ref) weak-reactor action)
  (declare (ignore weak-reactor action))
  (error "weak-when-more-resolved unimplemented for ~S" ref))

(defmethod weak-when-more-resolved ((ref promise-ref) weak-reactor action)
  (setf (gethash weak-reactor (%weak-when-more-resolved-table ref))
        action)
  (values))

;;; --- broken-ref ---

(defclass broken-ref (ref)
  ((problem :initarg :problem
            :type t
            :accessor %broken-ref-problem))
  (:documentation "Abstract. Make a DISCONNECTED-REF or an UNCONNECTED-REF instead of this."))

(defmethod shared-initialize :after ((this broken-ref) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (when *compatible-catch-leakage*
    (e-coercef (%broken-ref-problem this) +the-exception-guard+)))

(defmethod %ref-shorten ((x broken-ref))
  x)
(defmethod ref-state ((x broken-ref))
  (values 'broken (%broken-ref-problem x)))

(declaim (inline broken-ref-magic))
(defun broken-ref-magic (ref mverb args)
  (when (member mverb '(:|__whenMoreResolved/1| :|__whenBroken/1|))
    (e-send-only-dispatch (first args) :|run/1| ref))
  nil)

(defmethod e-call-dispatch ((ref broken-ref) mverb &rest args)
  (broken-ref-magic ref mverb args)
  (error (ref-opt-problem ref)))

(defmethod e-send-dispatch ((ref broken-ref) mverb &rest args)
  (broken-ref-magic ref mverb args)
  ref)

(defmethod e-send-only-dispatch ((ref broken-ref) mverb &rest args)
  (broken-ref-magic ref mverb args)
  nil)

(defmethod weak-when-more-resolved ((ref broken-ref) weak-reactor action)
  (declare (ignore weak-reactor action))
  (values))

(defclass unconnected-ref (broken-ref)
  ())

(defmethod ref-opt-sealed-dispatch ((ref unconnected-ref) brand)
  nil)

;;; - promises - 

(defclass local-resolver (vat-checking)
  ((ref :initarg :ref
        :type (or null promise-ref)
        :accessor resolver-opt-promise)))

(declaim (ftype (function () (values ref local-resolver)) make-promise))
(defun make-promise ()
  ;; NOTE: %enqueue-in-vat depends on this being thread-safe.
  (let ((promise (make-instance 'promise-ref)))
    (values promise (make-instance 'local-resolver :ref promise))))

;;;; --- near refs / vats ---

;;;; - forwarding-ref -

(defclass forwarding-ref (ref)
  ()
  (:documentation "Unconditionally forwards to another reference (existant or computed), whether a REF or not."))

(defclass resolved-ref (forwarding-ref)
  ((target :accessor resolved-ref-target))
  (:documentation "Forwards to an existing reference."))

(defmethod weak-when-more-resolved ((ref forwarding-ref) weak-reactor action)
  (weak-when-more-resolved (slot-value ref 'target) weak-reactor action))

(defmethod ref-state ((ref forwarding-ref))
  (ref-state (ref-shorten ref)))

(defmethod ref-opt-sealed-dispatch ((ref forwarding-ref) brand)
  (ref-opt-sealed-dispatch (ref-shorten ref) brand))

(defmethod e-call-dispatch ((ref forwarding-ref) mverb &rest args)
  (apply #'e-call-dispatch (ref-shorten ref) mverb args))

(defmethod e-send-dispatch ((ref forwarding-ref) mverb &rest args)
  (apply #'e-send-dispatch (ref-shorten ref) mverb args))

(defmethod e-send-only-dispatch ((ref forwarding-ref) mverb &rest args)
  (apply #'e-send-only-dispatch (ref-shorten ref) mverb args))

;;; - resolved-ref -

(defmethod print-object ((ref resolved-ref) stream)
  (if *print-readably*
    (error 'print-not-readable :object ref)
    (progn
      (print-unreadable-object (ref stream :type t :identity t))
      (format stream ":~W" (resolved-ref-target ref)))))

(defmethod shared-initialize ((ref resolved-ref) slot-names &key (target nil target-supplied-p) &allow-other-keys)
  (assert target-supplied-p)
  (setf (resolved-ref-target ref) target)
  (call-next-method))

(defmethod (setf resolved-ref-target) :after (new-target (ref resolved-ref))
  (weak-when-more-resolved
    new-target
    ref
    (lambda (ref)
      #+(or) (warn "got to resolved-ref shortener for ~S" ref)
      (ref-shorten ref))))

(defmethod %ref-shorten ((ref resolved-ref))
  ; NOTE: this is also implemented in REF-SHORTEN
  (setf (resolved-ref-target ref)
        (ref-shorten (resolved-ref-target ref))))


;;; --- SBCL-guts-specific optimization gimmicks ---

#+(or) ;; this is disabled because it triggers a sbcl optimizer bug (introduced in 0.9.16.42, according to Nikodemus Siivola) now that our CL gen makes the FUNCTIONness of E object expressions visible; see thread from http://article.gmane.org/gmane.lisp.steel-bank.devel/9878 "Compiler hang, sbcl 1.0.8/.9". XXX reenable this when bug is fixed
(sb-c:deftransform e-call-dispatch
    ((target mverb &rest args)
     (function t &rest t))
  "optimize e-call to function call"
  (let ((arg-names (mapcar (lambda (x)
                             (declare (ignore x))
                             (gensym "E-CALL-ARG"))
                           args)))
    `(lambda (target mverb ,@arg-names)
      (funcall target mverb ,@arg-names))))
