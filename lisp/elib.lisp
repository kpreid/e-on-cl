; This Common Lisp code is Copyright 2005 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.elib)

(eval-when (:compile-toplevel)
  (declaim (optimize (safety 3))))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  ; XXX tests for this
  (declaim (ftype (function (string) string) simplify-fq-name))
  (defun simplify-fq-name (fq-name)
    " Drops any package or containing class prefixes and any \"__C\" or \"__T\"
      suffixes prior to the last \"significant\" name.
      
      A \"significant\" name is any name that doesn't begin with a digit
      (ruling out anonymous objects and classes) and that isn't \"self\"."
    (let* ((last-sep (or (position-if (lambda (x) (member x '(#\. #\$))) fq-name :from-end t)
                         -1))
           (result (subseq fq-name (1+ last-sep))))
      (let ((suffix-start (- (length result) 3)))
        (when (and (>= suffix-start 0)
                   (or (string= "__C" result :start2 suffix-start)
                       (string= "__T" result :start2 suffix-start)))
          (setf result (subseq result 0 suffix-start))))
      (if (and (> (length result) 0)
               (or (string= "self" result)
                   (digit-char-p (aref result 0))))
        ; result so far is not "significant"
        (concatenate 'string "..."
                             (simplify-fq-name (subseq fq-name 0 last-sep))
                             (list (aref fq-name last-sep))
                             result)
        (if (string= result "") ; sanity check
          fq-name
          result)))))

; --- Macros ---

(defmacro def-atomic-sameness (type eq-func-form hash-func-form
    &aux (left (gensym)) (right (gensym)))
  `(progn 
    (defmethod eeq-same-dispatch ((,left ,type) (,right ,type))
      (,eq-func-form ,left ,right))
    (defmethod eeq-hash-dispatch ((,left ,type))
      (,hash-func-form ,left))))

(defmacro with-text-writer-to-string ((var) &body body &aux (stream-var (gensym)))
  `(with-output-to-string (,stream-var)
    (let ((,var (make-text-writer-to-cl-stream
                  ,stream-var
                  :autoflush nil) ))
      ,@body)))

(defmacro with-ref-transition-invariants ((ref-sym) &body body)
  "For a particular ref, which may be altered by 'change-class' or other means within the 'body', verify that only expected reference state transitions occur, signaling an (XXX vat-killing) error if, for example, a resolved far reference changes its identity, or a broken reference becomes unbroken. This macro is intended for implementation verification, not normal runtime use, and as such may have no effect at all."
  ; XXX add thorough checks
  (declare (ignore ref-sym))
  `(progn
    ,@body))

; --- Ref protocol ---

;(declaim (ftype (function (t keyword &rest t) t) e-call-dispatch e-call-match e-send-dispatch))

; mverb is :|verb/arity|
(defgeneric e-call-dispatch (rec mverb &rest args)
  (declare (optimize (speed 3))))
(defgeneric e-call-match    (rec mverb &rest args)
  (declare (optimize (speed 3))))
(defgeneric e-send-dispatch (rec mverb &rest args)
  (declare (optimize (speed 3))))

(defgeneric eeq-hash-dispatch (a))
(defgeneric eeq-same-dispatch (left right))
(defgeneric eeq-is-transparent-selfless (a))

; user code must never be able to define methods of this function
(defgeneric e-audit-check-dispatch (auditor specimen))

(declaim (ftype (function (t (or string symbol character) (or list vector)) t) e-call e-send))

(defun e-call (rec verb args)
  (declare (string verb) (type (or list vector) args))
  (apply #'e-call-dispatch rec (e-util:mangle-verb verb (length args)) (coerce args 'list)))
  
(defun e-send (rec verb args)
  (declare (string verb) (type (or list vector) args))
  (apply #'e-send-dispatch rec (e-util:mangle-verb verb (length args)) (coerce args 'list)))

(defmacro e. (rec-form verb-des &rest args-forms)
  `(e-call-dispatch ,rec-form ,(e-util:mangle-verb (string verb-des) (length args-forms)) ,@args-forms))

;; this doesn't seem to actually be faster
;(defmacro e. (rec-form verb-des &rest args-forms
;    &aux (rec-sym (gensym))
;         (mverb (e-util:mangle-verb (string verb-des) (length args-forms)))
;         (arg-syms (mapcar (lambda (x) (gensym)) args-forms)))
;  `(let ((,rec-sym ,rec-form)
;         ,@(mapcar #'list arg-syms args-forms))
;    (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
;    (typecase ,rec-sym
;      (function (funcall         ,rec-sym ',mverb ,@arg-syms))
;      (t        (e-call-dispatch ,rec-sym ',mverb ,@arg-syms)))))

(defmacro e<- (rec-form verb-des &rest args-forms)
  `(e-send-dispatch ,rec-form ,(e-util:mangle-verb (string verb-des) (length args-forms)) ,@args-forms))


(defgeneric %ref-shorten (ref)
  (:documentation "Implementation of reference shortening. Usage should call REF-SHORTEN instead, which optimizes common cases."))
  
;; REF-SHORTEN is defined *later* so that sbcl may "open-code test of type REF". This is here to declare the existence of the function.
(declaim (ftype (function (t) t) ref-shorten))


;(declaim (ftype (function (t) (values (member near eventual broken)
;                                      &optional t))
;                ref-state))

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

(define-condition synchronous-call-error (error) ()
  (:documentation "Indicates an attempt to synchronously call an EVENTUAL ref (i.e. a promise or remote reference.).")
  (:report "not synchronously callable"))

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
                   (e. (e-coercef values 'vector) |printOn| 
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

(defmethod e-call-dispatch ((rec t) (mverb symbol) &rest args)
  "Fallthrough case for e-call-dispatch - forwards to e-call-match so the class hierarchy gets another chance."
  (elib:miranda rec mverb args (lambda ()
    (apply #'e-call-match rec mverb args))))

(defmethod e-call-match ((rec t) mverb &rest args)
  "Final case of E call dispatch - always fails."
  (declare (ignore args))
  (error "no such method: ~A#~A" (simplify-fq-name (cl-type-fq-name (observable-type-of rec))) mverb))

; --- defining some vat pieces early so that vat-checking does not come after local-resolver which subclasses it, which ABCL doesn't like ---

(defvar *vat* nil)

(defclass vat-checking () 
  ((expected-vat :initform *vat*
                 :type (or null vat)))
  (:documentation "Remembers the current vat and checks that it is correct upon later opportunities (currently e-call-dispatch). Should not be relied upon, as it may be made a noop in an optimized mode.

If there is no current vat at initialization time, captures the current vat at the next check time, so that vat-checking instances may be used during pre-vat-creation setup code."))

(defmethod e-call-dispatch :before ((rec vat-checking) mverb &rest args)
  (declare (ignore mverb args))
  (with-slots (expected-vat) rec
    (if expected-vat
      (assert (eq *vat* expected-vat))
      (setf expected-vat *vat*))))

; --- Ref implementation ---

(defclass ref () ())

(defmethod %ref-shorten ((x t)) 
  ; NOTE: this is also implemented in REF-SHORTEN
  x)
(defmethod ref-state ((x t))
  (declare (ignore x))
  'near)
  
(defmethod %ref-shorten ((x ref))
  (error "ref-shorten not implemented by ~W" (type-of x)))
(defmethod ref-state ((x ref))
  (error "ref-state not implemented by ~W" (type-of x)))
(defmethod e-call-dispatch ((x ref) mverb &rest args)
  (declare (ignore mverb args))
  (error "e-call-dispatch not implemented by ~W" (type-of x)))
(defmethod e-send-dispatch ((x ref) mverb &rest args)
  (declare (ignore mverb args))
  (error "e-send-dispatch not implemented by ~W" (type-of x)))
  
(defmethod e-audit-check-dispatch (auditor (ref ref)
    &aux (short (ref-shorten ref)))
  (unless (eql ref short)
    (e-audit-check-dispatch auditor short)))

(defmethod print-object ((ref ref) stream)
  (let ((state (ref-state ref))
        (*print-circle* t))      ; !!
    (ccase state
      ((eventual)
        (print-unreadable-object (ref stream)
          (format stream "Eventual")))
      ((broken) ; XXX multiple-value-bind the state
        (print-unreadable-object (ref stream)
          (format stream "broken by ~A" (ref-opt-problem ref))))
      ((near)     
        (if *print-readably*
          (error 'print-not-readable 'object ref)
          (format stream "#<ref>:~W" (ref-shorten ref)))))))

; XXX declare this inline
(defun make-unconnected-ref (problem)
  (make-instance 'unconnected-ref
    :problem problem))


; XXX should weakly reference buffer
(defclass promise-ref (ref) 
  ((buffer :initform (make-array 0 :fill-pointer 0 :adjustable t)
           :type (array list)
           :documentation "Messages are pushed on the end of this vector.")))

(defmethod %ref-shorten ((ref promise-ref))
  ref)

(defmethod ref-state ((ref promise-ref))
  (values 'eventual nil))

(defmethod e-call-dispatch ((ref promise-ref) mverb &rest args)
  (declare (ignore mverb args))
  (error 'synchronous-call-error))

(defmethod e-send-dispatch ((ref promise-ref) mverb &rest args)
  ; XXX provide send-only
  (with-slots (buffer target) ref
    (multiple-value-bind (promise resolver) (make-promise)
      (vector-push-extend (list resolver mverb args) buffer)
      promise)))

; - broken-ref -

(defclass broken-ref (ref)
  ((problem :initarg :problem
            :type t)))
 
(defmethod shared-initialize :after ((this broken-ref) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (when *compatible-catch-leakage*
    (e-coercef (slot-value this 'problem) +the-exception-guard+)))

(defmethod %ref-shorten ((x broken-ref))
  x)
(defmethod ref-state ((x broken-ref))
  (values 'broken (slot-value x 'problem)))

(declaim (inline broken-ref-magic))
(defun broken-ref-magic (ref mverb args)
  (when (member mverb '(:|__whenMoreResolved/1| :|__whenBroken/1|))
    (e<- (first args) |run| ref))
  nil)

(defmethod e-call-dispatch ((ref broken-ref) mverb &rest args)
  (broken-ref-magic ref mverb args)
  (error (ref-opt-problem ref)))

(defmethod e-send-dispatch ((ref broken-ref) mverb &rest args)
  (broken-ref-magic ref mverb args)
  ref)

; - identified ref -

(defclass identified-ref (ref) 
  ((identity :initarg :identity
             :reader identified-ref-identity)))

(def-atomic-sameness identified-ref
  (lambda (a b) (eeq-is-same-ever (identified-ref-identity a)
                                  (identified-ref-identity b)))
  (lambda (a) (eeq-same-yet-hash (identified-ref-identity a) nil)))

(defmethod ref-state :around ((ref identified-ref))
  (let ((result (multiple-value-list (call-next-method))))
    (destructuring-bind (state &optional tag) result
      (when (eq state 'eventual)
        (assert tag () "implementation inconsistency: identified-ref claiming to be unresolved")))
    (values-list result)))

; - kinds of broken refs - 

(defclass unconnected-ref (broken-ref)
  ())

(defclass disconnected-ref (broken-ref identified-ref)
  ())

; - promises - 

(defclass local-resolver (vat-checking)
  ((ref :initarg :ref
        :type (or null promise-ref))))

(declaim (ftype (function () (values ref local-resolver)) make-promise))
(defun make-promise ()
  (let ((promise (make-instance 'promise-ref)))
    (values promise (make-instance 'local-resolver :ref promise))))

; --- near refs / vats ---

; - forwarding-ref -

(defclass forwarding-ref (ref)
  ((target :initarg :target))
  (:documentation "Unconditionally forwards to another reference, whether a 'ref or not."))

; XXX we could eliminate the need for these individual forwarders by having the methods specialized on 'ref forward to the ref-shortening of this ref if it's distinct

(defmethod %ref-shorten ((ref forwarding-ref))
  ; NOTE: this is also implemented in REF-SHORTEN
  (with-slots (target) ref
    (setf target (ref-shorten target))))

(defmethod ref-state ((ref forwarding-ref))
  (with-slots (target) ref
    (ref-state target)))

(defmethod e-call-dispatch ((ref forwarding-ref) mverb &rest args)
  (with-slots (target) ref
    (apply #'e-call-dispatch target mverb args)))

(defmethod e-send-dispatch ((ref forwarding-ref) mverb &rest args)
  (with-slots (target) ref
    (apply #'e-send-dispatch target mverb args)))

; - vat -

(defun %with-turn (body vat)
  "Execute the 'body' thunk, as a turn in the given vat."
  (assert (and (not (vat-in-turn vat))
               (or (null *vat*)
                   (eq *vat* vat)))
          (vat))
  (unwind-protect
    (let ((*vat* vat))
      (setf (vat-in-turn vat) t)
      (funcall body))
    (setf (vat-in-turn vat) nil)))

(defmacro with-turn ((vat) &body body)
  "Execute the body, as a turn in the given vat."
  `(%with-turn (lambda () ,@body) ,vat))

(defclass vat ()
  ((in-turn :initform nil
            :accessor vat-in-turn
            :type boolean
            :documentation "Whether some type of top-level turn is currently being executed in this vat. Used for consistency checking.")
   (sends :type queue
          :initform (make-instance 'queue))
   (time-queue :type sorted-queue
               :initform (make-instance 'sorted-queue))
   (safe-scope :initform (e.knot:make-safe-scope)
               :accessor vat-safe-scope)))

(defun vat-enqueue-turn (vat fun)
  ; xxx threading: either this acquires a lock or the queue is rewritten thread-safe
  (enqueue (slot-value vat 'sends) fun))

(defmethod e-send-dispatch (rec mverb &rest args)
  (assert (eq (ref-state rec) 'near) () "inconsistency: e-send-dispatch default case was called with a non-NEAR receiver")
  (multiple-value-bind (promise resolver) (make-promise)
    (vat-enqueue-turn *vat* (lambda ()
      ; XXX direct this into a configurable tracing system once we have one
      ;(format *trace-output* "~&; running ~A ~A <- ~A ~A~%" (e. (e. rec |__getAllegedType|) |getFQName|) (e-quote rec) (symbol-name mverb) (e-quote (coerce args 'vector)))
      (e. resolver |resolve| 
        (handler-case 
          (apply #'e-call-dispatch rec mverb args)
          (error (p)
            (format *trace-output* "~&; problem in send ~A <- ~A ~A: ~A~%" (e-quote rec) (symbol-name mverb) (e-quote (coerce args 'vector)) p)
            (make-unconnected-ref (transform-condition-for-e-catch p)))))))
    promise))

(defun run-vats ()
  "Should not exist. Currently used by the repl in rune.lisp. XXX make the repl use proper async IO."
  ; XXX missing a with-turn, but hasn't been proven so
  (with-slots (sends) *vat*
    (loop until (queue-null sends)
          do    (funcall (dequeue sends)))))

(defun serve-event-with-time-queue (time-queue immediate-queue &optional (timeout nil))
  (destructuring-bind (&optional qtime &rest qfunc)
      (sorted-queue-peek time-queue (lambda () nil))
    (when (and qtime (<= qtime (get-fine-universal-time)))
      (sorted-queue-pop time-queue)
      (enqueue immediate-queue qfunc)
      (return-from serve-event-with-time-queue t))
    (if qtime
      (let ((delta (min (- qtime (get-fine-universal-time))
                        most-positive-fixnum)))
        (e-util:serve-event (if timeout (min timeout delta)
                                        delta)))
      (if timeout
        (e-util:serve-event timeout)
        (e-util:serve-event)))))

(defun vat-loop ()
  (with-slots (sends time-queue) *vat*
    (loop
      (if (queue-null sends)
        (serve-event-with-time-queue time-queue sends)
        (progn
          (with-turn (*vat*)
            (funcall (dequeue sends)))
          (serve-event-with-time-queue time-queue sends 0))))))

(defun vat-enqueue-timed (time func)
  "Arrange for 'func' to be called with no arguments outside of a turn at universal time 'time' in the current vat."
  (with-slots (time-queue) *vat*
    (sorted-queue-put time-queue time func)))

(defun establish-vat ()
  (assert (null *vat*))
  (setf *vat* (make-instance 'vat)))

; --- Type description objects ---

; These are defined early here so that the classes are available for use in the various __getAllegedType implementations, some of which construct the type description at macroexpansion time.

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defclass type-desc () 
    ((doc-comment :initarg :doc-comment
                  :reader type-desc-doc-comment
                  :type string)
     (opt-fq-name :initarg :fq-name
                  :reader type-desc-opt-fq-name
                  :type (or null string))
     (supers :initarg :supers
             :reader type-desc-supers
             :type (vector t))
     (auditors :initarg :auditors
               :reader type-desc-auditors
               :type (vector t))
     (message-types-v :initarg :message-types-v
                      :reader type-desc-message-types-v
                      :type (vector message-desc))))
  
  (defgeneric type-desc-message-types (type-desc))
  
  (defclass message-desc () 
    ((verb :initarg :verb 
           :reader message-desc-verb
           :type string)
     (doc-comment :initarg :doc-comment :initform ""  
                  :reader message-desc-doc-comment
                  :type string)
     (opt-result-guard :initarg :opt-result-guard :initform nil
                       :reader message-desc-opt-result-guard
                       :type t)
     (params :initarg :params
             :reader message-desc-params
             :type (vector param-desc)))
    (:documentation "A description of a message signature within a type description."))
  
  (defclass param-desc () 
    ((opt-name :initarg :opt-name :initform nil
               :reader param-desc-opt-name
               :type (or null string))
     (opt-guard :initarg :opt-guard :initform nil
                :reader param-desc-opt-guard
                :type t)))

  ; The vtables for {type,message,param}-desc may be found in elib-values.lisp.

  ; This is necessary because E-LAMBDA and elang objects have 'literal' 
  ; TypeDescs in their __getAllegedType/0 methods.
  ; 
  ; The definition was copied from
  ; http://www.lispworks.com/reference/HyperSpec/Body/f_mk_ld_.htm
  ;
  (defmethod make-load-form ((a type-desc) &optional environment)
    (make-load-form-saving-slots a :environment environment))
  (defmethod make-load-form ((a message-desc) &optional environment)
    (make-load-form-saving-slots a :environment environment))
  (defmethod make-load-form ((a param-desc) &optional environment)
    (make-load-form-saving-slots a :environment environment))
  
  (defvar +miranda-message-descs+)
  
  (defun message-pairs-to-map-including-miranda-messages (pairs)
    (e. (e. +the-make-const-map+ |fromPairs| 
          (coerce pairs 'vector))
        |or|
        (message-types-to-map +miranda-message-descs+)))
  
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
      ;(print (mapcar #'e-quote out))
      (coerce (reverse out) 'vector))))

; --- E objects ---

; A built-for-E object is a FUNCTION expecting (mverb &rest args). It handles its own dispatch, match, and no-such-method.
;
; If mverb is *not* in the KEYWORD package, then it is a special verb and invocations of it should not be revealed to user code. Currently, the only special verb is 'audited-by-magic-verb, which is used by the auditing mechanism.

; These must be defined early as some e-calls to function objects are performed in later loading.
(defmethod e-call-dispatch ((recip function) mverb &rest args)
  (apply recip mverb args))

(defmethod e-audit-check-dispatch (auditor (specimen function))
  (funcall specimen 'audited-by-magic-verb auditor))


(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun vtable-entry-mverb (desc prefix-arity
      &aux (verb-or-mverb-string (string (first desc))))
    (if (find #\/ verb-or-mverb-string)
      (intern verb-or-mverb-string "KEYWORD")
      (e.util:mangle-verb
        verb-or-mverb-string
        (multiple-value-bind (min max)
            (progn
              (assert (cddr desc) () "Inferring arity for function vtable entry not supported: ~S" desc)
              (e.util:lambda-list-arguments-range (second desc)))
          (assert (= min max) () "Variable arguments not yet supported for vtable-case-entry arity inference")
          (- min prefix-arity)))))

  (defun vtable-case-entry (desc args-sym prefix-args &key type-name
      &aux (mverb (vtable-entry-mverb desc (length prefix-args))))
    "Return a CASE clause whose key is a mangled-verb, suitable for implementing method dispatch for E objects.

The first element of the desc is a string designator for the verb. If it contains a slash, it is treated as a mangled-verb (verb/arity); if not, the second element must be a lambda list, which is used to determine the number of arguments.

If there is exactly one further element, then it is an expression (typically #'...) which the returned clause will use in `(apply ,expr ,args-sym).

If there are two or more further elements, then they are a lambda list and implicit-progn forms which will be evaluated.

prefix-args is a list of forms which will be prepended to the arguments of the method body."
    (assert (not (eq (first desc) 'otherwise)))
    `((,mverb) 
      ,(vtable-method-body (rest desc) args-sym prefix-args :type-name type-name :verb-name mverb)))

  (defun vtable-method-body (body args-sym prefix-args &key type-name verb-name)
    "XXX transfer the relevant portions of vtable-case-entry's documentation here"
    (if (= 1 (length body))
      `(apply ,(first body) ,@prefix-args ,args-sym)
      (let* ((name (format nil "~A#~A" type-name verb-name))
             (name-sym
               ; These are feature conditionals to remind me that they must be set at compile time anyway.
               #-e.intern-vtable-methods
                 (make-symbol name)
               #+e.intern-vtable-methods
                 (loop 
                   for i from 1
                   for free = name then (format nil "~A-dup-~A" name i)
                   while (find-symbol name :e.elib.vtable-methods) 
                   finally (intern free :e.elib.vtable-methods))))
        `(apply (named-lambda ,name-sym ,@body) ,@prefix-args ,args-sym))))

  (defun lambda-list-to-param-desc-vector (list arity prefix-arity)
    ; XXX doesn't handle &rest, &optional or inappropriate lambda lists - particularly, it may return a too-short vector
    (coerce (loop 
      for i below arity
      for sym in (nthcdr prefix-arity list)
      collect 
        (make-instance 'param-desc :opt-name
          (with-standard-io-syntax
            (let ((*print-case* :downcase)
                  (*package* (or (symbol-package sym) (find-package :cl))))
              (write-to-string sym))))) 'vector))
                            
  (defun vtable-entry-message-desc-pair (entry &rest keys &key (prefix-arity 0) &allow-other-keys
      &aux (mverb (vtable-entry-mverb entry prefix-arity)))
    (vector (symbol-name mverb) 
            (apply #'vtable-entry-message-desc entry keys)))
  
  (defun vtable-entry-message-desc (entry &key (prefix-arity 0)
      &aux (mverb (vtable-entry-mverb entry prefix-arity))
           (impl-desc (rest entry)))
    (multiple-value-bind (verb arity) (e-util:unmangle-verb mverb)
      ; XXX *TEMPORARY* workaround for ABCL's lack of MAKE-LOAD-FORM support
      #+abcl "abcl-message-desc-load-gimmick"
      #-abcl (make-instance 'message-desc
        :verb verb
        :doc-comment (or (find-if #'stringp impl-desc) "") ; XXX too lenient
        :params (if (rest impl-desc) ; therefore inline
                  (lambda-list-to-param-desc-vector (first impl-desc) arity prefix-arity)
                  (make-array arity :initial-element 
                    (make-instance 'param-desc))))))
                            
  (defun nl-miranda-case-maybe (verb entries &rest body)
    (unless (find verb entries :key #'car)
      `(((,verb) ,@body))))

  (defun gen-fqn ()
    (format nil "~A$_" (namestring (or *compile-file-truename* *load-truename* #p"<cl-eval>"))))
    
  (defun e-lambda-expansion (method-entries fqn documentation stamp-forms opt-otherwise-body
      &aux (self-func (make-symbol fqn))
           (self-expr `#',self-func)
           (stamps-sym (gensym "STAMPS"))
           (mverb-sym  (gensym "MVERB"))
           (args-sym   (gensym "ARGS")))
    (declare (string fqn documentation))
    `(let ((,stamps-sym (vector ,@stamp-forms)))
        (labels ((,self-func (,mverb-sym &rest ,args-sym)
          (case ,mverb-sym
            ,@(loop for desc in method-entries
                    collect (vtable-case-entry desc args-sym '() :type-name fqn))
            ,@(nl-miranda-case-maybe :|__printOn/1| method-entries `(destructuring-bind (tw) ,args-sym
              (e-coercef tw +the-text-writer-guard+)
              (e. tw |print| ',(format nil "<~A>" (simplify-fq-name fqn)))
              nil))
            ,@(nl-miranda-case-maybe :|__getAllegedType/0| method-entries `(destructuring-bind () ,args-sym
              (make-instance 'type-desc
                :doc-comment ',documentation
                :fq-name ',fqn
                :supers #()
                :auditors #()
                :message-types-v
                  (or-miranda-message-descs
                    ',(mapcar #'vtable-entry-message-desc method-entries)))))
            
            ,@(nl-miranda-case-maybe :|__respondsTo/2| method-entries `(destructuring-bind (verb arity) ,args-sym
              (as-e-boolean (or
                (member (e-util:mangle-verb verb arity) 
                        ',(mapcar (lambda (entry) (vtable-entry-mverb entry 0)) 
                                  method-entries))
                (e-is-true (elib:miranda ,self-expr ,mverb-sym ,args-sym nil))))))
            ((elib:audited-by-magic-verb) (destructuring-bind (auditor) ,args-sym
              (not (not (find auditor ,stamps-sym :test #'eeq-is-same-ever)))))
            (otherwise 
              (elib:miranda ,self-expr ,mverb-sym ,args-sym (lambda ()
                ,(let ((fallthrough
                        `(error "no such method: ~A#~A" 
                                ',fqn ,mverb-sym)))
                  (if opt-otherwise-body
                    (vtable-method-body opt-otherwise-body `(cons ,mverb-sym ,args-sym) '() :type-name fqn)
                    fallthrough))))))))
          ,self-expr)))
          
  ) ; end eval-when
  

(defmacro efun (method-first &body method-rest)
  (e-lambda-expansion `((:|run| ,method-first ,@method-rest)) (gen-fqn) "" '() nil))


(defmacro e-lambda (fqn (&rest options) &body entries)
  "XXX document this

fqn may be NIL or a string."
  (let ((stamp-forms '())
        (opt-otherwise-body nil)
        (documentation ""))
    ;; Normalize/check simple options
    (if fqn
      (check-type fqn string)
      (setf fqn (gen-fqn)))
    ;; Parse options
    (loop for (key value) on options by #'cddr do
      (case key
        (:stamped
          (push value stamp-forms))
        (:doc
          (assert (string= documentation "") (documentation) "duplicate :doc option")
          (setf documentation value))
        (otherwise
          (error "Unrecognized option ~S ~S in e-lambda ~S."
                 key value fqn))))
    (check-type documentation string)
    ;; Extract 'otherwise' method
    (setf entries
      (remove-if
        (lambda (entry)
          (when (and (listp entry) (eql (first entry) 'otherwise))
            (assert (null opt-otherwise-body))
            (setf opt-otherwise-body (rest entry))
            t))
        entries))
    ;; Expansion
    (e-lambda-expansion entries fqn documentation stamp-forms opt-otherwise-body)))
  
; --- Miranda methods ---

; XXX merge def-miranda and the-miranda-method-case into one macro
(defmacro the-miranda-method-case (type-designator verb-list-sym (mverb-form args-form) &body entries)
  (let* ((type-name (string type-designator)))
    `(let ((mverb ,mverb-form)
           (args ,args-form)
           (,verb-list-sym ',(mapcar #'(lambda (entry) (vtable-entry-mverb entry 0)) entries)))
      (case mverb
        ,@(loop for desc in entries 
                collect (if (eql (car desc) 'otherwise)
                           `(otherwise (funcall (named-lambda ,(make-symbol (format nil "~A#<match>" type-name)) ,@(cdr desc)) mverb args))
                           (vtable-case-entry desc 'args '() :type-name type-name)))))))

(defmacro def-miranda (func-name descs-name fqn-designator (mverb-litem args-litem) &body entries)
  "The reason for this macro's existence is to allow compiling the miranda-methods into a single function and simultaneously capturing the method names and comments for alleged-type purposes. I realize this is ugly, and I'd be glad to hear of better solutions."
  `(progn
    (setf ,descs-name 
      ',(loop for entry in entries
              when (not (eql (first entry) 'otherwise))
              collect (vtable-entry-message-desc entry)))
    (defun ,func-name (self ,mverb-litem ,args-litem else-func)
      (the-miranda-method-case ,fqn-designator miranda-mverbs (,mverb-litem ,args-litem)
        ,@entries))))
 
;(declaim (inline miranda))
(def-miranda miranda +miranda-message-descs+ "MirandaMethods" (mverb args)
  
    (:|__printOn| (tw)
      ; FUNCTION-based E-objects must always implement their own __printOn/1.
      (assert (not (typep self 'function)))
      (e-coercef tw +the-text-writer-guard+)
      (e. tw |print| "<" (e-util:aan (simplify-fq-name (cl-type-fq-name (observable-type-of self)))) ">"))

    (:|__getAllegedType| ()
      ; FUNCTION-based E-objects must always implement their own __getAllegedType/1.
      (assert (not (typep self 'function)))
      (scan-example-for-vtable-message-types self)
      (make-instance 'cl-type-guard :type-specifier (observable-type-of self)))

    (:|__respondsTo| (verb arity)
      ; The object itself must handle returning true for those verbs which it implements.
      (as-e-boolean (member (e-util:mangle-verb verb arity) miranda-mverbs)))

    (:|__conformTo| (guard)
      (declare (ignore guard))
      self)

    (:|__optSealedDispatch| (brand)
      (declare (ignore brand))
      nil)

    (:|__optUncall| () nil)

    (:|__whenMoreResolved| (reactor)
      (e<- reactor |run| self)
      nil)

    (:|__whenBroken| (reactor)
      (declare (ignore reactor))
      nil)

    (:|__order| (nested-verb nested-args)
      ; XXX document further (original doc is Mozilla-licensed)
      "Returns a tuple of the result of immediately calling this.<nested-verb>(<nested-args>*) and this."
      (vector (e-call self nested-verb nested-args) self))

    (:|__reactToLostClient| (problem)
      (declare (ignore problem))
      nil)

    (:|__getPropertySlot| (prop-name)
      (e-coercef prop-name 'string)
      ; XXX should we have a fqn derived from the original object? a print derived from its simple name?
      (let* ((cap-name (string-upcase prop-name :end (min 1 (length prop-name))))
             (set-name (concatenate 'string "set" cap-name))
             (get-verb (e-util:mangle-verb (concatenate 'string "get" cap-name) 0))
             (set-verb (e-util:mangle-verb (concatenate 'string "set" cap-name) 1)))
      (e-lambda "org.cubik.cle.prim.DefaultPropertySlot"
          (:doc "This is a Slot acting as a facet on the `get$Property` and `set$Property` methods of another object.")
        (:|__printOn| (tw)
          (e-coercef tw +the-text-writer-guard+)
          (e. e.syntax:+e-printer+ |printPropertySlot| tw prop-name))
        (:|getValue| ()
          "E.call(target, `get$Property`)"
          (e-call-dispatch self get-verb))
        (:|setValue| (new)
          "E.call(target, `set$Property`, new); null"
          (e-call-dispatch self set-verb new)
          nil)
        (:|isFinal| ()
          (e. (e-coerce (e. self |__respondsTo| set-name 1) 'e-boolean) |not|)))))

    (otherwise (mverb args)
      (declare (ignore mverb args))
      (funcall else-func)))

; --- Native object cross-calling support ---

#+(or e.vtable-collect.use-example e.vtable-collect.use-mop)
(progn
  (defgeneric vtable-local-message-types (specializer)
    (:documentation "Returns a list of #(mverb message-desc) tuples for the given class, but not its superclasses."))

  (defmethod vtable-local-message-types ((specializer t))
    (declare (ignore specializer))
    '()))

#+e.vtable-collect.use-example
(progn
  (defgeneric vtable-collect-message-types (instance narrowest-type)
    (:documentation "Returns a list of #(mverb message-desc) tuples for the given instance, except those for <xxx explain narrowest-type>"))

  (defmethod vtable-collect-message-types ((instance t) narrowest-type)
    (declare (ignore instance narrowest-type))
    '()))

#+e.vtable-collect.use-typelist 
(defvar *types-with-vtables* '())

#+(or e.vtable-collect.use-example e.vtable-collect.use-typelist)
(defvar *vtable-message-types-cache* (make-hash-table :test #'equal)
  "Since Common Lisp specifies no way to get the superclasses (or class prededence list) of a class, this hash table stores, for a given class-name, the result of vtable-collect-message-types on the first instance of that class.")

#+(or e.vtable-collect.use-example e.vtable-collect.use-typelist)
(defun vtable-message-types (type)
  (or (gethash type *vtable-message-types-cache*)
      #-e.vtable-collect.use-typelist 
      (error "oops: no example of ~S seen yet for alleged type purposes" type)
      #+e.vtable-collect.use-typelist 
      (setf (gethash type *vtable-message-types-cache*)
        ; xxx this is wrong: it doesn't necessarily return entries in precedence-list order
        (loop for vtype in *types-with-vtables*
              when (subtypep type vtype)
              append (vtable-local-message-types type)))))

#+e.vtable-collect.use-mop
(defun vtable-message-types (type)
  ; Note that we reverse the class precedence list, because the caller of this stuffs it into non-strict ConstMap construction, and so takes the *last* seen vtable entry as the overriding one. xxx perhaps this subtle dependency should be made more robust/explicit?
  (loop for superclass in (reverse (e-util:class-precedence-list (find-class type)))
    append (vtable-local-message-types (class-name superclass))))

#-e.vtable-collect.use-example
(defun scan-example-for-vtable-message-types (instance)
  (declare (ignore instance)))

#+e.vtable-collect.use-example
(defun scan-example-for-vtable-message-types (instance
    &aux (type (observable-type-of instance)))
  (unless (gethash type *vtable-message-types-cache*)
    (setf (gethash type *vtable-message-types-cache*)
      (vtable-collect-message-types instance type))))


(defmacro def-vtable (type-spec &body entries)
  `(progn
    #+(or e.vtable-collect.use-example e.vtable-collect.use-typelist)
    (assert (not (gethash ',type-spec *vtable-message-types-cache*)))
    #+e.vtable-collect.use-typelist 
    (pushnew ',type-spec *types-with-vtables*)
  
    #+(or e.vtable-collect.use-example e.vtable-collect.use-mop)
    (defmethod vtable-local-message-types ((type-sym (eql ',type-spec)))
      (list ,@(loop for entry in entries collect
        (vtable-entry-message-desc-pair entry :prefix-arity 1))))
    
    #+e.vtable-collect.use-example
    (defmethod vtable-collect-message-types ((specimen ,type-spec) narrowest-type)
      (if (subtypep ',type-spec narrowest-type)
        (list* ,@(mapcar #'(lambda (entry) (vtable-entry-message-desc-pair entry :prefix-arity 1)) entries)
          (call-next-method))
        (call-next-method)))
    
    ; XXX gensymify 'args
    (defmethod e-call-dispatch ((rec ,type-spec) (mverb symbol) &rest args)
      (case mverb
        ,@(loop for desc in entries collect
            ; :type-name is purely a debugging hint, not visible at the E level, so it's OK that it might reveal primitive-type information
            (vtable-case-entry desc 'args `(rec) :type-name (prin1-to-string type-spec)))
        ((:|__respondsTo/2|) (destructuring-bind (verb arity) args
          (as-e-boolean (or
            (member (e-util:mangle-verb verb arity) 
                    ',(mapcar (lambda (entry) (vtable-entry-mverb entry 1)) 
                              entries))
            (e-is-true (call-next-method))))))
  
        (otherwise (call-next-method))))))

; xxx should we have observable-class-of instead? probably not, since we use type specifiers everywhere
(defgeneric observable-type-of (specimen)
  (:documentation "Given an object, return the narrowest type specifier which should visible (in its FQN form) to the E programmer. Usually specialized via the def-class-opaque macro."))

(defmacro def-class-opaque (class-name &optional (visible-type class-name))
  `(defmethod observable-type-of ((a ,class-name))
    (declare (ignore a))
    ',visible-type))

; xxx it might be useful to enforce the constraint that anything returning t from eeq-is-transparent-selfless must have a specialized observable-type-of, or something to that effect.
;
; Or, perhaps, the observable-type-of anything transparent-selfless should be derived from its uncall's maker somehow.

;(declaim (ftype (function (t) string) cl-type-fq-name cl-type-parameters))

(defgeneric cl-type-fq-name (type-sym)
  (:documentation  "Given a type specifier, return the corresponding E fully-qualified-name. Usually specialized via the def-fqn macro."))

(defgeneric cl-type-parameters (type-sym parameters))

(defmacro def-fqn (type fqn)
  (assert (typep type 'symbol))
  `(defmethod cl-type-fq-name ((type (eql ',type)))
    ',fqn))

; --- Primitive stamps ---

; These must? be defined early, since any (defvar +the-whatever+ (e-lambda :stamped +deep-frozen-stamp+ ...)) will cause evaluation of +deep-frozen-stamp+ at the time of execution of the defvar.

(defvar +deep-frozen-stamp+ (e-lambda 
    "org.erights.e.elib.serial.DeepFrozenStamp"
    (:doc "The primitive rubber-stamping auditor for DeepFrozen-by-fiat objects.
  
While this is a process-wide object, its stamps should not be taken as significant outside of the vats of the objects stamped by it.")
  ; NOTE: Eventually, deep-frozen-stamp & thread-and-process-wide-semantics-safe-stamp => directly sharable across threads, like Java-E does with all Java-PassByCopy.
  (:|audit| (object-expr witness)
    ; XXX audit/2 is an E-language-dependent interface, but we must implement it to allow E code to employ these stamps. Should we then define a more general interface? 'audit(language, expr, witness)'? Same questions apply to other primitive stamps.
    (declare (ignore object-expr witness))
    +e-true+)))

(defvar +selfless-stamp+ (e-lambda
    "org.erights.e.elib.serial.SelflessStamp"
    (:doc "The primitive rubber-stamping auditor for Frozen-and-Transparent-and-Selfless objects, whose uncalls are used in sameness tests.
  
While this is a process-wide object, its stamps should not be taken as significant outside of the vats of the objects stamped by it."
     :stamped +deep-frozen-stamp+)
  (:|audit| (object-expr witness)
    (declare (ignore object-expr witness))
    +e-true+)))

(defmethod e-audit-check-dispatch ((auditor t) (specimen (eql +selfless-stamp+)))
  "Prevents an infinite recursion:
      (eeq-is-transparent-selfless some-obj)
   -> (e-audit-check-dispatch +selfless-stamp+ some-obj)
   -> (eeq-is-same-ever +selfless-stamp+ some-approver-of-obj)
   -> (eeq-is-transparent-selfless +selfless-stamp+)
   -> (e-audit-check-dispatch +selfless-stamp+ +selfless-stamp+)
   -> (eeq-is-same-ever +selfless-stamp+ +deep-frozen-stamp+)
   -> repeat with +selfless-stamp+ in place of some-obj
     
Since we know the SelflessStamp is not itself selfless, we can shortcut the selfless check to not involve equalizer operations.

This could instead be defined as an eeq-is-transparent-selfless method, but I expect to remove that generic function eventually to reduce the number of kinds of 'dispatch on e-ref'. -- kpreid 2005-03-16"
  (if (eql (ref-shorten auditor) +selfless-stamp+)
    nil
    (call-next-method)))

; --- utilities referenced below ---

(declaim (inline e-coerce))
(defun e-coerce (object result-type &optional ejector)
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note)) ; due to inlined etypecase
  (typecase result-type
    ((or symbol cons)            ; type specifier
      (e-coerce-native object result-type ejector))
    (otherwise                   ; Guard
      (e. result-type |coerce| object ejector))))

(defun eject-or-ethrow (ejector condition)
  (setf condition (e-problem-to-condition condition))
  (e-coercef condition +the-exception-guard+)
  (if ejector
    (let ((r (e. ejector |run| condition)))
      (error "optEjector ~A returned: ~A" (e-quote ejector) (e-quote r)))
    (error condition)))

(defun e-quote (o)
  (with-text-writer-to-string (tw)
    (e. tw |quote| o)))

(defun e-print (o)
  (with-text-writer-to-string (tw)
    (e. tw |print| o)))

; NOTE: this macro has the quirk that you can't (declare (ignore ,result-var)) because it's outside the form which the body is evaluated. this could be fixed by making the progn instead (let ((,result-var ,result-var)) ,@forms)
(defmacro with-result-promise ((result-var) &body forms
    &aux (resolver-var (gensym "RESULT-RESOLVER"))
         (value-var    (gensym "VALUE")))
  "Evaluate the forms as an implicit progn, binding result-var to an E promise-ref which refers to the result of the forms. Returns that result (not the resolved promise).
  
In the event of a nonlocal exit, the promise will currently remain unresolved, but this macro may be revised to smash it instead."
  `(multiple-value-bind (,result-var ,resolver-var) (make-promise)
    (let ((,value-var (progn ,@forms)))
      (e. ,resolver-var |resolve| ,value-var)
      ,value-var)))

; --- slots ---

(defvar +the-make-simple-slot+ (e-lambda "org.erights.e.elib.slot.makeFinalSlot"
    (:stamped +deep-frozen-stamp+)
  (:|run| (value)
    (make-instance 'e-simple-slot :value value))))

(defclass e-simple-slot () 
  ((value :initarg :value))
  (:documentation "A normal immutable slot."))

(defmethod print-object ((slot e-simple-slot) stream)
  ; xxx make readable under read-eval?
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "& ~W" (slot-value slot 'value))))

(def-vtable e-simple-slot
  (:|__optUncall| (this)
    `#(,+the-make-simple-slot+ "run" #(,(slot-value this 'value))))
  (:|__printOn| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<& ")
    (e. tw |quote| (slot-value this 'value))
    (e. tw |print| ">")
    nil)
  (:|getValue| (this)
    "Returns the constant value of this slot."
    (slot-value this 'value))
  (:|setValue| (this new-value)
    "Always fails."
    (declare (ignore new-value))
    (error "not an assignable slot: ~A" (e-quote this)))
  (:|isFinal| (this)
    "Returns true."
    (declare (ignore this))
    +e-true+))

(defmethod eeq-is-transparent-selfless ((a e-simple-slot))
  (declare (ignore a)) t)
  

(defclass e-unset-slot () ())

(def-vtable e-unset-slot
  (:|getValue| (this)
    (declare (ignore this))
    (error "internal error: slot variable never assigned"))
  (:|setValue| (this new-value)
    (declare (ignore this new-value))
    (error "internal error: slot variable never assigned")))

(defvar +the-unset-slot+ (make-instance 'e-unset-slot))


(defclass e-var-slot (vat-checking) 
  ((value :initarg :value)))

(def-vtable e-var-slot
  (:|__printOn| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<var ")
    (e. tw |quote| (slot-value this 'value))
    (e. tw |print| ">"))
  (:|getValue| (this)
    (slot-value this 'value))
  (:|setValue| (this new-value)
    (setf (slot-value this 'value) new-value)
    nil)
  (:|isFinal| (this)
    (declare (ignore this))
    +e-false+))

(defmethod print-object ((slot e-var-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W" (slot-value slot 'value))))

(defclass e-guarded-slot (vat-checking) 
  ((value :initarg :value)
   (guard :initarg :guard)))

(defmethod shared-initialize :after ((slot e-guarded-slot) slot-names &key opt-ejector &allow-other-keys)
  (declare (ignore slot-names))
  (with-slots (guard value) slot
    (setf value (e. guard |coerce| value opt-ejector))))

(def-vtable e-guarded-slot
  (:|__printOn| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (with-slots (guard value) this
      (e. tw |print| "<var ")
      (e. tw |quote| value)
      (e. tw |print| " :")
      (e. tw |quote| guard)
      (e. tw |print| ">")))
  (:|getValue| (this)
    (slot-value this 'value))
  (:|setValue| (this new-value)
    (with-slots (guard value) this
      (setf value (e. guard |coerce| new-value nil)))
    nil)
  (:|isFinal| (this)
    (declare (ignore this))
    +e-false+))

(defmethod print-object ((slot e-guarded-slot) stream)
  (print-unreadable-object (slot stream :type nil :identity nil)
    (format stream "var & ~W :~W" (slot-value slot 'value) (slot-value slot 'guard))))

; --- guards ---

(locally
  (declare (optimize (speed 3) (space 3)))
  (defvar +the-void-guard+ (e-lambda "org.erights.e.elib.slot.VoidGuard"
      (:stamped +deep-frozen-stamp+)
    (:|__printOn| (tw) ; XXX move to e.syntax?
      (e-coercef tw +the-text-writer-guard+)
      (e. tw |print| "void"))
    (:|coerce| (specimen opt-ejector)
      (declare (ignore specimen opt-ejector))
      nil))))

; Simple native-type Guards
(defclass cl-type-guard () 
  ((ts :initarg :type-specifier
       :reader cl-type-specifier)
   (super :initform nil)
   (trivial-box :initform nil)))

(declaim (inline standard-coerce))
(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defun standard-coerce (test conform-guard-thunk error-thunk &key (test-shortened t))
    "Typical guard coercion. Returns a function which returns the first of these which passes the test, or ejects the result of error-thunk via opt-ejector: specimen, ref-shortened specimen, ref-shortened result of specimen.__conformTo(conform-guard-thunk.run()).

If returning an unshortened reference is acceptable and the test doesn't behave differently on unshortened references, specify :test-shortened nil for an optimization."
    (declare (optimize (speed 3) (space 3)))
    (lambda (long-specimen opt-ejector)
      (labels ((fail ()
                (eject-or-ethrow opt-ejector
                  (funcall error-thunk long-specimen))))
        (if (funcall test long-specimen)
          long-specimen
          (let ((specimen (ref-shorten long-specimen)))
            ; We shorten here even if test-shortened is false, because
            ; if we didn't we'd pay for shortening twice in ref-state 
            ; and the __conformTo call.
            (cond
              ((and test-shortened (funcall test specimen))
                specimen)
              ((not (eql (ref-state specimen) 'near))
                ; avoid synchronous-call errors from __conformTo
                (fail))
              (t
                (let ((coerced (ref-shorten 
                                 (e. specimen |__conformTo| 
                                   (funcall conform-guard-thunk)))))
                  (if (funcall test coerced)
                    coerced
                    (fail)))))))))))

(declaim (ftype (function (t t &optional t cl-type-guard) t) e-coerce-native))
(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defun e-coerce-native (long-specimen type &optional opt-ejector opt-guard)
    (declare (optimize (speed 3) (space 3)))
    (funcall (standard-coerce #'(lambda (specimen) (typep specimen type))
                              #'(lambda () (or opt-guard
                                             (make-instance 'cl-type-guard
                                               :type-specifier type)))
                              #'(lambda (specimen) (make-condition 'type-error
                                                     :datum specimen 
                                                     :expected-type type)))
             long-specimen
             opt-ejector)))


; XXX move these to more specific locations
(def-class-opaque string)
(def-class-opaque integer)
(def-class-opaque vector)
(def-class-opaque character)

; XXX thread-safety: make these all vat-local or remove super arg
(defvar +the-any-guard+    (make-instance 'cl-type-guard :type-specifier 't))
(defvar +the-nullok-guard+ (make-instance 'cl-type-guard :type-specifier 'null))
(defvar +the-exception-guard+ (make-instance 'cl-type-guard :type-specifier 'condition))

; --- Boolean ---

; xxx should e-boolean be made of two e-lambdas or symbols instead?

(defclass e-boolean ()
  ((value :initarg :value
          :type boolean)))
          
(defvar +e-false+ (make-instance 'e-boolean :value nil))
(defvar +e-true+  (make-instance 'e-boolean :value t))

(declaim (ftype (function (t) (member t nil)) e-is-true)
         (inline e-is-true))
(defun e-is-true (bool)
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  ; this cond can't be a case because the e-booleans aren't constants at compile time
  (cond
    ((eq bool +e-false+) nil)
    ((eq bool +e-true+)  t)
    (t 
      (eq (e-coerce bool 'e-boolean) +e-true+))))

(declaim (ftype (function (t) e-boolean) as-e-boolean)
         (inline as-e-boolean))
(defun as-e-boolean (x) 
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (if x
    +e-true+
    +e-false+))

(defmethod print-object ((bool e-boolean) stream)
  (format stream "#.+e-~A+" (if (slot-value bool 'value) "true" "false")))

; --- Ejector ---

(defun ejector-prethrow (ejector-spec value)
  "Implements elib:*break-on-ejections*, analogously to cl:*break-on-signals*."
  (when (typep value *break-on-ejections*)
    (break "About to exit via ~W with value ~W" ejector-spec value))
  nil)

(defclass ejector (vat-checking)
  ((catch-tag :initarg :catch-tag
              :initform (error "attempted to make ejector with no catch-tag"))
   (label     :initarg :label     
              :initform nil
              :type (or null string))))

; eject-or-ethrow found above

; --- Equalizer ---

; XXX OPT cache equalizer per-thread (in vat?)
(defun eeq-is-same-yet     (a b)
  (e-is-true (e. (make-equalizer) |sameYet| a b)))
(defun eeq-is-same-ever    (a b)
  (e-is-true (e. (make-equalizer) |sameEver| a b)))

(defun eeq-is-settled (a)
  (eeq-sameness-fringe a nil))

