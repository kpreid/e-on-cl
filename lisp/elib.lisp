; This Common Lisp code is Copyright 2005-2006 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.elib)

(eval-when (:compile-toplevel)
  (declaim (optimize (safety 3))))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  ; XXX tests for this
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
          result))))
  
  (defun join-fq-name (prefix qname)
    "Return 'qname' if it has the format of a fully-qualified name, or 'qname' joined with 'prefix' otherwise."
    (cond
      ((and (string/= qname "") (string= qname "$" :end1 1))
        (concatenate 'string prefix (subseq qname 1)))
      (t
        ; XXX the error case comes from Java-E structure (separated isFQName). we should find out why Java-E bothers separating it, and decide whether to use the same structure here
        qname
        #-(and) (error "Unrecognized qualified name: ~A (being qualified with prefix ~A)" (e-quote qname) (e-quote prefix))))))

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


(defgeneric e-call-dispatch (rec mverb &rest args)
  (:documentation "Main dispatch function for E calls and other call-like operations.

Shortens REC if needed. MVERB must be either a mangled verb as in E.UTIL:MANGLE-VERB, or a non-keyword symbol that is a magic verb (such as AUDITED-BY-MAGIC-VERB); if a mangled verb, its arity must be equal to the number of ARGS.

Implementations must REF-SHORTEN the ARGS if they want shortened arguments, and not expose non-keyword mverbs to untrusted code. XXX This list of requirements is probably not complete.")
  (declare (optimize (speed 3))))

(defgeneric e-call-match (rec mverb &rest args)
  (:documentation "The last method of E-CALL-DISPATCH calls this method with the same arguments, if MVERB is not a Miranda method. This is used for implementing 'matchers'/inheritance.")
  (declare (optimize (speed 3))))

(defgeneric e-send-dispatch (rec mverb &rest args)
  (:documentation "Used only for REFs' various behaviors on sends; should not be specialized for non-REFs.")
  (declare (optimize (speed 3))))

(defgeneric eeq-hash-dispatch (a))
(defgeneric eeq-same-dispatch (left right))

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

; --- defining some vat pieces early so that vat-checking does not come after local-resolver which subclasses it, which ABCL doesn't like ---

(defvar *vat* nil)
(defvar *runner* nil)

(defclass vat-checking () 
  ((vat-checking-expected-vat :type vat))
  (:documentation "Remembers the current vat and checks that it is correct upon later opportunities (currently e-call-dispatch). Should not be relied upon, as it may be made a noop in an optimized mode.

If there is no current vat at initialization time, captures the current vat at the next check time, so that vat-checking instances may be used during pre-vat-creation setup code."))

(defmethod shared-initialize ((this vat-checking) slot-names &key &allow-other-keys)
  (when *vat*
    (setf (slot-value this 'vat-checking-expected-vat) *vat*))
  (call-next-method))

(defmethod e-call-dispatch :before ((rec vat-checking) mverb &rest args)
  (declare (ignore mverb args))
  (with-slots ((expected-vat vat-checking-expected-vat)) rec
    (if (slot-boundp rec 'vat-checking-expected-vat)
      (assert (eq *vat* expected-vat))
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
  (error 'synchronous-call-error :recipient ref :mverb mverb :args args))

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
(defun broken-ref-magic (mverb args)
  (when (member mverb '(:|__whenMoreResolved/1| :|__whenBroken/1|))
    (e<- (first args) |run|))
  nil)

(defmethod e-call-dispatch ((ref broken-ref) mverb &rest args)
  (broken-ref-magic mverb args)
  (error (ref-opt-problem ref)))

(defmethod e-send-dispatch ((ref broken-ref) mverb &rest args)
  (broken-ref-magic mverb args)
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

(defun call-with-turn (body vat &key exclude-io (label t))
  "Execute the 'body' thunk, as a turn in the given vat."
  (assert label)
  (when (vat-in-turn vat)
    (error "~S is already executing a turn, ~S, so turn ~S may not execute" vat (vat-in-turn vat) label))
  (unless (or (null *vat*)
              (eq *vat* vat))
    (error "there is already a current vat, ~S, so ~S may not execute a turn" *vat* vat))
  (unwind-protect
    (let ((*vat* vat))
      (setf (vat-in-turn vat) label)
      (if (not exclude-io)
        (funcall body)
        (call-with-io-handler-exclusion 
          body 
          (handler-exclusion-group (vat-runner vat)) 
          `(turn-in ,vat))))
    (setf (vat-in-turn vat) nil)))

(defmacro with-turn ((&rest args) &body body)
  "Execute the body, as a turn in the given vat."
  `(call-with-turn (lambda () ,@body) ,@args))

(defclass runner ()
  ((label :initarg :label
          :initform nil
          :type (or null string)
          :reader label)
   (sends :type queue
          :initform (make-instance 'queue))
   (time-queue :type sorted-queue
               :initform (make-instance 'sorted-queue))
   (handler-group :type io-handler-exclusion-group
                  :initform (make-instance 'io-handler-exclusion-group)
                  :reader handler-exclusion-group)))

(defclass vat ()
  ((runner :initarg :runner
           :initform (error "no runner specified in vat creation")
           :accessor vat-runner
           :type runner)
   (in-turn :initform nil
            :accessor vat-in-turn
            :type boolean
            :documentation "Whether some type of top-level turn is currently being executed in this vat. Used for consistency checking.")
   (safe-scope :initform (e.knot:make-safe-scope)
               :accessor vat-safe-scope)
   (label :initarg :label
          :initform nil
          :type (or null string)
          :reader label)))

(defmethod print-object ((vat vat) stream)
  (print-unreadable-object (vat stream :type t :identity t)
    (format stream "~S~:[~; (in turn)~]"
      (label vat)
      (vat-in-turn vat))))

(defgeneric enqueue-turn (context function))

(defmethod enqueue-turn ((runner runner) function)
  ; xxx threading: either this acquires a lock or the queue is rewritten thread-safe
  (enqueue (slot-value runner 'sends) function))

(defmethod enqueue-turn ((vat vat) function)
  (enqueue-turn (vat-runner vat)
                (lambda ()
                  (call-with-turn function vat :exclude-io t))))

(defgeneric vr-add-io-handler (context target direction function))

(defmethod vr-add-io-handler ((runner runner) target direction function)
  (add-exclusive-io-handler (handler-exclusion-group runner)
                            (ref-shorten target)
                            (ref-shorten direction)
                            function))

(defmethod vr-add-io-handler ((vat vat) target direction function)
  (vr-add-io-handler (vat-runner vat)
                     target
                     direction
                     (lambda (target*)
                       (assert (eql target target*) () "buh?")
                       (with-turn (vat :exclude-io nil) 
                         (funcall (ref-shorten function) target*)))))

(defun vr-remove-io-handler (handler)
  (remove-exclusive-io-handler (ref-shorten handler)))

(defmethod e-send-dispatch (rec mverb &rest args)
  (assert (eq (ref-state rec) 'near) () "inconsistency: e-send-dispatch default case was called with a non-NEAR receiver")
  (multiple-value-bind (promise resolver) (make-promise)
    (enqueue-turn *vat* (lambda ()
      ; XXX direct this into a configurable tracing system once we have one
      ;(format *trace-output* "~&; running ~A ~A <- ~A ~A~%" (e. (e. rec |__getAllegedType|) |getFQName|) (e-quote rec) (symbol-name mverb) (e-quote (coerce args 'vector)))
      (e. resolver |resolve| 
        (handler-case 
          (apply #'e-call-dispatch rec mverb args)
          (error (p)
            (format *trace-output* "~&; problem in send ~A <- ~A ~A: ~A~%" (e-quote rec) (symbol-name mverb) (e-quote (coerce args 'vector)) p)
            (make-unconnected-ref (transform-condition-for-e-catch p)))))))
    promise))

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

(defun runner-loop (runner)
  (with-slots (sends time-queue) runner
    (loop
      (if (queue-null sends)
        (serve-event-with-time-queue time-queue sends)
        (progn
          (funcall (dequeue sends))
          (serve-event-with-time-queue time-queue sends 0))))))

(defgeneric enqueue-timed (context time func))

(defmethod enqueue-timed ((runner runner) time func)
  (with-slots (time-queue) runner
    (sorted-queue-put time-queue time func)))

(defmethod enqueue-timed ((vat vat) time func)
  (enqueue-timed (vat-runner vat) time (lambda ()
                                         (call-with-turn func vat))))

(defun establish-vat (&rest initargs)
  (assert (null *vat*))
  (assert (null *runner*))
  (setf *runner* (make-instance 'runner))
  (setf *vat* (apply #'make-instance 'vat :runner *runner* initargs)))

(defun top-loop ()
  (runner-loop *runner*))

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
  
  (defglobals +miranda-message-descs+)
  
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
      (coerce (reverse out) 'vector))))

; --- E objects ---

; A built-for-E object is a FUNCTION expecting (mverb &rest args). It handles its own dispatch, match, and no-such-method.

; These must be defined early as some e-calls to function objects are performed in later loading.
(defmethod e-call-dispatch ((recip function) mverb &rest args)
  (apply recip mverb args))


(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun partition (test sequence &rest options)
    "Return, as two values, the elements of SEQUENCE satisfying the test and those not satisfying the test. Accepts all the options REMOVE-IF[-NOT] does."
    ;; http://paste.lisp.org/display/10050
    (let (not)
      (values
        (apply #'remove-if-not 
               #'(lambda (element)
                 (not
                   (unless (funcall test element) 
                     (push element not))))
               sequence
               options)
        (nreverse not))))
  
  (defun mangled-verb-p (thing)
    (and (typep thing 'keyword)
         (find #\/ (symbol-name thing))))
  
  (defun smethod-mverb (smethod prefix-arity
      &aux (verb-or-mverb (first smethod)))
    (etypecase verb-or-mverb
      ((and symbol (or (not keyword) (satisfies mangled-verb-p)))
        verb-or-mverb)
      (keyword
        (let ((verb-string (symbol-name verb-or-mverb)))
          (e.util:mangle-verb
            verb-string
            (multiple-value-bind (min max)
                (progn
                  (assert (cddr smethod) () "Inferring arity for function smethod not supported: ~S" smethod)
                  (e.util:lambda-list-arguments-range (second smethod)))
              (assert (>= min prefix-arity) () "Method ~S has ~S parameters, which is not enough to accept ~S prefix argument~:P." verb-string min prefix-arity)
              (assert (= min max) () "Variable arguments not yet supported for smethod-case-entry arity inference")
              (- min prefix-arity)))))))

  (defun smethod-case-entry (smethod args-sym prefix-args &key type-name
      &aux (mverb (smethod-mverb smethod (length prefix-args))))
    "Return a CASE clause whose key is a mangled-verb, suitable for implementing method dispatch for E objects.

The first element of the smethod is a string designator for the verb. If it contains a slash, it is treated as a mangled-verb (verb/arity); if not, the second element must be a lambda list, which is used to determine the number of arguments.

If there is exactly one further element, then it is an expression (typically #'...) which the returned clause will use in `(apply ,expr ,args-sym).

If there are two or more further elements, then they are a lambda list and implicit-progn forms which will be evaluated.

prefix-args is a list of forms which will be prepended to the arguments of the method body."
    (assert (not (eq (first smethod) 'otherwise)))
    `((,mverb) 
      ,(smethod-body (rest smethod) args-sym prefix-args :type-name type-name :verb-name mverb)))
  
  (defun smethod-function-case-entry (smethod prefix-arity &key type-name
      &aux (mverb (smethod-mverb smethod prefix-arity)))
    (assert (not (eq (first smethod) 'otherwise)))
    `((,mverb)
      ,(smethod-function-form (rest smethod) :type-name type-name :verb-name mverb)))

  (defun smethod-body (body args-sym prefix-args &key type-name verb-name)
    `(apply ,(smethod-function-form body :type-name type-name :verb-name verb-name) ,@prefix-args ,args-sym))
  
  (defun smethod-function-form (body &key type-name verb-name)
    "XXX transfer the relevant portions of smethod-case-entry's documentation here"
    (if (= 1 (length body))
      (first body)
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
        `(named-lambda ,name-sym ,@body))))

  (defun lambda-list-to-param-desc-vector (list arity prefix-arity
      &aux (end (or (position '&aux list) (length list))))
    (unless (= end (+ arity prefix-arity))
      (error "arity ~A + prefix-arity ~A doesn't match lambda list ~S" arity prefix-arity list))
    (coerce (loop 
      for i below arity
      for sym in (nthcdr prefix-arity list)
      do
        (when (member sym lambda-list-keywords)
          (error "constructing param-desc vector from lambda list ~S with keyword ~s not possible" list sym))
      collect 
        (make-instance 'param-desc :opt-name
          (with-standard-io-syntax
            (let ((*print-case* :downcase)
                  (*package* (or (symbol-package sym) (find-package :cl))))
              (write-to-string sym))))) 'vector))
                            
  (defun smethod-message-desc-pair (smethod &rest keys &key (prefix-arity 0) &allow-other-keys
      &aux (mverb (smethod-mverb smethod prefix-arity)))
    (vector (symbol-name mverb) 
            (apply #'smethod-message-desc smethod keys)))
  
  (defun smethod-message-desc (smethod &key (prefix-arity 0)
      &aux (mverb (smethod-mverb smethod prefix-arity))
           (impl-desc (rest smethod)))
    (assert (mangled-verb-p mverb))
    (multiple-value-bind (verb arity) (e-util:unmangle-verb mverb)
      (make-instance 'message-desc
        :verb verb
        :doc-comment (or (find-if #'stringp impl-desc) "") ; XXX too lenient
        :params (if (rest impl-desc) ; therefore inline
                  (lambda-list-to-param-desc-vector (first impl-desc) arity prefix-arity)
                  (make-array arity :initial-element 
                    (make-instance 'param-desc))))))
  
  (defun smethod-maybe-describe-fresh (function smethod &rest options)
    "messy."
    ;; XXX should be more like mverb-is-magic-p
    (if (typep (first smethod) '(and symbol (not keyword)))
      nil
      (list (apply function smethod options))))
  
  (defun nl-miranda-case-maybe (verb smethods &rest body)
    (unless (find verb smethods :key (lambda (e) (smethod-mverb e 0)))
      `(((,verb) ,@body))))

  (defmacro %fqn-prefix ()
    (format nil "~A$" (or *compile-file-truename* 
                          *load-truename* 
                          "<unknown-lisp>")))
  
  (defmacro nest-fq-name ((qname) &body body &environment env)
    `(macrolet ((%fqn-prefix () 
                 ,(join-fq-name (environment-fqn-prefix env)
                                (concatenate 'string qname "$"))))
      ,@body))
  
  (defun environment-fqn-prefix (env)
    (macroexpand '(%fqn-prefix) env))
    
  (defun e-lambda-expansion (smethods fqn documentation stamp-forms opt-otherwise-body
      &aux (self-func (make-symbol fqn))
           (self-expr `#',self-func)
           (stamps-sym (gensym "STAMPS"))
           (mverb-sym  (gensym "MVERB"))
           (args-sym   (gensym "ARGS")))
    (declare (string fqn documentation))
    `(let ((,stamps-sym (vector ,@stamp-forms)))
      (declare (ignorable ,stamps-sym)) ;; will be ignored if there is an explicit audited-by-magic-verb method
      (nest-fq-name (,fqn)
        (labels ((,self-func (,mverb-sym &rest ,args-sym)
          (case ,mverb-sym
            ,@(loop for smethod in smethods
                    collect (smethod-case-entry smethod args-sym '() :type-name fqn))
            ,@(nl-miranda-case-maybe :|__printOn/1| smethods `(destructuring-bind (tw) ,args-sym
              (e-coercef tw +the-text-writer-guard+)
              (e. tw |print| ',(format nil "<~A>" (simplify-fq-name fqn)))
              nil))
            ,@(nl-miranda-case-maybe :|__getAllegedType/0| smethods `(destructuring-bind () ,args-sym
              (make-instance 'type-desc
                :doc-comment ',documentation
                :fq-name ',fqn
                :supers #()
                :auditors #()
                :message-types-v
                  (or-miranda-message-descs
                    ',(mapcan (lambda (e) (smethod-maybe-describe-fresh #'smethod-message-desc e)) smethods)))))
            
            ,@(nl-miranda-case-maybe :|__respondsTo/2| smethods `(destructuring-bind (verb arity) ,args-sym
              (e-coercef verb 'string)
              (e-coercef arity '(integer 0))
              (as-e-boolean (or
                (member (e-util:mangle-verb verb arity) 
                        ',(mapcar (lambda (smethod) (smethod-mverb smethod 0)) 
                                  smethods))
                (e-is-true (elib:miranda ,self-expr ,mverb-sym ,args-sym nil))))))
            ,@(nl-miranda-case-maybe 'elib:audited-by-magic-verb smethods `(destructuring-bind (auditor) ,args-sym
              (not (not (find auditor ,stamps-sym :test #'eeq-is-same-ever)))))
            (otherwise 
              (elib:miranda ,self-expr ,mverb-sym ,args-sym (lambda ()
                ,(let ((fallthrough
                        `(no-such-method ,self-expr ,mverb-sym ,args-sym)))
                  (if opt-otherwise-body
                    (smethod-body opt-otherwise-body `(cons ,mverb-sym ,args-sym) '() :type-name fqn)
                    fallthrough))))))))
          ,self-expr))))
          
  ) ; end eval-when

(defmacro efun (method-first &body method-rest &environment env)
  (e-lambda-expansion `((:|run| ,method-first ,@method-rest)) (join-fq-name (environment-fqn-prefix env) "_") "" '() nil))


(defmacro e-lambda (qname (&rest options) &body smethods &environment env)
  "XXX document this

fqn may be NIL, a string, or a symbol, in which case the symbol is bound to the object itself."
  (check-type qname (or null string symbol))
  (when (typep qname '(and symbol (not null)))
    (return-from e-lambda 
      `(with-result-promise (,qname)
         (e-lambda ,(concatenate 'string "$" (symbol-name qname)) 
                   (,@options) ,@smethods))))
  (let ((fqn (join-fq-name (environment-fqn-prefix env) (or qname "_")))
        (stamp-forms '())
        (documentation ""))
    ;; Parse options
    (loop for (key value) on options by #'cddr do
      (case key
        (:stamped
          (when (find 'audited-by-magic-verb smethods :key (lambda (e) (smethod-mverb e 0)))
            ;; XXX make this test cleaner
            ;; XXX this test should be done in e-lambda-expansion instead of here, so that all forms of e-lambdas get it
            (error "Both ~S option and ~S specified in e-lambda ~S." :stamped 'audited-by-magic-verb fqn))
          (push value stamp-forms))
        (:doc
          (assert (string= documentation "") (documentation) "duplicate :doc option")
          (setf documentation value))
        (otherwise
          (error "Unrecognized option ~S ~S in e-lambda ~S."
                 key value fqn))))
    (check-type documentation string)
    ;; Extract 'otherwise' method
    (multiple-value-bind (otherwises specifics)
        (partition (lambda (x) (typep x '(cons (eql otherwise) t)))
                   smethods)
      (assert (<= (length otherwises) 1))
      ;; Expansion
      (e-lambda-expansion specifics fqn documentation stamp-forms (rest (first otherwises))))))
  
; --- Miranda methods ---

(defmacro def-miranda (func-name descs-name fqn (self-var mverb-litem args-litem verb-list-sym) &body smethods)
  "The reason for this macro's existence is to allow compiling the miranda-methods into a single function and simultaneously capturing the method names and comments for alleged-type purposes. I realize this is ugly, and I'd be glad to hear of better solutions."
  (multiple-value-bind (otherwises specifics) 
      (partition (lambda (x) (eq (first x) 'otherwise)) smethods)
    `(symbol-macrolet ((,verb-list-sym ',(mapcar #'(lambda (smethod) (smethod-mverb smethod 0)) smethods)))
      (defglobal ,descs-name 
        ',(mapcan (lambda (e) (smethod-maybe-describe-fresh #'smethod-message-desc e)) specifics))
      (defun ,func-name (,self-var ,mverb-litem ,args-litem else-func)
        (case ,mverb-litem
          ,@(loop for smethod in specifics collect
              (smethod-case-entry smethod args-litem '() :type-name fqn))
          ,@(loop for desc in otherwises collect
              `(otherwise (funcall (named-lambda ,(make-symbol (format nil "~A#<match>" fqn)) ,@(cdr desc)) ,mverb-litem ,args-litem))))))))
 
;(declaim (inline miranda))
(def-miranda miranda +miranda-message-descs+ "org.erights.e.elib.prim.MirandaMethods" (self mverb args miranda-mverbs)
  
    (audited-by-magic-verb (auditor)
      (declare (ignore auditor))
      nil)
  
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
      ;; The object itself must handle returning true for those verbs which it implements.
      (e-coercef verb 'string)
      (e-coercef arity '(integer 0))
      (as-e-boolean (member (e-util:mangle-verb verb arity) miranda-mverbs)))

    (:|__conformTo| (guard)
      (declare (ignore guard))
      self)

    (:|__optSealedDispatch| (brand)
      (declare (ignore brand))
      nil)

    (:|__optUncall| () nil)

    (:|__whenMoreResolved| (reactor)
      (e<- reactor |run|)
      nil)

    (:|__whenBroken| (reactor)
      (declare (ignore reactor))
      nil)

    (:|__order| (nested-verb nested-args)
      ; XXX document further (original doc is Mozilla-licensed)
      "Returns a tuple of the result of immediately calling this.<nested-verb>(<nested-args>*) and this."
      (vector (e-call self 
                      (e-coercef nested-verb 'string) 
                      (e-coercef nested-args 'vector)) 
              self))

    (:|__reactToLostClient| (problem)
      (declare (ignore problem))
      nil)

    (:|__getPropertySlot| (prop-name)
      (e-coercef prop-name 'string)
      ; XXX should we have a fqn derived from the original object? a print derived from its simple name?
      (let* ((cap-name (string-upcase prop-name :end (min 1 (length prop-name))))
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
        (:|isFinal| () +e-false+))))

    (otherwise (mverb args)
      (declare (ignore mverb args))
      (funcall else-func)))

; --- Native object cross-calling support ---

(defgeneric opt-local-method-function-for-class (class mverb)
  (:method ((class t) (mverb t)) nil))

(defvar *non-vtable-leaf-classes* (make-hash-table)
  "This hash table contains a true value for every class for which knowing that a value is an instance of that class is not sufficient to choose method implementations - that is, all classes which are superclasses of classes that have vtables defined, or classes of instances that have vtables defined via eql specializers.")

(defun opt-method-function-for-class (class mverb)
  "Given a class and an mverb, return a function which implements this method for instances of that class, or nil if either no such function is available or the type is not sufficiently specific to determine the exact method."
  (let ((function
         (loop for superclass in (e-util:class-precedence-list class)
            while (not (gethash superclass *non-vtable-leaf-classes*))
            thereis (opt-local-method-function-for-class superclass mverb))))
    (lambda (&rest args)
      (assert (not (gethash class *non-vtable-leaf-classes*)) (class))
      (apply function args))))

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
        ; xxx this is wrong: it doesn't necessarily return smethods in precedence-list order
        (loop for vtype in *types-with-vtables*
              when (subtypep type vtype)
              append (vtable-local-message-types type)))))

#+e.vtable-collect.use-mop
(defun vtable-message-types (type)
  ; Note that we reverse the class precedence list, because the caller of this stuffs it into non-strict ConstMap construction, and so takes the *last* seen smethod as the overriding one. xxx perhaps this subtle dependency should be made more robust/explicit?
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

(defvar *exercise-reffiness* nil
  "Set this true to test for code which is inappropriately sensitive to the presence of forwarding refs, by placing them around each argument to DEF-VTABLE methods.")

(defun reffify-args (args)
  "See *EXERCISE-REFFINESS*."
  (map-into args (lambda (x) (make-instance 'forwarding-ref :target x)) args))

(defmacro def-vtable (type-spec &body smethods
    &aux (is-eql (and (consp type-spec) (eql (first type-spec) 'eql)))
         (vtable-class-var (gensym "VTABLE-CLASS"))
         (eql-instance-var (gensym "EQL-INSTANCE"))
         (evaluated-specializer
           (if is-eql
             `(eql ,eql-instance-var)
             type-spec)))
  `(let (,@(when is-eql
             `((,eql-instance-var ,(second type-spec))))
         (,vtable-class-var ,(unless is-eql `(find-class ',type-spec))))
             
    #+(or e.vtable-collect.use-example e.vtable-collect.use-typelist)
    (assert (not (gethash ',type-spec *vtable-message-types-cache*)))
    #+e.vtable-collect.use-typelist 
    (pushnew ',type-spec *types-with-vtables*)
  
    #+(or e.vtable-collect.use-example e.vtable-collect.use-mop)
    (defmethod vtable-local-message-types ((type-sym (eql ',type-spec)))
      (list ,@(loop for smethod in smethods append
        (smethod-maybe-describe-fresh #'smethod-message-desc-pair smethod :prefix-arity 1))))
    
    #+e.vtable-collect.use-example
    (defmethod vtable-collect-message-types ((specimen ,evaluated-specializer) narrowest-type)
      (if (subtypep ',evaluated-specializer narrowest-type)
        (list* ,@(mapcan #'(lambda (smethod) (smethod-maybe-describe-fresh #'smethod-message-desc-pair smethod :prefix-arity 1)) smethods)
          (call-next-method))
        (call-next-method)))
    
    (loop 
      for superclass in 
        ,(if is-eql
          `(e-util:class-precedence-list (class-of ,eql-instance-var))
          `(rest (e-util:class-precedence-list ,vtable-class-var)))
      do (setf (gethash superclass *non-vtable-leaf-classes*) t))
    
    (when ,vtable-class-var
      (defmethod opt-local-method-function-for-class ((class (eql ,vtable-class-var)) mverb)
        (case mverb
          ,@(loop for smethod in smethods collect
              (smethod-function-case-entry smethod 0 :type-name (prin1-to-string type-spec)))
          (otherwise nil))))
    
    ; XXX gensymify 'args
    (defmethod e-call-dispatch ((rec ,evaluated-specializer) mverb &rest args)
      (when *exercise-reffiness*
        (reffify-args args))
      (case mverb
        ,@(loop for smethod in smethods collect
            ; :type-name is purely a debugging hint, not visible at the E level, so it's OK that it might reveal primitive-type information
            (smethod-case-entry smethod 'args `(rec) :type-name (prin1-to-string type-spec)))
        ((:|__respondsTo/2|) (destructuring-bind (verb arity) args
          (e-coercef verb 'string)
          (e-coercef arity '(integer 0))
          (as-e-boolean (or
            (member (e-util:mangle-verb verb arity) 
                    ',(mapcar (lambda (smethod) (smethod-mverb smethod 1)) 
                              smethods))
            (e-is-true (call-next-method))))))
  
        (otherwise (call-next-method))))))

(defgeneric observable-type-of (specimen)
  ;; This should arguably be called observable-class-of.
  (:documentation "Given an object, return the narrowest type specifier which should visible (in its FQN form) to the E programmer. Usually specialized via the def-class-opaque macro."))

(defmacro def-class-opaque (class-name &optional (visible-type class-name))
  `(defmethod observable-type-of ((a ,class-name))
    (declare (ignore a))
    ',visible-type))

; xxx it might be useful to enforce the constraint that anything stamped +selfless-stamp+ and not a FUNCTION must have a specialized observable-type-of, or something to that effect.
;
; Or, perhaps, the observable-type-of anything transparent-selfless should be derived from its uncall's maker somehow.

(defgeneric cl-type-fq-name (type-sym)
  (:documentation  "Given a type specifier, return the corresponding E fully-qualified-name. Usually specialized via the def-fqn macro."))

(defgeneric cl-type-parameters (type-sym parameters))

(defmacro def-fqn (type fqn)
  (assert (typep type 'symbol))
  `(defmethod cl-type-fq-name ((type (eql ',type)))
    ',fqn))

; --- Primitive stamps ---

; These must? be defined early, since any (defglobal +the-whatever+ (e-lambda :stamped +deep-frozen-stamp+ ...)) will cause evaluation of +deep-frozen-stamp+ at the time of execution of the defglobal.

(defglobal +deep-frozen-stamp+ (e-lambda 
    "org.erights.e.elib.serial.DeepFrozenStamp"
    (:doc "The primitive rubber-stamping auditor for DeepFrozen-by-fiat objects.
  
While this is a process-wide object, its stamps should not be taken as significant outside of the vats of the objects stamped by it.")
  ; NOTE: Eventually, deep-frozen-stamp & thread-and-process-wide-semantics-safe-stamp => directly sharable across threads, like Java-E does with all Java-PassByCopy.
  (audited-by-magic-verb (auditor)
    ;; stamped by itself; can't use :stamped because that would try to take the value before the object is constructed
    (eql auditor +deep-frozen-stamp+))
  (:|audit| (object-expr witness)
    ; XXX audit/2 is an E-language-dependent interface, but we must implement it to allow E code to employ these stamps. Should we then define a more general interface? 'audit(language, expr, witness)'? Same questions apply to other primitive stamps.
    (declare (ignore object-expr witness))
    +e-true+)))

(defglobal +selfless-stamp+ (e-lambda
    "org.erights.e.elib.serial.SelflessStamp"
    (:doc "The primitive rubber-stamping auditor for Frozen-and-Transparent-and-Selfless objects, whose uncalls are used in sameness tests.
  
While this is a process-wide object, its stamps should not be taken as significant outside of the vats of the objects stamped by it.")
  (audited-by-magic-verb (auditor)
    (cond 
      ((eql (ref-shorten auditor) +selfless-stamp+)
        ;; Prevents an infinite recursion:
        ;;       (eeq-is-transparent-selfless some-obj)
        ;;    -> (approvedp +selfless-stamp+ some-obj)
        ;;    -> (eeq-is-same-ever +selfless-stamp+ some-approver-of-obj)
        ;;    -> (eeq-is-transparent-selfless +selfless-stamp+)
        ;;    -> (approvedp +selfless-stamp+ +selfless-stamp+)
        ;;    -> (eeq-is-same-ever +selfless-stamp+ +deep-frozen-stamp+)
        ;;    -> repeat with +selfless-stamp+ in place of some-obj
        ;;      
        ;; Since we know the SelflessStamp is not itself selfless, we can shortcut the selfless check to not involve equalizer operations.
        nil)
      ((eql (ref-shorten auditor) +deep-frozen-stamp+)
        ;; Similar to above; the precise form of this recursion has not been determined, but this is a hopeful workaround.
        t)))
  (:|audit| (object-expr witness)
    (declare (ignore object-expr witness))
    +e-true+)))

; --- utilities referenced below ---

(deftype e-list (element-type &aux (sym (gensym (princ-to-string element-type))))
  (setf (symbol-function sym) 
        (lambda (specimen)
          (every (lambda (element) (typep element element-type)) specimen)))
  `(and vector (satisfies ,sym)))

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
  (if (ref-shorten ejector)
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

; --- guards ---

(locally
  (declare (optimize (speed 3) (space 3)))
  (defglobal +the-void-guard+ (e-lambda "org.erights.e.elib.slot.VoidGuard"
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


; XXX thread-safety: make these all vat-local or remove super arg
(defglobal +the-any-guard+    (make-instance 'cl-type-guard :type-specifier 't))
(defglobal +the-nullok-guard+ (make-instance 'cl-type-guard :type-specifier 'null))
(defglobal +the-exception-guard+ (make-instance 'cl-type-guard :type-specifier 'condition))

; --- Equalizer ---

(defvar *the-equalizer*)

(defun eeq-is-same-yet  (a b)
  (e-is-true (e. *the-equalizer* |sameYet| a b)))
(defun eeq-is-same-ever (a b)
  (e-is-true (e. *the-equalizer* |sameEver| a b)))

(defun eeq-is-settled (a)
  (eeq-sameness-fringe a nil))

;;; --- functions that might be referenced before they're defined in the elib package ---

(declaim (ftype (function (t) t) type-specifier-to-guard))