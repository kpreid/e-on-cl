; This Common Lisp code is Copyright 2005-2007 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.elib)

(eval-when (:compile-toplevel)
  (declaim (optimize (safety 3))))

; --- E objects ---

; A built-for-E object is a FUNCTION expecting (mverb &rest args). It handles its own dispatch, match, and no-such-method.

; These must be defined early as some e-calls to function objects are performed in later loading.
(defmethod e-call-dispatch ((recip function) mverb &rest args)
  (apply recip mverb args))

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

(defgeneric vtable-local-message-types (specializer)
  (:documentation "Returns a list of #(mverb message-desc) tuples for the given class, but not its superclasses.")
  (:method ((specializer t))
    (declare (ignore specializer))
    '()))

(defun vtable-message-types (type)
  ; Note that we reverse the class precedence list, because the caller of this stuffs it into non-strict ConstMap construction, and so takes the *last* seen smethod as the overriding one. xxx perhaps this subtle dependency should be made more robust/explicit?
  (loop for superclass in (reverse (e-util:class-precedence-list (find-class type)))
    append (vtable-local-message-types (class-name superclass))))

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
    
    ,@(unless is-eql
        `((let ((class (find-class ',type-spec)))
            (unless (class-finalized-p class)
              (finalize-inheritance class)))))
  
    (defmethod vtable-local-message-types ((type-sym (eql ',type-spec)))
      (list ,@(loop for smethod in smethods append
        (smethod-maybe-describe-fresh #'smethod-message-desc-pair smethod :prefix-arity 1))))
    
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

; xxx it might be useful to enforce the constraint that anything transparent-selfless and not a FUNCTION must have a specialized observable-type-of, or something to that effect.
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

(defglobal +thread-sharable-stamp+ (e-lambda 
    "org.erights.e.elib.serial.ThreadSharableStamp"
    (:doc "The primitive rubber-stamping auditor for objects whose /implementation/ (those components of its state not exposed by __optUncall) is thread-safe. It does *not* guarantee observed immutability or no-outside-effects-during-turn behaviors; see DeepFrozenStamp. This stamp is a Lisp-system-wide authority.")
  (audited-by-magic-verb (auditor)
    ;; stamped by itself; can't use :stamped because that would try to take the value before the object is constructed
    (or (eql auditor +deep-frozen-stamp+)
        (eql auditor +thread-sharable-stamp+)))
  (:|audit| (object-expr witness)
    (declare (ignore object-expr witness))
    +e-true+)))

(defglobal +deep-frozen-stamp+ (e-lambda 
    "org.erights.e.elib.serial.DeepFrozenStamp"
    (:doc "The primitive rubber-stamping auditor for DeepFrozen-by-fiat objects.
  
While this is a process-wide object, its stamps should not be taken as significant outside of the vats of the objects stamped by it.")
  (audited-by-magic-verb (auditor)
    ;; stamped by itself; can't use :stamped because that would try to take the value before the object is constructed
    (or (eql auditor +deep-frozen-stamp+)
        (eql auditor +thread-sharable-stamp+)))
  (:|audit| (audition)
    (declare (ignore audition))
    +e-true+)))

(defglobal +selfless+ (e-lambda 
    "org.erights.e.elib.serial.Selfless"
    (:doc "XXX document this")
  (audited-by-magic-verb (auditor)
    (or (eql auditor +deep-frozen-stamp+)
        (eql auditor +thread-sharable-stamp+)))
  (:|audit| (audition)
    (declare (ignore audition))
    +e-true+)
  (:|coerce/2| (standard-coerce #'selflessp (lambda () +selfless+)))
  (:|passes| (x) (as-e-boolean (selflessp x)))))

(defglobal +transparent-stamp+ (e-lambda
    "org.erights.e.elib.serial.TransparentStamp"
    (:doc "The primitive rubber-stamping auditor for Transparent objects, whose uncalls are guaranteed to be accurate.
  
While this is a process-wide object, its stamps should not be taken as significant outside of the vats of the objects stamped by it.")
  (audited-by-magic-verb (auditor)
    (setf auditor (ref-shorten auditor))
    (cond 
      ((eql auditor +transparent-stamp+)
        ;; Prevents an infinite recursion:
        ;;       (transparent-selfless-p some-obj)
        ;;    -> (approvedp +transparent-stamp+ some-obj)
        ;;    -> (samep +transparent-stamp+ some-approver-of-obj)
        ;;    -> (transparent-selfless-p +transparent-stamp+)
        ;;    -> (approvedp +transparent-stamp+ +transparent-stamp+)
        ;;    -> (samep +transparent-stamp+ +deep-frozen-stamp+)
        ;;    -> repeat with +transparent-stamp+ in place of some-obj
        ;;      
        ;; Since we know the TransparentStamp is not itself transparent, we can shortcut the transparent check to not involve equalizer operations.
        nil)
      ((eql auditor +deep-frozen-stamp+)
        ;; Similar to above; the precise form of this recursion has not been determined, but this is a hopeful workaround.
        t)
      ((eql auditor +thread-sharable-stamp+)
        ;; Just an ordinary stamp.
        t)))
  (:|audit| (audition)
    (declare (ignore audition))
    +e-true+)))

(defglobal +transparent-guard+ (e-lambda 
    "org.erights.e.elib.serial.Transparent"
    (:stamped +deep-frozen-stamp+
     :stamped +thread-sharable-stamp+)
  (:|passes/1| (specimen)
    (as-e-boolean (approvedp +transparent-stamp+ specimen)))
  (:|coerce/2| (standard-coerce 
                 (lambda (s) (approvedp +transparent-stamp+ s))
                 (lambda () +transparent-guard+)))))

(defglobal +semitransparent-stamp+ (e-lambda
    "org.erights.e.elib.serial.SemitransparentStamp"
    (:doc "Objects with this stamp, if also Selfless, are compared by amplifying with the Semitransparent brand; the sealed box should contain a portrayal. XXX this may be renamed.")
  (audited-by-magic-verb (auditor)
    (setf auditor (ref-shorten auditor))
    ;; see TransparentStamp comments
    (cond 
      ((eql auditor +transparent-stamp+)     nil)
      ((eql auditor +deep-frozen-stamp+)     t)
      ((eql auditor +thread-sharable-stamp+) t)))
  (:|audit| (audition)
    (declare (ignore audition))
    +e-true+)))
(defglobal +semitransparent-result-box-brand+ (e-lambda 
    "org.erights.e.elib.serial.SemitransparentBoxBrand" 
    (:stamped +deep-frozen-stamp+
     :stamped +thread-sharable-stamp+)))
(defgeneric semitransparent-result-box-contents (box))
(defclass semitransparent-result-box ()
  ((value :initarg :value :reader semitransparent-result-box-contents)))

(defglobal +pass-by-construction+ (e-lambda 
    "org.erights.e.elib.serial.PassByConstruction"
    (:stamped +deep-frozen-stamp+
     :stamped +thread-sharable-stamp+)
  (:|audit| (audition)
    (declare (ignore audition))
    +e-true+)
  (:|coerce/2| (standard-coerce 
                 (lambda (s) 
                   (or (approvedp +pass-by-construction+ s)
                       (and (approvedp +selfless+ s)
                            (approvedp +transparent-stamp+ s)
                            (typep (e. (e. s |__optUncall|) |get| 0)
                                   `(or ,(guard-to-type-specifier +pass-by-construction+)
                                        ,(guard-to-type-specifier +standard-graph-exit+))))))
                 (lambda () +pass-by-construction+)))))
        
(defglobal +standard-graph-exit-stamp+ (e-lambda 
    "org.erights.e.elib.serial.StandardGraphExitStamp"
    (:stamped +deep-frozen-stamp+
     :stamped +thread-sharable-stamp+)
  (:|audit| (object-expr witness)
    (declare (ignore object-expr witness))
    +e-true+)))

; --- utilities referenced below ---

(deftype e-list (element-type &aux (sym (make-symbol (format nil "generated predicate for (E-LIST ~A)" element-type))))
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
    (let ((r (efuncall ejector condition)))
      (error "optEjector ~A returned: ~A" (e-quote ejector) (e-quote r)))
    (progn
      ;; XXX once we've sufficiently moved to passing 'throw' not 'null', make this an error
      (warn "gave null ejector to throw ~S ~A" condition condition)
      (error condition))))

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

(defgeneric guard-to-type-specifier (guard))
;; see guard.lisp for methods

(defglobal +the-void-guard+ (e-lambda "org.erights.e.elib.slot.VoidGuard"
    (:stamped +deep-frozen-stamp+
     :stamped +thread-sharable-stamp+)
  (:|__printOn| ((tw +the-text-writer-guard+)) ; XXX move to e.syntax?
    (e. tw |print| "void"))
  (:|coerce| (specimen opt-ejector)
    (declare (ignore specimen opt-ejector))
    nil)))
 
; Simple native-type Guards
(defclass cl-type-guard () 
  ((ts :initarg :type-specifier
       :reader guard-to-type-specifier)))

(declaim (inline standard-coerce))
(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defun standard-coerce (test 
                          conform-guard-thunk 
                          &key
                          (error 
                           (lambda (specimen)
                             (make-condition 'type-error
                               :datum specimen 
                               :expected-type (guard-to-type-specifier (funcall conform-guard-thunk)))))
                          (test-shortened t))
    "Typical guard coercion. Returns a function which returns the first of these which passes the test, or ejects the result of error-thunk via opt-ejector: specimen, ref-shortened specimen, ref-shortened result of specimen.__conformTo(conform-guard-thunk.run()).

If returning an unshortened reference is acceptable and the test doesn't behave differently on unshortened references, specify :test-shortened nil for an optimization."
    (declare (optimize (speed 3) (space 3)))
    (lambda (long-specimen opt-ejector)
      (labels ((fail ()
                (eject-or-ethrow opt-ejector
                  (funcall error long-specimen))))
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
  (defun e-coerce-native (long-specimen type &optional (ejector +the-thrower+) opt-guard)
    (declare (optimize (speed 3) (space 3)))
    (funcall (standard-coerce #'(lambda (specimen) (typep specimen type))
                              #'(lambda () (or opt-guard
                                             (type-specifier-to-guard type)))
                              :error 
                              #'(lambda (specimen) (make-condition 'type-error
                                                     :datum specimen 
                                                     :expected-type type)))
             long-specimen
             ejector)))

(defun type-specifier-to-guard (ts)
  (or
    (and (consp ts)
         (= (length ts) 2)
         (eq (first ts) 'satisfies)
         (get (second ts) 'satisfies-type-specifier-guard))
    (make-instance 'cl-type-guard :type-specifier ts)))

;;; --- Additional reference pieces ---

(defun no-such-method (recipient mverb args)
  (error 'no-such-method-error :recipient recipient 
                               :mverb mverb
                               :args args))

(defmethod e-call-dispatch ((rec t) mverb &rest args)
  "Fallthrough case for e-call-dispatch - forwards to e-call-match so the class hierarchy gets another chance."
  (elib:miranda rec mverb args (lambda (fail)
    (apply #'e-call-match fail rec mverb args))))

(defmethod e-call-match (fail (rec t) mverb &rest args)
  "Final case of E call dispatch - always fails."
  (declare (ignore rec mverb args))
  (funcall fail))


#+e.instrument.ref-shorten-uses 
  (defvar *instrument-ref-shorten-kinds* (make-hash-table))

(defun ref-shorten (x)
  (declare (optimize (speed 3) (space 3) (safety 3) (compilation-speed 0))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  #+e.instrument.ref-shorten-uses
    (incf (gethash (class-of x) *instrument-ref-shorten-kinds* 0))
  (typecase x
    ((not ref) x)
    (resolved-ref
      (setf (resolved-ref-target x) 
            (ref-shorten (resolved-ref-target x))))
    (t
      (%ref-shorten x))))

;;; --- Promise resolution ---

;; NOTE: vtable and resolution is declared now (rather than, say, in elib-guts.lisp) so that later code can make use of promises when being loaded

(declaim (inline promise-ref-resolve))
(defun promise-ref-resolve (ref new-target
    &aux (buffer (promise-ref-buffer ref))
         (weak-wmrs (%weak-when-more-resolved-table ref)))
  (declare (type promise-ref ref)
           (type (vector list) buffer))
  (setf new-target (ref-shorten new-target))
  (with-ref-transition-invariants (ref)
    (if (eq new-target ref)
      (change-class ref 'unconnected-ref :problem (make-condition 'vicious-cycle-error))
      (change-class ref 'resolved-ref :target new-target)))
  
  ; after change-class, the buffer has been dropped by the ref
  ; we could optimize the case of just forwarding many messages to the target, for when the target is another promise, but don't yet
  (loop for (resolver mverb args) across buffer do
    (if resolver
      (e. resolver |resolve| (apply #'e-send-dispatch new-target mverb args))
      (apply #'e-send-only-dispatch new-target mverb args)))
  
  (maphash (lambda (reactor action)
             (funcall action reactor))
           weak-wmrs)
  
  (values))

(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defun resolve-race (this target)
    "Internal function used by the local-resolver vtable."
    (with-accessors ((ref resolver-opt-promise)) this
      (if (null ref)
        +e-false+
        (progn
          (promise-ref-resolve ref target)
          (setf ref nil)
          +e-true+)))))

(def-vtable local-resolver
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |print| (if (resolver-opt-promise this)
                     "<Resolver>"
                     "<Closed Resolver>")))
  (:|resolveRace/1| 'resolve-race)
  (:|resolve| (this target)
    (unless (e-is-true (resolve-race this target))
      ;; under sealed exception rules, this is not an information leak
      (error "this resolver's ref has already been resolved, therefore cannot be resolved to ~A" (e-quote target))))
  (:|smash| (this problem)
    "Equivalent to this.resolve(Ref.broken(problem))."
    ; XXX the doc comment is accurate, and specifies the appropriate behavior, but it does so by performing the same operation as Ref.broken()'s implementation in a separate implementation. Both occurrences should probably be routed through the exception-semantics section in base.lisp.
    (e. this |resolve| (elib:make-unconnected-ref (e-coerce problem 'condition))))
  (:|isDone| (this)
    "Returns whether this resolver's promise has been resolved already."
    (as-e-boolean (not (resolver-opt-promise this))))
  (:|gettingCloser| (this)
    "Has no visible effect; used by causality tracing. XXX write documentation."
    (declare (ignore this))
    nil)) 

;;; --- sugar cache ---

;; XXX used to be in vat.lisp, but moved here now that the implementation contains an e-lambda. sufficient justification?

(declaim (inline sugar-cache-get))
(defun sugar-cache-get (key fqn fail)
  (let ((cache (sugar-cache *vat*)))
    (or (gethash key cache)
        (setf (gethash key cache)
          (e. (e. (vat-safe-scope *vat*) |fetch| "import__uriGetter" +the-thrower+) 
              |fetch| fqn fail)))))

(declaim (inline sugar-cache-call))
(defun sugar-cache-call (fail rec mverb key fqn &rest args)
  (let ((sugar (sugar-cache-get key fqn
                 (efun ()
                   (return-from sugar-cache-call (funcall fail))))))
    (case mverb
      (:|__respondsTo/2|
        (e. sugar |__respondsTo| 
          (concatenate 'string "instance_" (e-coerce (first args) 'string))
          (1+ (e-coerce (second args) '(integer 0)))))
      ;; XXX __getAllegedType/0
      (otherwise
        (apply #'e-call-dispatch sugar
          (multiple-value-bind (verb arity) (unmangle-verb mverb)
            (mangle-verb (concatenate 'string "instance_" verb) (1+ arity)))
          rec
          args)))))

;;; --- functions that might be referenced before they're defined in the elib package ---

(declaim (ftype (function (t) t) type-specifier-to-guard))

(declaim (ftype (function (t t) boolean) 
                same-yet-p samep))
(declaim (ftype (function (t) boolean) 
                settledp))
