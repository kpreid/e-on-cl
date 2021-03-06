; Copyright 2005-2008 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.compiler)

; --- Scope layouts ---

; Currently, a scope layout may be:
;   null - the empty lexical scope
;   cons - (cons [one noun/binding association] [another scope])
;   (eql *) - the wildcard for GET-TRANSLATION
;   prefix-scope-layout - defining the FQN prefix
;   scope-box-scope-layout - indicating that nouns outside it may be rebound
;   object-scope-layout - providing the innermost enclosing ObjectExpr's static scope, for scope-layout-meta-state-bindings

(defgeneric scope-layout-noun-binding (scope noun-string)
  (:documentation "Return the binding object for a given noun in this scope. Signals UNBOUND-NOUN if there is no binding."))

(defgeneric scope-layout-fqn-prefix (scope)
  (:documentation "Return the FQN prefix for objects defined in this scope."))

(defgeneric scope-layout-bindings (scope-layout)
  (:documentation "Return an alist of the nouns defined in this scope and their binding objects."))

(defgeneric scope-layout-noun-is-local (scope noun-string)
  (:documentation "Returns whether the given noun is bound inside of a scope box, and therefore should not be allowed to be rebound."))

(defgeneric scope-layout-meta-state-bindings (scope-layout)
  (:documentation "Returns a list of '(vector # #) forms suitable for constructing the return value of MetaStateExpr."))

;; XXX this is no longer used since MatchBindExpr became nonkernel. delete?
(defgeneric scope-layout-bindings-before (inner-layout outer-layout)
  (:documentation "Returns a list of (noun . binding) from INNER-LAYOUT until reaching OUTER-LAYOUT. For example, if A and B consist entirely of conses, (scope-layout-bindings-before a b) is similar to (ldiff a b)."))

(defmethod scope-layout-bindings-before :around (inner outer)
  (if (eql inner outer)
    nil
    (call-next-method)))

(defgeneric scope-layout-opt-object-expr (scope-layout)
  (:documentation "Currently internal. Returns the innermost inclosing ObjectExpr if possible, as for meta.context()."))


(defmethod scope-layout-noun-binding ((scope-layout (eql '*)) noun-string)
  (make-instance 'direct-def-binding 
    :value-var (make-symbol (concatenate 'string "free-" noun-string))))

(defmethod scope-layout-bindings ((scope-layout (eql '*)))
  nil)

(defmethod scope-layout-fqn-prefix ((scope-layout (eql '*)))
  "*")

(defmethod scope-layout-noun-is-local ((scope-layout (eql '*)) noun-string)
  (declare (ignore noun-string))
  nil)
  
(defmethod scope-layout-meta-state-bindings ((scope-layout (eql '*)))
  nil)

(defmethod scope-layout-opt-object-expr ((scope-layout (eql '*)))
  nil)


(defmethod scope-layout-noun-binding ((scope-layout null) noun-string)
  (error 'unbound-noun :noun noun-string))

(defmethod scope-layout-bindings ((scope-layout null))
  nil)

(defmethod scope-layout-fqn-prefix ((scope-layout null))
  ;; xxx add a :if-unavailable (member :error :default nil) parameter?
  "__unknown$")

(defmethod scope-layout-noun-is-local ((scope-layout null) noun-string)
  (declare (ignore noun-string))
  nil)
  
(defmethod scope-layout-meta-state-bindings ((scope-layout null))
  nil)

(defmethod scope-layout-opt-object-expr ((scope-layout null))
  nil)

(defmethod scope-layout-noun-binding ((scope-layout cons) noun-string
    &aux (entry (first scope-layout)))
  (if (string= (car entry) noun-string)
    (cdr entry)
    (scope-layout-noun-binding (rest scope-layout) noun-string)))

(defmethod scope-layout-bindings ((scope-layout cons))
  (cons (first scope-layout)
        (scope-layout-bindings (rest scope-layout))))

(defmethod scope-layout-fqn-prefix ((scope-layout cons))
  (scope-layout-fqn-prefix (rest scope-layout)))

(defmethod scope-layout-noun-is-local ((scope-layout cons) noun-string
    &aux (entry (first scope-layout)))
  (or (string= (car entry) noun-string)
      (scope-layout-noun-is-local (rest scope-layout) noun-string)))

(defmethod scope-layout-meta-state-bindings ((scope-layout cons))
  (cons `(vector ,(concatenate 'string "&" (caar scope-layout))
                 ,(binding-get-slot-code (cdar scope-layout)))
        (scope-layout-meta-state-bindings (rest scope-layout))))

(defmethod scope-layout-bindings-before ((inner cons) outer)
  (cons (first inner)
        (scope-layout-bindings-before (rest inner) outer)))

(defmethod scope-layout-opt-object-expr ((scope-layout cons))
  nil)


(defclass scope-layout () 
  ((rest :initarg :rest
         :reader scope-layout-rest))
  (:documentation "Base class for simple 'inheriting' scopes. Forwards all operations to its 'rest' slot."))

(defmethod print-object ((obj scope-layout) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream ". ~W" (scope-layout-rest obj))))

(defmethod scope-layout-noun-binding ((scope-layout scope-layout) noun-string)
  (scope-layout-noun-binding (scope-layout-rest scope-layout) noun-string))

(defmethod scope-layout-fqn-prefix ((scope-layout scope-layout))
  (scope-layout-fqn-prefix (scope-layout-rest scope-layout)))

(defmethod scope-layout-bindings ((scope-layout scope-layout))
  (scope-layout-bindings (scope-layout-rest scope-layout)))

(defmethod scope-layout-noun-is-local ((scope-layout scope-layout) noun-string)
  (scope-layout-noun-is-local (scope-layout-rest scope-layout) noun-string))

(defmethod scope-layout-meta-state-bindings ((scope-layout scope-layout))
  (scope-layout-meta-state-bindings (scope-layout-rest scope-layout)))

(defmethod scope-layout-bindings-before ((inner scope-layout) outer)
  (scope-layout-bindings-before (scope-layout-rest inner) outer))

(defmethod scope-layout-opt-object-expr ((scope-layout scope-layout))
  (scope-layout-opt-object-expr (scope-layout-rest scope-layout)))


(defclass prefix-scope-layout (scope-layout)
  ((fqn-prefix :initarg :fqn-prefix :reader scope-layout-fqn-prefix))
  (:documentation "Establishes a new FQN prefix."))


(defclass scope-box-scope-layout (scope-layout) ()
  (:documentation "Represents a scope box. Enables rebinding of bindings established outside the scope box."))

(defmethod scope-layout-noun-is-local ((scope-layout scope-box-scope-layout) noun-string)
  (declare (ignore noun-string))
  nil)


(defclass object-scope-layout (scope-layout)
  ((nouns :initarg :nouns
          :type list
          :reader %object-scope-layout-nouns)
  (object-expr :initarg :object-expr
               :type e.kernel:|ObjectExpr|
               :reader scope-layout-opt-object-expr))
  (:documentation "Marks the boundary of an object definition, the scope which meta.getState() (implemented via scope-layout-meta-state-bindings) operates on."))

(defmethod scope-layout-meta-state-bindings ((scope-layout object-scope-layout))
  "Return a list of forms which evaluate to vector pairs for constructing the map which meta.getState returns."
  (loop for noun in (%object-scope-layout-nouns scope-layout) collect
    `(vector ,(concatenate 'string "&" noun)
             ,(binding-get-slot-code (scope-layout-noun-binding scope-layout noun)))))


(defun scope-layout-bind (scope-layout noun-string binding)
  "Return a scope layout with a binding added; fail if it is already defined locally."
  (if (scope-layout-noun-is-local scope-layout noun-string)
    (error "~S already in scope" noun-string)
    (cons (cons noun-string binding) scope-layout)))

(defun scope-layout-nest (scope-layout)
  "Return a scope layout which contains the same bindings as the argument, but allows rebinding of them."
  (make-instance 'scope-box-scope-layout :rest scope-layout))

; --- Scope layout noun bindings ---

(defgeneric binding-get-code (binding)
  (:documentation "Return a form which evaluates to the value of the binding.")
  (:method (binding)
    `(e. ,(binding-get-slot-code binding) |get|)))

(defgeneric binding-get-slot-code (binding)
  (:documentation "Return a form which evaluates to the slot of the binding."))

(defgeneric binding-set-code (binding value-form)
  (:documentation "Return a form which will set the value of the binding to the result of evaluating value-form.")
  (:method (binding value-form)
    `(e. ,(binding-get-slot-code binding) |put| ,value-form)))

(defgeneric binding-audit-guard-code (binding)
  (:documentation "Return a form which evaluates to the guard which should be exposed by an audition."))


(defclass lexical-slot-binding ()
  ((symbol :initarg :symbol
           :type symbol
           :reader binding-get-slot-code)
   (guard-var :initarg :slot-guard-var
              :initform '+the-any-guard+
              :reader binding-audit-guard-code)))


(defclass final-binding ()
  ()
  (:documentation "Bindings which have a constant value, and generate FinalSlots when necessary."))
  
(defmethod binding-get-slot-code ((binding final-binding))
  ;; For this to be correct, e-simple-slot must be selfless.
  `(make-instance 'e-simple-slot :value ,(binding-get-code binding)))


(defclass direct-def-binding (final-binding)
  ((symbol :initarg :value-var
           :type symbol
           :reader binding-get-code)
   (guard-var :initarg :value-guard-var
              :initform '+the-any-guard+
              :reader binding-audit-value-guard-code))
  (:documentation "The type of binding produced by FinalPatterns. Refers to CL variables holding the value and the guard of the value."))

(defun final-binding-guard-ref (value-guard)
  (when value-guard
    (make-instance 'thunk-lazy-ref :thunk (lambda ()
      (e. (load-time-value (type-specifier-to-guard 'e-simple-slot)) |get|
        value-guard)))))

(defmethod binding-audit-guard-code ((binding direct-def-binding))
  `(final-binding-guard-ref ,(binding-audit-value-guard-code binding)))


(defclass direct-var-binding ()
  ((value-symbol :initarg :value-symbol
                 :type symbol
                 :reader %var-binding-symbol)
   (broken-symbol :initarg :broken-symbol
                  :type symbol
                  :reader %var-binding-broken-flag)
   (guard-symbol :initarg :guard-symbol
                 :type symbol
                 :reader %binding-guard-code)))

(defmethod binding-get-code ((binding direct-var-binding))
  `(if ,(%var-binding-broken-flag binding)
     (error (ref-opt-problem ,(%var-binding-broken-flag binding)))
     ,(%var-binding-symbol binding)))

(defun dvb-slot-type (binding)
  (if (%binding-guard-code binding)
    'e-guarded-slot
    'e-var-slot))

(defmethod binding-get-slot-code ((binding direct-var-binding))
  `(or ,(%var-binding-broken-flag binding)
       (make-instance ',(dvb-slot-type binding)
         :getter (lambda () ,(%var-binding-symbol binding))
         :setter ,(let ((x (gensym)))
                    `(lambda (,x)
                       (setf ,(%var-binding-symbol binding) ,x)))
         ,@(when (%binding-guard-code binding)
             `(:guard ,(%binding-guard-code binding))))))

(defmethod binding-set-code ((binding direct-var-binding) value-form)
  `(if ,(%var-binding-broken-flag binding)
     (error (ref-opt-problem ,(%var-binding-broken-flag binding)))
     ,(if (%binding-guard-code binding)
        `(setf ,(%var-binding-symbol binding) (e. ,(%binding-guard-code binding) |coerce| ,value-form nil))
        `(setf ,(%var-binding-symbol binding) ,value-form))))

(defmethod binding-audit-guard-code ((binding direct-var-binding))
  `',(type-specifier-to-guard (dvb-slot-type binding)))


(defclass constant-guard-binding ()
  ((cgb-guard :initarg :audit-slot-guard
              :initform +the-any-guard+
              :reader %binding-audit-slot-guard)))

(defmethod binding-audit-guard-code ((binding constant-guard-binding))
  `',(%binding-audit-slot-guard binding))


(defclass value-binding (constant-guard-binding)
  ((value :initarg :value
          :reader binding-value)))
  
(defmethod binding-get-code ((binding value-binding))
  `',(binding-value binding))

(defmethod binding-get-slot-code ((binding value-binding))
  ;; For this to be correct, e-simple-slot must be selfless.
  `',(make-instance 'e-simple-slot :value (binding-value binding)))


(defclass slot-binding (constant-guard-binding)
  ((slot :initarg :slot
         :reader %slot-binding-slot)))
  
(defmethod binding-get-slot-code ((binding slot-binding))
  `',(%slot-binding-slot binding))


(defgeneric binding-for-slot (slot guard))

(defmethod binding-for-slot ((slot t) guard)
  (make-instance 'slot-binding :slot slot :audit-slot-guard guard))

(defmethod binding-for-slot ((slot e-simple-slot) guard)
  (make-instance 'value-binding :value (eelt slot) :audit-slot-guard guard))

(defun to-compiler-binding (binding)
  "Given a runtime scope binding (a CoercedSlot), compute an equivalent compiler binding."
  (e-coercef binding 'coerced-slot)
  (binding-for-slot (eelt binding) (e. binding |getGuard|)))

(defun binding-reify-code (binding)
  "Given a compiler binding, produce a form which returns the equivalent CoercedSlot."
  `(make-instance 'coerced-slot
                  :value ,(binding-get-slot-code binding)
                  :guard ,(binding-audit-guard-code binding)))

;;; --- Ejector bindings ---

;; A block-binding is a special binding for an *unreified* ejector. It cannot be referenced normally; the compiler uses it when it is recognizable that an ejector is only ever used in ways that can be compiled into direct uses of the underlying block.

(defgeneric eject-via-binding-code (binding condition-form)
  (:method ((binding t) condition-form)
    `(eject-or-ethrow ,(binding-get-code binding) ,condition-form)))

(defclass block-binding ()
  ((operator :initarg :operator :reader block-binding-operator)
   (label :initarg :label :reader block-binding-label)))

(defmethod binding-audit-guard-code ((binding block-binding))
  `(final-binding-guard-ref +the-any-guard+))

(defmethod eject-via-binding-code ((binding block-binding) condition-form)
  `(%ejector-throw ,(block-binding-label binding)
                   (lambda (v) (,(block-binding-operator binding) v))
                   ,condition-form))


(defconstant +throw-binding+ '+throw-binding+)

(defmethod eject-via-binding-code ((binding (eql +throw-binding+)) condition-form)
  `(efuncall +the-thrower+ ,condition-form))

(defmethod binding-get-code ((binding (eql +throw-binding+)))
  '+the-thrower+)

(defmethod binding-get-slot-code ((binding (eql +throw-binding+)))
  '(load-time-value (make-instance 'e-simple-slot :value +the-thrower+)))

(defmethod binding-audit-guard-code ((binding (eql +throw-binding+)))
  `(final-binding-guard-ref +the-any-guard+))

;;; --- support for (scope-layout-noun-binding nil *) ---

(define-condition unbound-noun (program-error)
  ((noun :initarg :noun :reader unbound-noun-noun :type string)))

(def-vtable unbound-noun
  (:|__printOn| (this (tw +the-text-writer-guard+))
    ;; XXX review wording of this error
    (e. tw |write| "problem: shouldn't happen: uncaught unbound noun in compiler: ")
    (e. tw |print| (unbound-noun-noun this))))

;;; --- utilities for generated code ---

(defun %catch-expr-resignal (condition)
  "This function exists to make it clear in a backtrace that the condition did not originate at this location."
  (error condition))

(defun %miranda-type-merge (base more)
  ;; XXX should this be a method on TypeDesc?
  ;; XXX coerce 'more'?
  (e. +the-make-type-desc+ |run|
    (e. base |getDocComment|)
    (e. base |getFQName|)
    (e. base |getSupers|) ;; XXX do something with this?
    (e. base |getAuditors|)
    (e. (e. (e. base |getMessageTypes|) |or| 
            (e. more |getMessageTypes|)) 
        |getValues|)))

(declaim (notinline refer-to)
         (ftype (function (t) null) refer-to))
(defun refer-to (object)
  "Force a function definition to close over the given OBJECT."
  (declare (optimize (speed 3) (space 3)) (ignore object))
  nil)

;;; --- StaticContext ---

;; This is a class so that instances may be written to a compiled file.
(defclass static-context ()
  ((source-span :initarg :source-span
                :reader static-context-source-span)
   (fqn-prefix :initarg :fqn-prefix
               :initform (error "fqn-prefix not supplied")
               :reader static-context-fqn-prefix)
   (opt-object-source :initarg :opt-object-source
                      :initform (error "object-source not supplied")
                      :reader static-context-opt-object-source))
  #| (:documentation XXX) |#)

(defmethod make-load-form ((a static-context) &optional environment)
  (make-load-form-saving-slots a :environment environment))

(defobject +the-make-static-context+
    "org.erights.e.elang.scope.makeStaticContext"
    (:stamped +deep-frozen-stamp+)
  (:|asType| () (type-specifier-to-guard 'static-context))
  (:|run| ((fqn-prefix 'string) 
           (syn-env 'null)
           (opt-object-source '(or null e.kernel:|ObjectExpr|))
           (source-span '(or null source-span)))
    (declare (ignore syn-env))
    (make-instance 'static-context
      :source-span source-span
      :fqn-prefix fqn-prefix
      :opt-object-source opt-object-source)))

(def-fqn static-context "org.erights.e.elang.scope.staticContext")

;; XXX static contexts must be selfless or fresh, or they break equivalence of separate evaluation
(def-vtable static-context
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (or (eql auditor +deep-frozen-stamp+)
        (eql auditor +selfless+)
        (eql auditor +transparent-stamp+)))
  (:|__optUncall| (this)
    `#(,+the-make-static-context+
       "run"
       #(,(static-context-fqn-prefix this)
         nil
         ,(static-context-opt-object-source this)
         ,(static-context-source-span this))))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (declare (ignore this))
    (e. tw |print| "<static context>"))
  (:|getOptSource/0| 'static-context-opt-object-source)
  (:|getSource| (this)
    (or (ref-shorten (e. this |getOptSource|))
        (error "There is no enclosing object expression at ~A~/e.tables:~span/" (e. this |getFQNPrefix|) (static-context-source-span this))))
  (:|getFQNPrefix/0| 'static-context-fqn-prefix))

(defun scope-layout-static-context (layout &key source-span)
  (make-instance 'static-context
    :source-span source-span
    :fqn-prefix (scope-layout-fqn-prefix layout)
    :opt-object-source (scope-layout-opt-object-expr layout)))

;;; --- ObjectExpr code generation ---

(defun miranda-case-maybe (mverb methods &rest body)
  (unless (find-if (lambda (method &aux (e (node-elements method)))
                     (eq mverb (mangle-verb (cadr e) (length (caddr e)))))
                   methods)
    `(((,mverb) ,@body))))

(defun matchers-body (generators layout matchers mverb-var args-var remaining-code)
  (let ((matcher-body (getf generators :matcher-body)))
    (labels ((m-b-list (matchers)
               (if matchers
                 (funcall matcher-body layout 
                                       (first matchers)
                                       mverb-var
                                       args-var
                                       (m-b-list (rest matchers)))
                 remaining-code)))
      (m-b-list (coerce matchers 'list)))))

;; XXX TODO: map out the twisty path of build-matcher-call, build-method-case, matcher-function, and fail, and see if some simplification can be done

(defgeneric object-body (generators self-fsym layout script qualified-name checker-sym type-desc))

(defmethod object-body (generators self-fsym layout (script |EScript|) qualified-name checker-sym type-desc
    &aux (simple-name (simplify-fq-name qualified-name))
         (method-body (getf generators :method-body))
         (methods (e. script |getMethods|))
         (matchers (e. script |getMatchers|)))
  (flet ((build-method-case (build-matcher-call)
           `(case mverb
              ,@(loop for method across methods
                      for (nil verb patterns nil nil) = (node-elements method)
                      collect
                        `((,(mangle-verb verb (length patterns)))
                          ,(funcall method-body layout method 'args)))
              ,@(miranda-case-maybe :|__printOn/1| methods `(default-print args ',simple-name))
              ,@(miranda-case-maybe :|__getAllegedType/0| methods
                (if (plusp (length matchers))
                  `(block nil
                    (%miranda-type-merge ',type-desc
                                         ,(funcall build-matcher-call
                                           :|__getAllegedType/0|
                                           nil
                                           `(return ',type-desc))))
                  `',type-desc))
              ,@(when (plusp (length methods))
                  ;; implement __respondsTo iff this object has methods, otherwise e.elib:miranda will handle it
                  (miranda-case-maybe :|__respondsTo/2| methods 
                    `(default-responds-to 
                      #',self-fsym args 
                      ',(loop for method across methods
                              for (nil verb patterns nil nil) = (node-elements method)
                              collect (mangle-verb verb (length patterns)))
                      (lambda (v a fail)
                        (declare (ignorable v a))
                        ,(funcall build-matcher-call 'v 'a '(funcall fail))))))
              ,@(when checker-sym
                  `(((e.elib:audited-by-magic-verb) 
                     (funcall ,checker-sym (first args)))))
              (otherwise 
                (miranda #',self-fsym mverb args (lambda (fail)
                  ,(funcall build-matcher-call
                     'mverb 'args 
                     `(funcall fail))))))))
    (if (plusp (length matchers))
      (let ((matcher-entry-fname (gensym (format nil "~A-MATCHER" qualified-name)))
            (fail-fun (gensym "FAIL-FUN")))
        `(flet ((,matcher-entry-fname (mmverb margs ,fail-fun)
                (declare (ignorable mmverb margs))
                ,(matchers-body generators layout matchers 'mmverb 'margs
                   `(funcall ,fail-fun))))
           ,(build-method-case (lambda (mverb args fail-form) 
                                 `(,matcher-entry-fname ,mverb ,args (lambda () ,fail-form))))))
      (build-method-case (lambda (mverb args fail-form)
                           (declare (ignore mverb args))
                           fail-form)))))

(defun default-print (args simple-name)
  (destructuring-bind (tw) args
    (e. (e-coerce tw +the-text-writer-guard+) |write| (concatenate 'string "<" simple-name ">"))))

(defun default-responds-to (self args mangled-verbs matcher-function)
  (destructuring-bind (verb arity) args
    (e-coercef verb 'string)
    (e-coercef arity '(integer 0))
    (as-e-boolean
      (or (member (mangle-verb verb arity) mangled-verbs)
          (e-is-true (miranda self :|__respondsTo/2| args 
                        (lambda (fail)
                          (funcall matcher-function :|__respondsTo/2| args fail))))))))

(defmethod object-body (generators self-fsym layout (matcher |EMatcher|) qualified-name checker-sym type-desc)
  (declare (ignore self-fsym type-desc))
  `(case mverb
    ((e.elib:audited-by-magic-verb) 
      ,(if checker-sym
         `(funcall ,checker-sym (first args)) 
         nil))
    ; XXX should propagate match failure exception instead
    (otherwise 
      ,(matchers-body generators layout (list matcher) 'mverb 'args
        `(error "Plumbing match failure: ~W#~A" ',qualified-name mverb)))))

(defmacro updating-fully-qualify-name (scope-layout-place qn-form
    &aux (qn-var (gensym)) (prefix-var (gensym)))
  ; This is a macro because it will need to update the scope layout once scope layouts have 'anonymous object serial number' information
  `(let ((,qn-var ,qn-form)
         (,prefix-var (scope-layout-fqn-prefix ,scope-layout-place)))
    (cond
      ((string= ,qn-var "_") 
        ;; XXX should generate a serial number instead
        (concatenate 'string ,prefix-var "_"))
      (t
        (join-fq-name ,prefix-var ,qn-var)))))

(defobject +e-audition-stamp+ 
    "$eAuditionStamp" 
    (:stamped +deep-frozen-stamp+)
  (:|audit| (audition)
    (declare (ignore audition))
    +e-true+))

(defobject +e-audition-guard+ 
    "$EAudition"
    (:stamped +deep-frozen-stamp+)
  (:|coerce/2| (standard-coerce
                 (lambda (specimen) (approvedp +e-audition-stamp+ specimen))
                 (lambda () +e-audition-guard+)
                 :error (lambda (specimen) (format nil "~A is not an E Audition" (e-quote specimen)))
                 :test-shortened nil)))

(defun guard-table-form (nouns layout)
  (let ((tv (gensym "AGT")))
    `(let ((,tv (make-hash-table :test #'equal)))
       (setf ,@(loop for noun in nouns
                     collect `(gethash ',noun ,tv)
                     collect (binding-audit-guard-code
                               (scope-layout-noun-binding layout noun))))
       ,tv)))

(defun invoke-auditor (auditor audition)
  (e-coerce
    (if (and (e-is-true (e. auditor |__respondsTo| "audit" 2))
             (not (e-is-true (e. auditor |__respondsTo| "audit" 1))))
      ;; use old auditing protocol iff it is the only one supported
      (e. auditor |audit| (e. audition |getObjectExpr|) audition)
      (e. auditor |audit| audition))
    '(or null e-boolean)))

(defun make-audit-cache ()
  (make-generic-hash-table :test 'samep))

(defstruct (audit-cache-record
            (:conc-name #:acr-))
  answer
  asked
  guards)

(defun bootstrap-safe-deep-frozen-p (specimen)
  (or (approvedp +deep-frozen-stamp+ specimen)
      (let ((slot (e. (vat-safe-scope *vat*) |fetchSlot| "DeepFrozen" +the-thrower+)))
        (if (e.knot::unseal-lazy-slot-forced-box (e. slot |__optSealedDispatch| e.knot::+lazy-slot-forced-brand+))
          (e-is-true (e. (eelt slot) |isDeepFrozen| specimen))
          (progn
            #+e.audit-cache.debug (break "not DFchecking ~S" specimen)
            nil)))))

(defun make-audition (fqn this-expr guard-table audit-cache)
  (let* (audition 
         (approvers '())
         (audition-ok t)
         (asked-log (make-symbol "ASKED-LOG"))
         (guard-log (make-symbol "GUARD-LOG"))
         (logs (list asked-log guard-log)))
    (setf audition
      (e-lambda "org.erights.e.elang.evm.Audition" 
          (:stamped +e-audition-stamp+)
        (:|__printOn| ((tw +the-text-writer-guard+))
          (e. tw |write| (concatenate 'string
            "<"
            (if audition-ok "" "closed ")
            "Audition for "
            (aan fqn)
            ">"))
          nil)
        (:|getFQName| ()
          ;; I note that with the addition of this method, the audition is
          ;; starting to resemble a StaticContext. -- kpreid 2006-10-02
          "The fully-qualified name of the object under audit."
          fqn)
        (:|getObjectExpr| ()
          "The ObjectExpr defining the object under audit."
          this-expr)
        (:|ask| (auditor)
          "Audits the object with the given auditor. XXX describe behavior upon false/throw returns from auditor"
          (assert audition-ok ()
            "~A is out of scope" (e-quote audition))
          (when (boundp asked-log)
            (push auditor (symbol-value asked-log)))
          (let* ((caching (approvedp +deep-frozen-stamp+ auditor))
                 (cached (when caching
                           (let ((cached (hashref auditor audit-cache)))
                             (block check
                               (dolist (pair (when cached (acr-guards cached)))
                                 (unless (samep (gethash (car pair) guard-table)
                                                (cdr pair))
                                   (e. e.knot::+sys-trace+ |run| (format nil "AUDIT: ~S ~A: ignoring cache for guard ~S mismatch: old=~A new=~A" fqn (e-quote auditor) (car pair) (e-quote (cdr pair)) (e-quote (gethash (car pair) guard-table))))
                                   (return-from check nil)))
                               cached))))
                 (answer (if cached
                           (progn
                             #+e.audit-cache.debug (format *trace-output* "~&$$$ ~S ~S: using cache entry ~S~%" fqn audit-cache cached)
                             (loop for asked in (acr-asked cached) do
                               (e. audition |ask| asked))
                             (acr-answer cached))
                           (progv logs '(nil nil)
                             (let ((r (invoke-auditor auditor audition)))
                                (when (and caching
                                           (not (eql (symbol-value guard-log)
                                                     'ruined)))
                                  (let ((record
                                          (make-audit-cache-record
                                            :answer r
                                            :asked (symbol-value asked-log)
                                            :guards (symbol-value guard-log))))
                                    #+e.audit-cache.debug (format *trace-output* "~&$$$ Storing ~S ~S ~S: ~S~%" fqn audit-cache auditor record)
                                    (setf (hashref auditor audit-cache)
                                          record)))
                                r)))))
            (when (e-is-true answer)
              (push auditor approvers)))
          nil)
        (:|getGuard| ((noun 'string))
          "Returns a guard which the named slot in the audited object's environment has passed."
          (assert audition-ok ()
            "~A is out of scope" (e-quote audition))
          (let ((answer (or (gethash noun guard-table)
                            (error "~A is not a free variable in ~A"
                                   (e-quote noun) (e-quote fqn)))))
            (when (and (boundp guard-log)
                       (not (eql (symbol-value guard-log) 'ruined)))
              (if (bootstrap-safe-deep-frozen-p answer)
                (push (cons noun answer) (symbol-value guard-log))
                (progn
                  #+e.audit-cache.debug (format *trace-output* "~&$$$ ~S: ruined by guard of ~S: ~S~%" fqn  noun (e-quote answer))
                  (setf (symbol-value guard-log) 'ruined))))
            answer))))
    (values audition 
            (lambda () (setf audition-ok nil)) 
            (lambda (auditor)
              (when (position auditor 
                              approvers
                              :test #'samep) 
                t)))))

(defmacro compiler-object (object-body post-forms self-fsym)
  ;; XXX poor name
  "Used by the E-to-CL compiler as the translation of ObjectExpr. This is a macro rather than part of the compiler, so that compiler-macros can recognize object expressions easily. XXX There is more work to be done on simplifying this macro's parameter structure."
  `(let ((.hash-code. #+e.function-sxhash-inadequate (make-hash-code)
                      #-e.function-sxhash-inadequate (cons nil nil)))
     (labels ((,self-fsym (mverb &rest args)
                ;; Besides providing a hash code for the sxhash-inadequate
                ;; case, this prevents the CL compiler from hoisting the
                ;; closure, forcing it to generate a fresh closure for each
                ;; execution of this ObjectExpr.
                ;; 
                ;; xxx optimization: we could skip this if the code is known
                ;; to close over something which varies sufficiently.
                (if (eql mverb 'e.elib:selfish-hash-magic-verb)
                  .hash-code.
                  ,object-body)))
       ,@post-forms
       #',self-fsym)))

(defun object-form (generators layout this-expr auditor-forms
    &aux (script (e. this-expr |getScript|)))
  "...
  
The scope layout provided should include the binding for the object's name pattern."
  (let* ((has-auditors (> (length auditor-forms) 0))
         (checker-sym (when has-auditors (make-symbol "APPROVEDP")))
         (type-desc (e. this-expr |asTypeDesc| (scope-layout-fqn-prefix layout)))
         (fqn (e. type-desc |getFQName|))
         (self-fsym (make-symbol fqn))
         (outer-nouns (map 'list #'ref-shorten (e. (e. (e. this-expr |staticScope|) |namesUsed|) |getKeys|)))
         (inner-layout
           (scope-layout-nest
             (make-instance 'object-scope-layout
               :nouns outer-nouns
               :object-expr this-expr
               :rest (make-instance 'prefix-scope-layout 
                       :fqn-prefix (concatenate 'string fqn "$")
                       :rest layout)))))
    (flet ((build-labels (post-forms)
             `(compiler-object 
                ,(object-body generators self-fsym inner-layout script fqn
                              checker-sym type-desc)
                ,post-forms
                ,self-fsym)))
      (if (not has-auditors)
        (build-labels '())
        (let ((audition-sym  (make-symbol "AUDITION"))
              (finisher-sym (make-symbol "AUDITION-FINISH")))
          `(multiple-value-bind (,audition-sym ,finisher-sym ,checker-sym) 
              (make-audition ',fqn 
                             ',this-expr
                             ,(guard-table-form outer-nouns inner-layout)
                             (load-time-value (make-audit-cache)))
            ,@(loop for auditor-form in auditor-forms collect
                `(funcall ,audition-sym :|ask/1| ,auditor-form))
            ,(build-labels
               ;; This does not need to be in an unwind-protect, because in
               ;; the event of a nonlocal exit the object reference will not
               ;; be available, so its mutability is moot.
               `((funcall ,finisher-sym)))))))))
