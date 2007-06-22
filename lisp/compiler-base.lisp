; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang.compiler)

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
    :symbol (make-symbol (concatenate 'string "free-" noun-string))
    :noun noun-string))

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

(defgeneric binding-get-code      (binding)
  (:documentation "Return a form which evaluates to the value of the binding."))
(defgeneric binding-get-slot-code (binding)
  (:documentation "Return a form which evaluates to the slot of the binding."))
(defgeneric binding-set-code      (binding value-form)
  (:documentation "Return a form which will set the value of the binding to the result of evaluating value-form."))


; slot bindings - represented as symbols for historical reasons - xxx change that?

(defmethod binding-get-code ((binding symbol))
  `(e. ,binding |getValue|))

(defmethod binding-get-slot-code ((binding symbol))
  binding)

(defmethod binding-set-code ((binding symbol) value-form)
  `(e. ,binding |setValue| ,value-form))



(defclass direct-def-binding () 
  ((symbol :initarg :symbol
           :type symbol
           :reader binding-get-code)
   (noun :initarg :noun ;; XXX this ought to be on a general binding class
         :initform (error ":noun not supplied for a direct-def-binding")
         :type string
         :reader binding-get-source-noun)))

(defmethod binding-get-slot-code ((binding direct-def-binding))
  ; requires that e-simple-slot be selfless (which it is now)
  `(make-instance 'elib:e-simple-slot :value ,(binding-get-code binding)))

(defmethod binding-set-code ((binding direct-def-binding) value-form)
  ;; xxx eventually this should be able to point at the source position of the assignment
  (declare (ignore value-form))
  (error "shouldn't happen: unassignable binding in compiler: ~A" (binding-get-source-noun binding)))


(defclass direct-var-binding ()
  ((value-symbol :initarg :value-symbol
                 :type symbol
                 :reader %var-binding-symbol)
   (broken-symbol :initarg :broken-symbol
                  :type symbol
                  :reader %var-binding-broken-flag)
   (guard-symbol :initarg :guard-symbol
                 :type symbol
                 :reader %binding-guard-code)
   (noun :initarg :noun ;; XXX this ought to be on a general binding class
         :initform (error ":noun not supplied for a direct-var-binding")
         :type string
         :reader binding-get-source-noun)))

(defmethod binding-get-code ((binding direct-var-binding))
  `(if ,(%var-binding-broken-flag binding)
     (error (ref-opt-problem ,(%var-binding-broken-flag binding)))
     ,(%var-binding-symbol binding)))

(defmethod binding-get-slot-code ((binding direct-var-binding))
  `(or ,(%var-binding-broken-flag binding)
       (make-instance ',(if (%binding-guard-code binding)
                          'elib:e-guarded-slot
                          'elib:e-var-slot)
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


(defclass value-binding ()
  ((value :initarg :value
          :reader binding-value)))
  
(defmethod binding-get-code ((binding value-binding))
  `',(binding-value binding))

(defmethod binding-get-slot-code ((binding value-binding))
  ; requires that e-simple-slot be selfless (which it is now)
  `',(make-instance 'elib:e-simple-slot :value (binding-value binding)))

(defmethod binding-set-code ((binding value-binding) value-form)
  ; XXX This should probably be a different message. The current one is just imitating a previous implementation.
  ; Should we have a binding-is-assignable function? ejector on this?
  (declare (ignore value-form))
  (error "not an assignable slot: <& ~A>" (e-quote (binding-value binding))))


(defclass slot-binding ()
  ((slot :initarg :slot
         :reader %slot-binding-variable)))
  
(defmethod binding-get-code ((binding slot-binding))
  `(e. ',(%slot-binding-variable binding) |getValue|))

(defmethod binding-get-slot-code ((binding slot-binding))
  `',(%slot-binding-variable binding))

(defmethod binding-set-code ((binding slot-binding) value-form)
  `(e. ,(%slot-binding-variable binding) |setValue| ,value-form))


(defgeneric binding-for-slot (slot))

(defmethod binding-for-slot ((slot t))
  (make-instance 'slot-binding :slot slot))

(defmethod binding-for-slot ((slot elib:e-simple-slot))
  ; could be extended to cover any DeepFrozen slot
  (make-instance 'value-binding :value (e. slot |getValue|)))

;;; --- ejector-specifier utilities ---

(defun eject-code (ejector-specifier condition-code)
  (ecase (first ejector-specifier)
    ((ejector)   `(eject-or-ethrow ,(second ejector-specifier) ,condition-code))
    ((eject-function)
                 (destructuring-bind (label-form function-form) (rest ejector-specifier)
                   `(elib:%ejector-throw ,label-form ,function-form ,condition-code)))
    ((nil)       `(error ,condition-code))))

(defun opt-ejector-make-code (ejector-specifier)
  "Given an ejector specifier, return a form which returns an Ejector or Throw." ; XXX introduce 'does-not-return-function' concept
  (ecase (car ejector-specifier)
    ((ejector)   (cadr ejector-specifier))
    ((eject-function)
                 `(ejector ,@(rest ejector-specifier)))
    ((nil)       `+the-thrower+)))

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
        (eql auditor +selfless-stamp+)))
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
                     (eq mverb (e-util:mangle-verb (cadr e) (length (caddr e)))))
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

(defun methodical-object-body (generators self-fsym layout methods matchers qualified-name checker-sym type-desc
    &aux (simple-name (simplify-fq-name qualified-name))
         (method-body (getf generators :method-body)))
  (flet ((build-method-case (build-matcher-call)
           `(case mverb
              ,@(loop for method across methods
                      for (nil verb patterns nil nil) = (node-elements method)
                      collect
                        `((,(e-util:mangle-verb verb (length patterns)))
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
                  ;; implement __respondsTo iff this object has methods, otherwise elib:miranda will handle it
                  (miranda-case-maybe :|__respondsTo/2| methods 
                    `(default-responds-to 
                      #',self-fsym args 
                      ',(loop for method across methods
                              for (nil verb patterns nil nil) = (node-elements method)
                              collect (e-util:mangle-verb verb (length patterns)))
                      (lambda (v a fail)
                        (declare (ignorable v a))
                        ,(funcall build-matcher-call 'v 'a '(funcall fail))))))
              ,@(when checker-sym
                  `(((elib:audited-by-magic-verb) 
                     (funcall ,checker-sym (first args)))))
              (otherwise 
                (elib:miranda #',self-fsym mverb args (lambda (fail)
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
      (or (member (e-util:mangle-verb verb arity) mangled-verbs)
          (e-is-true (elib:miranda self :|__respondsTo/2| args 
                        (lambda (fail)
                          (funcall matcher-function :|__respondsTo/2| args fail))))))))

(defun plumbing-object-body (generators self-fsym layout methods matchers qualified-name checker-sym type-desc)
  (declare (ignore self-fsym methods type-desc))
  `(case mverb
    ((elib:audited-by-magic-verb) 
      ,(if checker-sym
         `(funcall ,checker-sym (first args)) 
         nil))
    ; XXX should propagate match failure exception instead
    (otherwise 
      ,(matchers-body generators layout matchers 'mverb 'args
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

(defun make-audition (fqn this-expr meta-state)
  (let (audition 
        (approvers '())
        (audition-ok t))
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
        (:|ask| (other-auditor)
          "Audits the object with the given auditor. XXX describe behavior upon false/throw returns from auditor"
          (assert audition-ok ()
            "~A is out of scope" (e-quote audition))
          (when (if (and (e-is-true (e. other-auditor |__respondsTo| "audit" 2))
                         (not (e-is-true (e. other-auditor |__respondsTo| "audit" 1))))
                  ;; use old auditing protocol iff it is the only one supported
                  (e-is-true (e. other-auditor |audit| this-expr audition))
                  (e-is-true (e. other-auditor |audit| audition)))
            (push other-auditor approvers))
          nil)
        (:|getSlot| (slot-name)
          "Returns the named slot in the audited object's lexical scope.

XXX This is an excessively large authority and will probably be replaced."
          (assert audition-ok ()
            "~A is out of scope" (e-quote audition))
          ; XXX this is a rather big authority to grant auditors - being (eventually required to be) DeepFrozen themselves, they can't extract information, but they can send messages to the slot('s value) to cause undesired effects
          ;; XXX also, cross-layer reference into the current compiler implementation
          (e. meta-state |fetch| (e. "&" |add| slot-name)
            (efun () (error "There is no slot named ~A in ~A." (e-quote slot-name ) (e-quote audition)))))))
    (values audition 
            (lambda () (setf audition-ok nil)) 
            (lambda (auditor)
              (when (position auditor 
                              approvers
                              :test #'elib:samep) 
                t)))))

#| saved old getSlot impl that avoids touching every slot
(cond
  ,@(loop for (name . binding) in (scope-layout-bindings layout) collect
    `((string= slot-name ,name) ,(binding-get-slot-code binding)))
  (t (error "no such slot: ~A" slot-name))) |#

(defun object-form (generators layout this-expr auditor-forms
    &aux (script (e. this-expr |getScript|)))
  "...
  
The scope layout provided should include the binding for the object's name pattern."
  (destructuring-bind (opt-methods matchers) (node-elements script)
    (let* ((has-auditors (> (length auditor-forms) 0))
           (checker-sym (when has-auditors (make-symbol "APPROVEDP")))
           (type-desc (e. this-expr |asTypeDesc| (scope-layout-fqn-prefix layout)))
           (fqn (e. type-desc |getFQName|))
           (self-fsym (make-symbol fqn))
           (inner-layout
             (scope-layout-nest
               (make-instance 'object-scope-layout
                 :nouns (map 'list #'ref-shorten (e. (e. (e. this-expr |staticScope|) |namesUsed|) |getKeys|))
                 :object-expr this-expr
                 :rest (make-instance 'prefix-scope-layout 
                         :fqn-prefix (concatenate 'string fqn "$")
                         :rest layout))))
           (labels-fns
             `((,self-fsym (mverb &rest args)
                 ;; This is to prevent the CL compiler from hoisting the closure
                 ;; closure, forcing it to generate a fresh closure for each
                 ;; execution of this ObjectExpr.
                 ;; 
                 ;; xxx optimization: we could skip this token if the code is
                 ;; known to close over something which varies sufficiently.
                 (refer-to .identity-token.)
                 ,(funcall (if opt-methods
                             #'methodical-object-body
                             #'plumbing-object-body)
                  generators self-fsym inner-layout opt-methods matchers fqn checker-sym type-desc)))))
      (flet ((build-labels (post-forms)
               `(let ((.identity-token. (cons nil nil)))
                  (labels ,labels-fns ,@post-forms #',self-fsym))))
        (if (not has-auditors)
          (build-labels '())
          (let ((audition-sym  (make-symbol "AUDITION"))
                (finisher-sym (make-symbol "AUDITION-FINISH")))
            `(multiple-value-bind (,audition-sym ,finisher-sym ,checker-sym) 
                (make-audition ',fqn 
                              ',this-expr
                              ,(e.compiler.seq::leaf-sequence 
                                (make-instance '|MetaStateExpr| :elements '()) inner-layout))
              ,@(loop for auditor-form in auditor-forms collect
                  `(funcall ,audition-sym :|ask/1| ,auditor-form))
              ,(build-labels
                 ;; This does not need to be in an unwind-protect, because in
                 ;; the event of a nonlocal exit the object reference will not
                 ;; be available, so its mutability is moot.
                 `((funcall ,finisher-sym))))))))))
