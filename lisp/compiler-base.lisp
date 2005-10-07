; Copyright 2005 Kevin Reid, under the terms of the MIT X license
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

(defgeneric scope-layout-bindings-before (inner-layout outer-layout)
  (:documentation "Returns a list of (noun . binding) from INNER-LAYOUT until reaching OUTER-LAYOUT. For example, if A and B consist entirely of conses, (scope-layout-bindings-before a b) is similar to (ldiff a b)."))


(defmethod scope-layout-bindings-before :around (inner outer)
  (if (eql inner outer)
    nil
    (call-next-method)))


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


(defmethod scope-layout-noun-binding ((scope-layout null) noun-string)
  (error 'unbound-noun :noun noun-string))

(defmethod scope-layout-bindings ((scope-layout null))
  nil)

(defmethod scope-layout-fqn-prefix ((scope-layout null))
  ;; XXX add a :if-unavailable (member :error :default nil) parameter?
  "__unknown")

(defmethod scope-layout-noun-is-local ((scope-layout null) noun-string)
  (declare (ignore noun-string))
  nil)
  
(defmethod scope-layout-meta-state-bindings ((scope-layout null))
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


(defclass scope-layout () 
  ((rest :initarg :rest))
  (:documentation "Base class for simple 'inheriting' scopes. Forwards all operations to its 'rest' slot."))

(defmethod print-object ((obj scope-layout) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream ". ~W" (slot-value obj 'rest))))

(defmethod scope-layout-noun-binding ((scope-layout scope-layout) noun-string)
  (scope-layout-noun-binding (slot-value scope-layout 'rest) noun-string))

(defmethod scope-layout-fqn-prefix ((scope-layout scope-layout))
  (scope-layout-fqn-prefix (slot-value scope-layout 'rest)))

(defmethod scope-layout-bindings ((scope-layout scope-layout))
  (scope-layout-bindings (slot-value scope-layout 'rest)))

(defmethod scope-layout-noun-is-local ((scope-layout scope-layout) noun-string)
  (scope-layout-noun-is-local (slot-value scope-layout 'rest) noun-string))

(defmethod scope-layout-meta-state-bindings ((scope-layout scope-layout))
  (scope-layout-meta-state-bindings (slot-value scope-layout 'rest)))

(defmethod scope-layout-bindings-before ((inner scope-layout) outer)
  (scope-layout-bindings-before (slot-value inner 'rest) outer))

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
          :type list))
  (:documentation "Marks the boundary of an object definition, the scope which meta.getState() (implemented via scope-layout-meta-state-bindings) operates on."))

(defmethod scope-layout-meta-state-bindings ((scope-layout object-scope-layout))
  "Return a list of forms which evaluate to vector pairs for constructing the map which meta.getState returns."
  (loop for noun in (slot-value scope-layout 'nouns) collect
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

(defgeneric binding-exit-info     (binding broken-ref-form)
  ;; NOTE: this was written for the sequence compiler, but it *might*
  ;; be generic enough for future work, so I'm leaving it here.
  (:documentation "Returns a list of, for each Lisp variable used by the binding's code, the variable and the form to which it should be bound if an enclosing MatchBindExpr fails, which may evaluate broken-ref-form at most once."))


; slot bindings - represented as symbols for historical reasons - xxx change that?

(defmethod binding-get-code ((binding symbol))
  `(e. ,binding |getValue|))

(defmethod binding-get-slot-code ((binding symbol))
  binding)

(defmethod binding-set-code ((binding symbol) value-form)
  `(e. ,binding |setValue| ,value-form))

(defmethod binding-exit-info ((binding symbol) broken-ref-form)
  `((,binding ,broken-ref-form)))



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
  (error "~A is not an assignable variable" (binding-get-source-noun binding)))

(defmethod binding-exit-info ((binding direct-def-binding) broken-ref-form)
  `((,(binding-get-code binding) ,broken-ref-form)))


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
        `(setf ,(%var-binding-symbol binding) (e. ,(%binding-guard-code binding) |coerce| ,value-form))
        `(setf ,(%var-binding-symbol binding) ,value-form))))

(defmethod binding-exit-info ((binding direct-var-binding) broken-ref-form)
  `((,(%var-binding-symbol binding) ,broken-ref-form)
    (,(%binding-guard-code binding) ,broken-ref-form)
    (,(%var-binding-broken-flag binding) ,broken-ref-form)))


(defclass value-binding ()
  ((value :initarg :value)))
  
(defmethod binding-get-code ((binding value-binding))
  `',(slot-value binding 'value))

(defmethod binding-get-slot-code ((binding value-binding))
  ; requires that e-simple-slot be selfless (which it is now)
  `',(make-instance 'elib:e-simple-slot :value (slot-value binding 'value)))

(defmethod binding-set-code ((binding value-binding) value-form)
  ; XXX This should probably be a different message. The current one is just imitating a previous implementation.
  ; Should we have a binding-is-assignable function? ejector on this?
  (declare (ignore value-form))
  (error "not an assignable slot: <& ~A>" (e-quote (slot-value binding 'value))))

(defmethod binding-exit-info ((binding value-binding) broken-ref-form)
  (error "binding-exit-info on a value-binding shouldn't happen"))


(defclass slot-binding ()
  ((slot :initarg :slot)))
  
(defmethod binding-get-code ((binding slot-binding))
  `(e. ',(slot-value binding 'slot) |getValue|))

(defmethod binding-get-slot-code ((binding slot-binding))
  `',(slot-value binding 'slot))

(defmethod binding-set-code ((binding slot-binding) value-form)
  `(e. ,(slot-value binding 'slot) |setValue| ,value-form))

(defmethod binding-exit-info ((binding slot-binding) broken-ref-form)
  (error "binding-exit-info on a slot-binding shouldn't happen"))


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
                   `(let ((condition ,condition-code))
                      (elib:ejector-prethrow ,label-form condition)
                      (funcall ,function-form condition))))
    ((nil)       `(error ,condition-code))))

(defun opt-ejector-make-code (ejector-specifier)
  "Given an ejector specifier, return a form which returns a nullOk[Ejector]."
  (ecase (car ejector-specifier)
    ((ejector)   (cadr ejector-specifier))
    ((eject-function)
                 `(ejector ,@(rest ejector-specifier)))
    ((nil)       `nil)))

;;; --- support for (scope-layout-noun-binding nil *) ---

(define-condition unbound-noun (program-error)
  ((noun :initarg :noun :reader unbound-noun-noun :type string)))

(def-vtable unbound-noun
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    ;; XXX review wording of this error
    (e. tw |write| "problem: undefined variable: ")
    (e. tw |print| (unbound-noun-noun this))))

;;; --- utilities for generated code ---

(defun %catch-expr-resignal (condition)
  "This function exists to make it clear in a backtrace that the condition did not originate at this location."
  (error condition))

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

(defun methodical-object-body (generators self-fsym layout methods matchers qualified-name checker-sym doc-comment
    &aux (simple-name (simplify-fq-name qualified-name))
         (type-desc
           (e. +the-make-type-desc+ |run|
             doc-comment
             qualified-name
             #()
             #()
             (or-miranda-message-descs
               (loop for method across methods
                 for (doc-comment verb patterns opt-result-guard nil) = (node-elements method)
                 collect
                   (make-instance 'message-desc
                     :verb verb
                     :doc-comment doc-comment
                     :params (map 'vector #'pattern-to-param-desc patterns)
                     :opt-result-guard (opt-guard-expr-to-safe-opt-guard opt-result-guard))))))
         (method-body (getf generators :method-body)))
  `(case mverb
    ,@(loop for method across methods
            for (nil verb patterns nil nil) = (node-elements method)
            collect
              `((,(e-util:mangle-verb verb (length patterns)))
                ,(funcall method-body layout method 'args)))
    ,@(miranda-case-maybe :|__printOn/1| methods `(default-print args ',simple-name))
    ,@(miranda-case-maybe :|__getAllegedType/0| methods `',type-desc)
    ,@(miranda-case-maybe :|__respondsTo/2| methods 
      `(default-responds-to 
        #',self-fsym args 
        ',(loop for method across methods
                for (nil verb patterns nil nil) = (node-elements method)
                collect (e-util:mangle-verb verb (length patterns)))))
    ((elib:audited-by-magic-verb) 
      ,(if checker-sym
         `(funcall ,checker-sym (first args)) 
         nil))
    (otherwise 
      (elib:miranda #',self-fsym mverb args (lambda ()
        ,(matchers-body generators layout matchers 'mverb 'args
          `(error "no such method: ~A#~A" ',qualified-name mverb)))))))

(defun default-print (args simple-name)
  (destructuring-bind (tw) args
    (e. (e-coerce tw +the-text-writer-guard+) |write| (concatenate 'string "<" simple-name ">"))))

(defun default-responds-to (self args mangled-verbs)
  (destructuring-bind (verb arity) args
    (e-coercef verb 'string)
    (e-coercef arity '(integer 0))
    (as-e-boolean
      (or (member (e-util:mangle-verb verb arity) mangled-verbs)
          (e-is-true (elib:miranda self :|__respondsTo/2| args nil))))))

(defun plumbing-object-body (generators self-fsym layout methods matchers qualified-name checker-sym doc-comment)
  (declare (ignore self-fsym methods doc-comment))
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

(defun make-witness (fqn this-expr meta-state)
  (let (witness 
        (approvers '())
        (witness-ok t))
    (setf witness
      (e-lambda "org.erights.e.elang.evm.AuditWitness" ()
        (:|__printOn| (tw)
          (e-coercef tw +the-text-writer-guard+)
          (e. tw |write| (concatenate 'string
            "<"
            (if witness-ok "" "closed ")
            "Witness for "
            (aan fqn)
            ">"))
          nil)
        (:|ask| (other-auditor)
          "Audits the object with the given auditor. XXX describe behavior upon false/throw returns from auditor"
          (assert witness-ok ()
            "~A is out of scope" (e-quote witness))
          (when (e-is-true (e. other-auditor |audit| this-expr witness))
            (push other-auditor approvers))
          nil)
        (:|getSlot| (slot-name)
          "Returns the named slot in the audited object's lexical scope.

XXX This is an excessively large authority and will probably be replaced."
          (assert witness-ok ()
            "~A is out of scope" (e-quote witness))
          ; XXX this is a rather big authority to grant auditors - being (eventually required to be) DeepFrozen themselves, they can't extract information, but they can send messages to the slot('s value) to cause undesired effects
          ;; XXX also, cross-layer reference into the current compiler implementation
          (e. meta-state |get| (e. "&" |add| slot-name)))))
    (values witness 
            (lambda () (setf witness-ok nil)) 
            (lambda (auditor)
              (when (position auditor 
                              approvers
                              :test #'elib:eeq-is-same-ever) 
                t)))))

#| saved old getSlot impl that avoids touching every slot
(cond
  ,@(loop for (name . binding) in (scope-layout-bindings layout) collect
    `((string= slot-name ,name) ,(binding-get-slot-code binding)))
  (t (error "no such slot: ~A" slot-name))) |#

(defun object-form (generators layout this-expr doc-comment auditor-forms script fqn)
  (destructuring-bind (opt-methods matchers) (node-elements script)
    (let* ((has-auditors (> (length auditor-forms) 0))
           (checker-sym (when has-auditors (make-symbol "APPROVEDP")))
           (self-fsym (make-symbol fqn))
           (inner-layout
             (make-instance 'object-scope-layout
               :nouns (coerce (e. (e. (e. this-expr |staticScope|) |namesUsed|) |getKeys|) 'list)
               :rest (make-instance 'prefix-scope-layout 
                       :fqn-prefix (concatenate 'string fqn "$")
                       :rest layout))))
      (let ((labels-fns
             `((,self-fsym (mverb &rest args)
                 ,(funcall (if opt-methods
                             #'methodical-object-body
                             #'plumbing-object-body)
                  generators self-fsym inner-layout opt-methods matchers fqn checker-sym doc-comment)))))
        (if (not has-auditors)
          `(labels ,labels-fns #',self-fsym)
          (let ((witness-sym  (make-symbol "WITNESS"))
                (finisher-sym (make-symbol "WITNESS-FINISH")))
            `(multiple-value-bind (,witness-sym ,finisher-sym ,checker-sym) 
                (make-witness ',fqn 
                              ',this-expr
                              ,(e.compiler.seq::leaf-sequence 
                                (make-instance '|MetaStateExpr| :elements '()) layout))
              ,@(loop for auditor-form in auditor-forms collect
                  `(funcall ,witness-sym :|ask/1| ,auditor-form))
              (labels ,labels-fns
                ;; This does not need to be in an unwind-protect, because in
                ;; the event of a nonlocal exit the object reference will not
                ;; be available, so its mutability is moot.
                (funcall ,finisher-sym)
                #',self-fsym))))))))
