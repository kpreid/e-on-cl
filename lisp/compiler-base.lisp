; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang)

; --- inner scopes ---

; XXX the name 'inner scope' should be replaced. it's confusing if actually used in the compiler code (which uses the old no-longer-accurate name 'name-map')

; Currently, an inner scope may be:
;   null - the empty lexical scope
;   cons - (cons [one noun/binding association] [another scope])
;   (eql *) - the wildcard for GET-TRANSLATION
;   prefix-inner-scope - defining the FQN prefix
;   scope-box-inner-scope - indicating that nouns outside it may be rebound
;   object-inner-scope - providing the innermost enclosing ObjectExpr's static scope, for inner-scope-meta-state-bindings

(defgeneric inner-scope-noun-binding (scope noun-string)
  (:documentation "Return the binding object for a given noun in this scope. Signals UNBOUND-VARIBLE if there is no binding."))

(defgeneric inner-scope-fqn-prefix (scope))

(defgeneric inner-scope-to-outer-scope (inner-scope))

(defgeneric inner-scope-bindings (inner-scope))

(defgeneric inner-scope-noun-is-local (scope noun-string)
  (:documentation "Returns whether the given noun is bound inside of a scope box, and therefore should not be allowed to be rebound."))

(defgeneric inner-scope-meta-state-bindings (inner-scope)
  (:documentation "Returns a list of '(vector # #) forms suitable for constructing the return value of MetaStateExpr."))


(defmethod inner-scope-noun-binding ((inner-scope (eql '*)) noun-string)
  (outer-slot-binding noun-string))

(defmethod inner-scope-bindings ((inner-scope (eql '*)))
  nil)

(defmethod inner-scope-fqn-prefix ((inner-scope (eql '*)))
  "*")

(defmethod inner-scope-noun-is-local ((inner-scope (eql '*)) noun-string)
  (declare (ignore noun-string))
  nil)
  
(defmethod inner-scope-meta-state-bindings ((inner-scope (eql '*)))
  nil)


(defmethod inner-scope-noun-binding ((inner-scope null) noun-string)
  (error 'unbound-variable :name (simple-slot-symbol noun-string)))

(defmethod inner-scope-bindings ((inner-scope null))
  nil)

(defmethod inner-scope-fqn-prefix ((inner-scope null))
  "__unknown")

(defmethod inner-scope-noun-is-local ((inner-scope null) noun-string)
  (declare (ignore noun-string))
  nil)
  
(defmethod inner-scope-meta-state-bindings ((inner-scope null))
  nil)
  
  
(defmethod inner-scope-noun-binding ((inner-scope cons) noun-string
    &aux (entry (first inner-scope)))
  (if (string= (car entry) noun-string)
    (cdr entry)
    (inner-scope-noun-binding (rest inner-scope) noun-string)))

(defmethod inner-scope-bindings ((inner-scope cons))
  (cons (first inner-scope)
        (inner-scope-bindings (rest inner-scope))))

(defmethod inner-scope-fqn-prefix ((inner-scope cons))
  (inner-scope-fqn-prefix (rest inner-scope)))

(defmethod inner-scope-noun-is-local ((inner-scope cons) noun-string
    &aux (entry (first inner-scope)))
  (or (string= (car entry) noun-string)
      (inner-scope-noun-is-local (rest inner-scope) noun-string)))

(defmethod inner-scope-meta-state-bindings ((inner-scope cons))
  (cons `(vector ,(concatenate 'string "&" (caar inner-scope))
                 ,(binding-get-slot-code (cdar inner-scope)))
        (inner-scope-meta-state-bindings (rest inner-scope))))
  

; base class for simple 'inheriting' scopes
(defclass inner-scope () ((rest :initarg :rest)))

(defmethod print-object ((obj inner-scope) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream ". ~W" (slot-value obj 'rest))))

(defmethod inner-scope-noun-binding ((inner-scope inner-scope) noun-string)
  (inner-scope-noun-binding (slot-value inner-scope 'rest) noun-string))

(defmethod inner-scope-fqn-prefix ((inner-scope inner-scope))
  (inner-scope-fqn-prefix (slot-value inner-scope 'rest)))

(defmethod inner-scope-bindings ((inner-scope inner-scope))
  (inner-scope-bindings (slot-value inner-scope 'rest)))

(defmethod inner-scope-noun-is-local ((inner-scope inner-scope) noun-string)
  (inner-scope-noun-is-local (slot-value inner-scope 'rest) noun-string))

(defmethod inner-scope-meta-state-bindings ((inner-scope inner-scope))
  (inner-scope-meta-state-bindings (slot-value inner-scope 'rest)))


(defclass prefix-inner-scope (inner-scope)
  ((fqn-prefix :initarg :fqn-prefix :reader inner-scope-fqn-prefix))
  (:documentation "Establishes a new FQN prefix."))


(defclass scope-box-inner-scope (inner-scope) ()
  (:documentation "Represents a scope box. Enables rebinding of bindings established outside the scope box."))

(defmethod inner-scope-noun-is-local ((inner-scope scope-box-inner-scope) noun-string)
  (declare (ignore noun-string))
  nil)


(defclass object-inner-scope (inner-scope)
  ((nouns :initarg :nouns
          :type list))
  (:documentation "XXX document this"))

(defmethod inner-scope-meta-state-bindings ((inner-scope object-inner-scope))
  (loop for noun in (slot-value inner-scope 'nouns) collect
    `(vector ,(concatenate 'string "&" noun)
             ,(binding-get-slot-code (inner-scope-noun-binding inner-scope noun)))))


(defun inner-scope-bind (inner-scope noun-string binding)
  (if (inner-scope-noun-is-local inner-scope noun-string)
    (error "~S already in scope" noun-string)
    (cons (cons noun-string binding) inner-scope)))

(defun inner-scope-nest (inner-scope)
  "Return an inner scope which contains the same bindings as the argument, but allows rebinding of them."
  (make-instance 'scope-box-inner-scope :rest inner-scope))

; --- inner scope noun bindings ---

(defgeneric binding-get-code      (binding))
(defgeneric binding-get-slot-code (binding))
(defgeneric binding-set-code      (binding value-form))
(defgeneric binding-let-entry     (binding))
(defgeneric binding-smash-code    (binding broken-ref-form))

; slot bindings - represented as symbols for historical reasons - XXX change that

(defmethod binding-get-code ((binding symbol))
  `(e. ,binding |getValue|))

(defmethod binding-get-slot-code ((binding symbol))
  binding)

(defmethod binding-set-code ((binding symbol) value-form)
  `(e. ,binding |setValue| ,value-form))

(defmethod binding-let-entry ((binding symbol))
  `(,binding ,elib:the-unset-slot))

(defmethod binding-smash-code ((binding symbol) broken-ref-form)
  `(setq ,binding (make-instance 'elib:e-simple-slot :value ,broken-ref-form)))



(defclass direct-def-binding () 
  ((symbol :initarg :symbol
           :reader binding-get-code)))

(defmethod binding-get-slot-code ((binding direct-def-binding))
  ; requires that e-simple-slot be selfless (which it is now)
  `(make-instance 'elib:e-simple-slot :value ,(binding-get-code binding)))

(defmethod binding-set-code ((binding direct-def-binding) value-form)
  `(setq ,(binding-get-code binding) ,value-form))

(defmethod binding-let-entry ((binding direct-def-binding))
  `(,(binding-get-code binding) ',(elib:make-unconnected-ref "accidentally unbound direct binding")))

(defmethod binding-smash-code ((binding direct-def-binding) broken-ref-form)
  `(setq ,(binding-get-code binding) ,broken-ref-form))


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

(defclass slot-binding ()
  ((slot :initarg :slot)))
  
(defmethod binding-get-code ((binding slot-binding))
  `(e. ',(slot-value binding 'slot) |getValue|))

(defmethod binding-get-slot-code ((binding slot-binding))
  `',(slot-value binding 'slot))

(defmethod binding-set-code ((binding slot-binding) value-form)
  `(e. ,(slot-value binding 'slot) |setValue| ,value-form))


(defgeneric binding-for-slot (slot))

(defmethod binding-for-slot ((slot t))
  (make-instance 'slot-binding :slot slot))

(defmethod binding-for-slot ((slot elib:e-simple-slot))
  ; could be extended to cover any DeepFrozen slot
  (make-instance 'value-binding :value (e. slot |getValue|)))

; --- ... ---

(defun eject-code (ejector-specifier condition-code)
  (ecase (car ejector-specifier)
    ((ejector)   `(eject-or-ethrow ,(cadr ejector-specifier) ,condition-code))
    ((catch-tag) `(let ((condition ,condition-code))
                    (elib:ejector-prethrow ',ejector-specifier condition)
                    (throw ,(cadr ejector-specifier) condition)))
    ((nil)       `(error ,condition-code))))

(defun opt-ejector-make-code (ejector-specifier)
  "Given an ejector specifier, return a form which returns a nullOk[Ejector]."
  (ecase (car ejector-specifier)
    ((ejector)   (cadr ejector-specifier))
    ((catch-tag) `(make-instance 'elib:ejector :catch-tag ,(cadr ejector-specifier)))
    ((nil)       `nil)))

(defun outer-slot-symbol (var-name)
  "The symbol used for a variable when the name map contains an outer-scope wildcard (*)."
  (intern (concatenate 'string "outer-&" var-name)))

(defun outer-slot-binding (var-name)
  "The binding used for a variable when the name map contains an outer-scope wildcard (*)."
  (make-instance 'direct-def-binding :symbol (intern (concatenate 'string "outer-" var-name))))

(defun simple-slot-symbol (var-name)
  "The symbol used for a variable when there are no scope conflicts requiring another name to be used."
  (intern (concatenate 'string "&" var-name)))

; XXX this should be either fixed or scrapped. It was stubbed out when inner scope objects were introduced.
(defun unmapped-symbol-such-as (symbol name-map)
  "Given a symbol and a name map, returns a symbol not present in the name map, which may be the input symbol or a similarly- or identically-named (possibly uninterned) symbol."
  (declare (ignore name-map))
  (make-symbol (symbol-name symbol)))

; (defun unmapped-symbol-such-as (symbol name-map)
;   "Given a symbol and a name map, returns a symbol not present in the name map, which may be the input symbol or a similarly- or identically-named (possibly uninterned) symbol."
;   (cond
;     ((or (null name-map)
;          (eql '* name-map))       symbol)
;     ((eql (cdar name-map) symbol) (make-symbol (symbol-name symbol)))
;     (t                            (unmapped-symbol-such-as symbol (cdr name-map)))))

(defun pick-slot-symbol (varName outer-name-map)
  ; XXX documentation is variously stale
  "Returns an unused symbol for the given variable name, and a new name-map with an entry for it."
  (let ((sym (unmapped-symbol-such-as (simple-slot-symbol varName) outer-name-map)))
    (values sym (inner-scope-bind outer-name-map varName sym))))

(defun pick-binding-symbol (varName outer-name-map)
  ; XXX documentation is variously stale
  "Returns a new direct-def binding object for the given variable name, and a new name-map with an entry for it."
  (let* ((sym (unmapped-symbol-such-as (intern varName) outer-name-map))
         (binding (make-instance 'direct-def-binding :symbol sym)))
    (values binding (inner-scope-bind outer-name-map varName binding))))

(defun slot-symbol-var-name (sym)
  "Return the Kernel-E variable name corresponding to the given symbol's name, or nil if it is not in the expected format."
  (let* ((sn (symbol-name sym))
         (amp (position #\& sn)))
    (if amp
      (subseq sn (1+ amp)))))

(defun make-lets (vars inner-form)
  (if vars
    `(let (,@(mapcar (lambda (v) (binding-let-entry v)) vars)) 
      (declare (ignorable ,@(mapcar (lambda (v) (car (binding-let-entry v))) vars)))
      ,inner-form)
    inner-form))

(defgeneric transform (expr outer-name-map))
(defgeneric transform-pattern (patt specimen-form ejector-spec outer-name-map)
  (:documentation "note that the transform function MUST generate code which evaluates specimen-form exactly once"))

(defmacro updating-transform (expr-var name-map-var vars-var)
  `(multiple-value-bind (new-vars new-name-map result) (transform ,expr-var ,name-map-var)
    (setf ,name-map-var new-name-map)
    (setf ,vars-var (append new-vars ,vars-var))
    result))

(defmacro updating-transform-pattern (expr-var specimen-form-var ejector-spec-var name-map-var vars-var)
  `(multiple-value-bind (new-vars new-name-map result) (transform-pattern ,expr-var ,specimen-form-var ,ejector-spec-var ,name-map-var)
    (setf ,name-map-var new-name-map)
    (setf ,vars-var (append new-vars ,vars-var))
    result))

(defun scope-box (vars outer-name-map sub-form)
  (values () outer-name-map (make-lets vars sub-form)))

(defun scope-box-transform (outer-name-map sub)
  (values () outer-name-map
          (tail-transform-exprs
            (list sub)
            (inner-scope-nest outer-name-map))))

