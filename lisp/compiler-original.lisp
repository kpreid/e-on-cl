; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

;;; --- misc. symbol utils that were in compiler-base ---

; XXX this should be either fixed or scrapped. It was stubbed out when scope layout objects (then 'inner scopes', and 'name maps' before that) were introduced.
(defun unmapped-symbol-such-as (symbol layout)
  "Given a symbol and a name map, returns a symbol not present in the name map, which may be the input symbol or a similarly- or identically-named (possibly uninterned) symbol."
  (declare (ignore layout))
  (make-symbol (symbol-name symbol)))

(defun pick-slot-symbol (varName outer-layout)
  ; XXX documentation is variously stale
  "Returns an unused symbol for the given variable name, and a new layout with an entry for it."
  (let ((sym (unmapped-symbol-such-as (simple-slot-symbol varName) outer-layout)))
    (values sym (scope-layout-bind outer-layout varName sym))))

(defun pick-binding-symbol (varName outer-layout)
  ; XXX documentation is variously stale
  "Returns a new direct-def binding object for the given variable name, and a new layout with an entry for it."
  (let* ((sym (unmapped-symbol-such-as (intern varName) outer-layout))
         (binding (make-instance 'direct-def-binding :symbol sym)))
    (values binding (scope-layout-bind outer-layout varName binding))))

;;; --- core ---

(defun make-lets (vars inner-form)
  (if vars
    `(let (,@(mapcar (lambda (v) (binding-let-entry v)) vars)) 
      (declare (ignorable ,@(mapcar (lambda (v) (car (binding-let-entry v))) vars)))
      ,inner-form)
    inner-form))

(defgeneric transform (expr outer-layout))
(defgeneric transform-pattern (patt specimen-form ejector-spec outer-layout)
  (:documentation "note that the transform function MUST generate code which evaluates specimen-form exactly once"))

(defmacro updating-transform (expr-var layout-var vars-var)
  `(multiple-value-bind (new-vars new-layout result) (transform ,expr-var ,layout-var)
    (setf ,layout-var new-layout)
    (setf ,vars-var (append new-vars ,vars-var))
    result))

(defmacro updating-transform-pattern (expr-var specimen-form-var ejector-spec-var layout-var vars-var)
  `(multiple-value-bind (new-vars new-layout result) (transform-pattern ,expr-var ,specimen-form-var ,ejector-spec-var ,layout-var)
    (setf ,layout-var new-layout)
    (setf ,vars-var (append new-vars ,vars-var))
    result))

(defun scope-box (vars outer-layout sub-form)
  (values () outer-layout (make-lets vars sub-form)))

(defun scope-box-transform (outer-layout sub)
  (values () outer-layout
          (tail-transform-exprs
            (list sub)
            (scope-layout-nest outer-layout))))

; --- transformation utils ---

(defmacro def-expr-transformation (class (layout-var &rest elements-lambda-list) &body body
    &aux (node-var (gensym)))
  `(defmethod transform ((,node-var ,class) ,layout-var)
    (destructuring-bind ,elements-lambda-list (node-elements ,node-var) 
      ,@body)))

(defmacro def-patt-transformation (class (specimen-form-var ejector-spec-var layout-var &rest elements-lambda-list) &body body
    &aux (node-var (gensym)))
  `(defmethod transform-pattern ((,node-var ,class) ,specimen-form-var ,ejector-spec-var ,layout-var)
    (declare (ignorable ,ejector-spec-var)) ; because declare ignore doesn't work when nested
    (destructuring-bind ,elements-lambda-list (node-elements ,node-var) 
      ,@body)))


(defgeneric tail-transform-expr (expr tail-exprs layout))
(defgeneric tail-transform-pattern (patt tail-patts tail-exprs specimen-form ejector-spec layout))

(defmethod tail-transform-expr (expr tail-exprs layout &aux vars)
  (let ((first-form (updating-transform expr layout vars)))
    (make-lets vars `(progn 
      ,first-form
      ,(tail-transform-exprs tail-exprs layout)))))

(defmethod tail-transform-pattern (patt tail-patts tail-exprs specimen-form ejector-spec layout &aux vars)
  (let ((first-form (updating-transform-pattern patt specimen-form ejector-spec layout vars)))
    (make-lets vars `(progn 
      ,first-form
      ,(tail-transform-patterns tail-patts tail-exprs specimen-form ejector-spec layout)))))

(defun tail-transform-exprs (exprs layout)
  (if exprs
    (if (rest exprs)
      (tail-transform-expr (first exprs) (rest exprs) layout)
      (multiple-value-bind (vars new-layout form) (transform (first exprs) layout)
        (declare (ignore new-layout))
        (make-lets vars form)))
    `(progn)))

(defun tail-transform-patterns (patterns exprs specimen-form ejector-spec layout)
  (if patterns
    (tail-transform-pattern (first patterns) (rest patterns) exprs specimen-form ejector-spec layout)
    (tail-transform-exprs exprs layout)))

; --- transformation ---

(def-expr-transformation |AssignExpr| (layout noun rValue &aux (vars ()))
  (assert (typep noun '|NounExpr|))
  (let ((code `(let ((r-value ,(updating-transform rValue layout vars)))
                ,(binding-set-code (scope-layout-noun-binding layout (car (node-elements noun)))
                                   'r-value)
                r-value)))
    (values vars layout code)))

(def-expr-transformation |CallExpr| (layout recip verb &rest args &aux (vars ()))
  (let ((code `(e. ,(updating-transform recip layout vars)
                   ,verb
                   ,@(loop for x in args collect (updating-transform x layout vars)))))
  (values vars layout code)))

(def-expr-transformation |CatchExpr| (outer-layout attempt pattern catcher)
  (let* ((pattern-eject-block (gensym "E-CATCH-PATTERN-BLOCK-"))
         (condition-sym (gensym "E-CATCH-CONDITION-"))
         (attempt-vars ())
         (catcher-vars ())
         (attempt-layout outer-layout)
         (catcher-layout outer-layout)
         (attempt-code (updating-transform attempt attempt-layout attempt-vars))
         (pattern-code (updating-transform-pattern pattern `(transform-condition-for-e-catch ,condition-sym) `(eject-function "catch-pattern" (lambda (v) (return-from ,pattern-eject-block v))) catcher-layout catcher-vars))
         (catcher-code (updating-transform catcher catcher-layout catcher-vars))
         (attempt-block-sym (gensym "E-CATCH-BLOCK-")))
    (values () outer-layout 
      `(block ,attempt-block-sym
        (handler-case
          ,(make-lets attempt-vars attempt-code)
          (e-catchable-condition (,condition-sym)
            ,(make-lets catcher-vars
              `(progn
                (block ,pattern-eject-block
                  ,pattern-code
                  (return-from ,attempt-block-sym 
                    ,catcher-code))
                ; If we reach here, then the pattern did not match.
                (catch-expr-resignal ,condition-sym)))))))))

(def-expr-transformation |DefineExpr| (layout patt rValue opt-ejector-expr &aux (vars ()))
  ; XXX statically reject references to pattern vars in rvalue/ejector
  (let* ((rvalue-code (updating-transform rValue layout vars))
         (ejector-sym (gensym "DEFEJ"))
         (ejector-form (when opt-ejector-expr (updating-transform opt-ejector-expr layout vars)))
         (patt-code (updating-transform-pattern patt 'define-specimen (list 'ejector ejector-sym) layout vars)))
    (values vars layout 
      `(let ((define-specimen ,rvalue-code)
             (,ejector-sym ,ejector-form))
        (declare (ignorable ,ejector-sym))
        ,patt-code 
        define-specimen))))

(defmethod tail-transform-expr ((expr |DefineExpr|) tail-exprs layout &aux vars)
  (destructuring-bind (patt rValue opt-ejector-expr) (node-elements expr)
    (let* ((rvalue-code (updating-transform rValue layout vars))
           (ejector-sym (gensym "DEFEJ"))
           (ejector-form (when opt-ejector-expr (updating-transform opt-ejector-expr layout vars)))
           (patt-code (tail-transform-patterns
                        (list patt)
                        tail-exprs
                        `(progn
                          (setf ,ejector-sym ,ejector-form)
                          ,rvalue-code)
                        (list 'ejector ejector-sym)
                        layout)))
      `(let ((,ejector-sym #:missing-ejector))
        (declare (ignorable ,ejector-sym))
        ,(make-lets vars patt-code)))))

(def-expr-transformation |EscapeExpr| (outer-layout hatch body opt-arg-pattern opt-catcher)
  (assert (eql (null opt-arg-pattern) (null opt-catcher)))
  (let* ((escape-layout outer-layout)
         (catch-layout outer-layout)
         (escape-vars ())
         (catch-vars ())
         (body-block (gensym "ESCAPE-CATCH-BODY-BLOCK-"))
         (block-sym (gensym "ESCAPE-CATCH-OUTER-BLOCK-"))
         (catch-value-var (gensym "ESCAPE-CATCH-VALUE-"))
         (hatch-code (updating-transform-pattern hatch
                       `(ejector ',(pattern-opt-noun hatch) (lambda (v) (return-from ,body-block v))) 'nil
                       escape-layout
                       escape-vars))
         (body-code (updating-transform body escape-layout escape-vars))
         (catch-patt-code (when opt-arg-pattern (updating-transform-pattern opt-arg-pattern catch-value-var 'nil catch-layout catch-vars)))
         (catch-body-code (when opt-catcher (updating-transform opt-catcher catch-layout catch-vars))))
    (if opt-catcher
      (values () outer-layout
        `(block ,block-sym
          ,(make-lets escape-vars
            `(let ((,catch-value-var 
                    (block ,body-block 
                      ,hatch-code
                      (return-from ,block-sym ,body-code))))
              ,(make-lets catch-vars
                `(progn
                  ,catch-patt-code
                  ,catch-body-code))))))
      (scope-box
        escape-vars
        outer-layout
        `(block ,body-block
          ,hatch-code
          ,body-code)))))

(def-expr-transformation |FinallyExpr| (layout attempt unwinder)
  (let* ((attempt-code (nth-value 2 (scope-box-transform layout attempt)))
         (unwinder-code (nth-value 2 (scope-box-transform layout unwinder))))
    (values () layout 
      `(unwind-protect
        ,attempt-code
        ,unwinder-code))))

(def-expr-transformation |HideExpr| (layout sub) 
  (scope-box-transform layout sub))

(def-expr-transformation |IfExpr| (outer-layout test then else)
  (let* ((block-sym (gensym))
         (then-layout (scope-layout-nest outer-layout))
         (else-layout (scope-layout-nest outer-layout))
         (test-vars ())
         (test-code (updating-transform test then-layout test-vars))
         (then-vars test-vars)
         (then-code (updating-transform then then-layout then-vars))
         (else-vars ())
         (else-code (updating-transform else else-layout else-vars)))
    (values () outer-layout
      (if (null test-vars)
        ; simple case - the test contains no variable definitions that need to be carried to the then-branch
        `(if (e-is-true ,test-code)
          ,(make-lets then-vars then-code) 
          ,(make-lets else-vars else-code))
        ; complex case
        `(block ,block-sym
          ,(make-lets then-vars `(if (e-is-true ,test-code)
                                  (return-from ,block-sym ,then-code))) 
          ,(make-lets else-vars else-code)))
      )))

(def-expr-transformation |LiteralExpr| (layout data)
  (values () layout `',data))

(def-expr-transformation |MatchBindExpr| (layout specimen pattern &aux
    specimen-vars pattern-vars)
  (let* ((pattern-eject-block (gensym "E-CATCH-PATTERN-BLOCK-"))
         (problem-var (make-symbol "PROBLEM"))
         (broken-ref-var (make-symbol "BROKEN-REF"))
         (specimen-code (updating-transform specimen layout specimen-vars))
         (patt-code (updating-transform-pattern pattern specimen-code `(eject-function "match-bind" (lambda (v) (return-from ,pattern-eject-block v))) layout pattern-vars)))
    (values (concatenate 'list specimen-vars pattern-vars) layout 
      `(block match-bind
        (let* ((,problem-var (block ,pattern-eject-block
                               ,patt-code
                               ; if we reach here, the pattern matched
                               (return-from match-bind ,elib:+e-true+)))
               ,@(when pattern-vars
                   `((,broken-ref-var (elib:make-unconnected-ref ,problem-var)))))
          (declare (ignorable ,problem-var))
          ; if we reach here, the ejector was used
          ; all variables defined by the pattern become broken references
          ,@(loop for binding in pattern-vars
                  collect (binding-smash-code binding broken-ref-var))
          elib:+e-false+)))))


(def-expr-transformation |MetaContextExpr| (layout
    &aux (prefix (scope-layout-fqn-prefix layout)))
  (values () layout `',(e-lambda "org.erights.e.elang.scope.StaticContext" ()
    (:|__printOn| (tw)
      (e-coercef tw +the-text-writer-guard+)
      (e. tw |print| "<static context>"))
    (:|getFQNPrefix| ()
      prefix))))

(def-expr-transformation |MetaStateExpr| (layout)
  (values () layout
    `(e. ',+the-make-const-map+ |fromPairs|
      (vector ,@(scope-layout-meta-state-bindings layout)))))

(def-expr-transformation |NounExpr| (layout varName)
  (values () layout (binding-get-code (scope-layout-noun-binding layout varName))))

(defglobal +classic-object-generators+
  '(:method-body  method-body
    :matcher-body matcher-body))

(def-expr-transformation |ObjectExpr| (layout doc-comment qualified-name auditor-exprs script
    &aux vars
         (this-expr (make-instance '|ObjectExpr| :elements (list doc-comment qualified-name auditor-exprs script)))) ; XXX need a this argument
  (let* ((fqn (updating-fully-qualify-name layout qualified-name))
         (auditor-forms 
           (loop for auditor-expr across (coerce auditor-exprs 'vector) collect
             `(updating-transform auditor-expr layout vars))))
    ;(prin1 (type-of auditor-exprs))
    (values vars layout
      (object-form +classic-object-generators+ layout this-expr doc-comment auditor-forms script fqn))))

(def-expr-transformation |SeqExpr| (layout &rest subs &aux (vars ()))
  (let ((form `(progn ,@(mapcar (lambda (x) (updating-transform x layout vars)) subs))))
    (values vars layout form)))

(defmethod tail-transform-expr ((expr |SeqExpr|) tail-exprs layout)
  (tail-transform-exprs (append (node-elements expr) tail-exprs) layout))


(def-expr-transformation |SlotExpr| (layout noun-expr)
  (assert (typep noun-expr '|NounExpr|))
  (values () layout (binding-get-slot-code (scope-layout-noun-binding layout (car (node-elements noun-expr))))))


(def-patt-transformation |CdrPattern| (specimen-form ejector-spec layout list-pattern rest-pattern &aux (vars ()))
  (let* ((coerced-sym (gensym "VECTOR"))
         (list-patt-length (length (node-elements list-pattern)))
         (list-patt-code (updating-transform-pattern list-pattern `(subseq ,coerced-sym 0 ,list-patt-length) ejector-spec layout vars))
         (rest-patt-code (updating-transform-pattern rest-pattern `(subseq ,coerced-sym ,list-patt-length) ejector-spec layout vars)))
    (values vars layout
      `(let ((,coerced-sym (e-coerce-native ,specimen-form 'vector ,(opt-ejector-make-code ejector-spec))))
        (unless (>= (length ,coerced-sym) ,list-patt-length)
          ,(eject-code ejector-spec `(make-condition 'simple-error
                                                     :format-control "a ~A size list doesn't match a >= ~A size list pattern"
                                                     :format-arguments (list (length ,coerced-sym) ,list-patt-length))))
        (progn
          ,list-patt-code
          ,rest-patt-code)))))

(defun var-binding-impl (opt-guard-code body)
  ;; xxx watch this for a SBCL ctor leak?
  (if opt-guard-code
    `(make-instance 'elib:e-guarded-slot :value ,body :guard ,opt-guard-code)
    `(make-instance 'elib:e-var-slot :value ,body)))

(defun final-binding-impl (opt-guard-code body)
  (assert (null opt-guard-code))
  `(make-instance 'elib:e-simple-slot :value ,body))

(defun slot-binding-impl (opt-guard-code body)
  (assert (null opt-guard-code))
  body)

(defun binding-pattern-impl (slot-expr-func coerce-immediately specimen-form ejector-spec layout nounExpr optGuardExpr
    &aux (vars ()) sym)
  (let ((guard-code (if (not (null optGuardExpr))
                      (updating-transform optGuardExpr layout vars))))
    (setf (values sym layout) (pick-slot-symbol (car (node-elements nounExpr)) layout))
    (values 
      (cons sym vars)
      layout
      `(setf ,sym ,(funcall slot-expr-func
        (if (not coerce-immediately)
          guard-code)
        (if (and guard-code coerce-immediately)
          `(e. ,guard-code |coerce|
                           ,specimen-form
                           ,(opt-ejector-make-code ejector-spec))
          specimen-form))))))


; old code for FinalPattern: (binding-pattern-impl #'final-binding-impl t specimen-form ejector-spec layout nounExpr optGuardExpr)
; XXX throw out final-binding-impl; review other binding kinds for similar optimizations

(def-patt-transformation |FinalPattern| (specimen-form ejector-spec layout nounExpr optGuardExpr &aux vars)
  (let ((guard-code (if (not (null optGuardExpr))
                      (updating-transform optGuardExpr layout vars)))
        (binding nil))
    (setf (values binding layout) (pick-binding-symbol (car (node-elements nounExpr)) layout))
    (values (cons binding vars) layout 
      `(setf ,(binding-get-code binding)
                  ,(if guard-code
                    `(e. ,guard-code |coerce|
                                     ,specimen-form
                                     ,(opt-ejector-make-code ejector-spec))
                    specimen-form)))))

(defmethod tail-transform-pattern ((patt |FinalPattern|) tail-patts tail-exprs specimen-form ejector-spec layout &aux vars)
  (destructuring-bind (nounExpr optGuardExpr) (node-elements patt)
  (let ((guard-code (if (not (null optGuardExpr))
                      (updating-transform optGuardExpr layout vars)))
        (binding nil))
    (setf (values binding layout) (pick-binding-symbol (car (node-elements nounExpr)) layout))
    (make-lets vars
      `(let ((,(binding-get-code binding)
                ,(if guard-code
                  `(e. ,guard-code |coerce|
                                   ,specimen-form
                                   ,(opt-ejector-make-code ejector-spec))
                  specimen-form)))
        ,(tail-transform-patterns tail-patts tail-exprs specimen-form ejector-spec layout))))))

(def-patt-transformation |IgnorePattern| (specimen-form ejector-spec layout)
  (values () layout specimen-form))

(def-patt-transformation |ListPattern| (specimen-form ejector-spec layout &rest subs &aux (vars ()))
  (let* ((pattern-arity (length subs))
         (sub-forms (loop for i below pattern-arity
                          and sub in subs
                          collect (updating-transform-pattern sub `(aref slist ,i) ejector-spec layout vars))))
    (values vars layout 
      `(let ((slist (e-coerce-native ,specimen-form 'vector ,(opt-ejector-make-code ejector-spec))))
        (unless (eql (length slist) ,pattern-arity)
          ,(eject-code ejector-spec `(make-condition 'simple-error
                                                     :format-control "a ~A size list doesn't match a ~A size list pattern"
                                                     :format-arguments (list (length slist) ,pattern-arity))))
        ,@sub-forms))))

(def-patt-transformation |SlotPattern| (specimen-form ejector-spec layout &rest args)
  (apply #'binding-pattern-impl #'slot-binding-impl t specimen-form ejector-spec layout args))

(def-patt-transformation |SuchThatPattern| (specimen-form ejector-spec layout sub-patt test &aux (vars ()))
  (let* ((patt-code (updating-transform-pattern sub-patt specimen-form ejector-spec layout vars))
         (test-code (updating-transform test layout vars)))
    (values vars layout
      `(progn
        ,patt-code
        (if (not (e-is-true ,test-code)) 
          ,(eject-code ejector-spec '(make-condition 'simple-error :format-control "such-that expression was false")))))))

(def-patt-transformation |VarPattern| (specimen-form ejector-spec layout &rest args)
  (apply #'binding-pattern-impl #'var-binding-impl nil specimen-form ejector-spec layout args))

;;; --- ObjectExpr component functions ---

(defun method-body (layout method args-var &aux vars)
  (destructuring-bind (docComment verb patterns optResultGuard body) (node-elements method)
    (declare (ignore docComment verb))
    (setf layout (scope-layout-nest layout))
    (let* ((arg-symbols (loop for i below (length patterns)
                              collect (intern (format nil "arg~A" i))))
           (code `(destructuring-bind (,@arg-symbols) ,args-var
                    ,@(loop for pat across (coerce patterns 'vector)
                            and sym in arg-symbols
                            collect (updating-transform-pattern pat sym nil layout vars))
                    ,(if (null optResultGuard) 
                       (updating-transform body layout vars)
                       `(e. ,(updating-transform optResultGuard layout vars) |coerce| ,(updating-transform body layout vars) nil)
                       ))))
      (make-lets vars code))))

(defun matcher-body (layout matcher mverb-var args-var remaining-code
    &aux vars)
  (destructuring-bind (pattern body) (node-elements matcher)
    (setf layout (scope-layout-nest layout))
    (let* ((pattern-eject-block (gensym "E-CATCH-PATTERN-BLOCK-"))
           (patt-code (updating-transform-pattern pattern `(vector (e-util:unmangle-verb ,mverb-var) (coerce ,args-var 'vector)) `(eject-function ,(format nil "~A matcher" (scope-layout-fqn-prefix layout)) (lambda (v) (return-from ,pattern-eject-block v))) layout vars))
           (body-code (updating-transform body layout vars)))
      `(block method-matcher
        ,(make-lets vars
          `(block ,pattern-eject-block
            ,patt-code
            ; if we reach here, the pattern matched
            (return-from 
              method-matcher
              ,body-code)))
        ; if we reach here, the ejector was used
        ,remaining-code))))

;;; --- binding operations ---

(defgeneric binding-let-entry     (binding)
  (:documentation "Return the LET clause which must be scoped outside of all uses of this binding."))

(defgeneric binding-smash-code    (binding broken-ref-form)
  (:documentation "Return a form which should be evaluated iff the binding is to remain in scope without its ordinary initialization code running. This is used to implement MatchBindExpr."))


(defmethod binding-let-entry ((binding symbol))
  `(,binding ,+the-unset-slot+))

(defmethod binding-smash-code ((binding symbol) broken-ref-form)
  `(setf ,binding (make-instance 'elib:e-simple-slot :value ,broken-ref-form)))
  

(defmethod binding-let-entry ((binding direct-def-binding))
  `(,(binding-get-code binding) ',(elib:make-unconnected-ref "accidentally unbound direct binding")))

(defmethod binding-smash-code ((binding direct-def-binding) broken-ref-form)
  `(setf ,(binding-get-code binding) ,broken-ref-form))


(defmethod binding-let-entry ((binding direct-var-binding))
  `(,(%var-binding-symbol binding) ',(elib:make-unconnected-ref "accidentally unset direct var binding")))

(defmethod binding-smash-code ((binding direct-var-binding) broken-ref-form)
  (error "not implemented"))


(defclass e-unset-slot () ())

(def-vtable e-unset-slot
  (:|getValue| (this)
    (declare (ignore this))
    (error "internal error: slot variable never assigned"))
  (:|setValue| (this new-value)
    (declare (ignore this new-value))
    (error "internal error: slot variable never assigned")))

(defglobal +the-unset-slot+ (make-instance 'e-unset-slot))