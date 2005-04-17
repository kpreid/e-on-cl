; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

; --- transformation utils ---

(defmacro def-expr-transformation (class (name-map-var &rest elements-lambda-list) &body body
    &aux (node-var (gensym)))
  `(defmethod transform ((,node-var ,class) ,name-map-var)
    (destructuring-bind ,elements-lambda-list (node-elements ,node-var) 
      ,@body)))

(defmacro def-patt-transformation (class (specimen-form-var ejector-spec-var name-map-var &rest elements-lambda-list) &body body
    &aux (node-var (gensym)))
  `(defmethod transform-pattern ((,node-var ,class) ,specimen-form-var ,ejector-spec-var ,name-map-var)
    (declare (ignorable ,ejector-spec-var)) ; because declare ignore doesn't work when nested
    (destructuring-bind ,elements-lambda-list (node-elements ,node-var) 
      ,@body)))


(defgeneric tail-transform-expr (expr tail-exprs name-map))
(defgeneric tail-transform-pattern (patt tail-patts tail-exprs specimen-form ejector-spec name-map))

(defmethod tail-transform-expr (expr tail-exprs name-map &aux vars)
  (let ((first-form (updating-transform expr name-map vars)))
    (make-lets vars `(progn 
      ,first-form
      ,(tail-transform-exprs tail-exprs name-map)))))

(defmethod tail-transform-pattern (patt tail-patts tail-exprs specimen-form ejector-spec name-map &aux vars)
  (let ((first-form (updating-transform-pattern patt specimen-form ejector-spec name-map vars)))
    (make-lets vars `(progn 
      ,first-form
      ,(tail-transform-patterns tail-patts tail-exprs specimen-form ejector-spec name-map)))))

(defun tail-transform-exprs (exprs name-map)
  (if exprs
    (if (rest exprs)
      (tail-transform-expr (first exprs) (rest exprs) name-map)
      (multiple-value-bind (vars new-name-map form) (transform (first exprs) name-map)
        (declare (ignore new-name-map))
        (make-lets vars form)))
    `(progn)))

(defun tail-transform-patterns (patterns exprs specimen-form ejector-spec name-map)
  (if patterns
    (tail-transform-pattern (first patterns) (rest patterns) exprs specimen-form ejector-spec name-map)
    (tail-transform-exprs exprs name-map)))

; --- transformation ---

(def-expr-transformation |AssignExpr| (name-map noun rValue &aux (vars ()))
  (assert (typep noun '|NounExpr|))
  (let ((code `(let ((r-value ,(updating-transform rValue name-map vars)))
                ,(binding-set-code (inner-scope-noun-binding name-map (car (node-elements noun)))
                                   'r-value)
                r-value)))
    (values vars name-map code)))

(def-expr-transformation |CallExpr| (name-map recip verb &rest args &aux (vars ()))
  (let ((code `(e. ,(updating-transform recip name-map vars)
                   ,verb
                   ,@(loop for x in args collect (updating-transform x name-map vars)))))
  (values vars name-map code)))

(defun catch-expr-resignal (condition)
  "This function exists to make it clear in a backtrace that the condition did not originate at this location."
  (error condition))

(def-expr-transformation |CatchExpr| (outer-name-map attempt pattern catcher)
  (let* ((catch-tag-sym (gensym "E-CATCH-EJECTOR-TAG-VAR-"))
         (condition-sym (gensym "E-CATCH-CONDITION-"))
         (attempt-vars ())
         (catcher-vars ())
         (attempt-name-map outer-name-map)
         (catcher-name-map outer-name-map)
         (attempt-code (updating-transform attempt attempt-name-map attempt-vars))
         (pattern-code (updating-transform-pattern pattern `(transform-condition-for-e-catch ,condition-sym) `(catch-tag ,catch-tag-sym) catcher-name-map catcher-vars))
         (catcher-code (updating-transform catcher catcher-name-map catcher-vars))
         (attempt-block-sym (gensym "E-CATCH-BLOCK-")))
    (values () outer-name-map 
      `(block ,attempt-block-sym
        (handler-case
          ,(make-lets attempt-vars attempt-code)
          (e-catchable-condition (,condition-sym)
            ,(make-lets catcher-vars
              `(let ((,catch-tag-sym (gensym "E-CATCH-EJECTOR-TAG-")))
                (catch ,catch-tag-sym
                  ,pattern-code
                  (return-from ,attempt-block-sym 
                    ,catcher-code))
                ; If we reach here, then the pattern did not match.
                (catch-expr-resignal ,condition-sym)))))))))
 
; broken version: executes the catch block before unwind-protect/finally         
;(def-expr-transformation |CatchExpr| (outer-name-map attempt pattern catcher)
;  (let* ((catch-tag-sym (gensym "E-CATCH-EJECTOR-TAG-VAR-"))
;         (condition-sym (gensym "E-CATCH-CONDITION-"))
;         (attempt-vars ())
;         (catcher-vars ())
;         (attempt-name-map outer-name-map)
;         (catcher-name-map outer-name-map)
;         (attempt-code (updating-transform attempt attempt-name-map attempt-vars))
;         (pattern-code (updating-transform-pattern pattern condition-sym `(catch-tag ,catch-tag-sym) catcher-name-map catcher-vars))
;         (catcher-code (updating-transform catcher catcher-name-map catcher-vars))
;         (attempt-block-sym (gensym "E-CATCH-BLOCK-")))
;    (values () outer-name-map 
;      `(block ,attempt-block-sym
;        (handler-bind
;          ; XXX is 'error the right type specifier?
;          ; We don't want to handle non-exceptionish nonlocal exits.
;          ; We don't want to handle nondeterministic errors, if possible.
;          ; We do want to handle conditions signaled by translated E code.
;          ; We do want to handle ordinarily-fatal conditions signaled by CL stuff E code calls.
;          ((error #'(lambda (,condition-sym)
;            ,(make-lets catcher-vars
;              `(let ((,catch-tag-sym (gensym "E-CATCH-EJECTOR-TAG-")))
;                (catch ,catch-tag-sym
;                  ,pattern-code
;                  (return-from ,attempt-block-sym ,catcher-code)))))))
;          ,(make-lets attempt-vars attempt-code))))))

(def-expr-transformation |DefineExpr| (name-map patt rValue &aux (vars ()))
  (let ((rvalue-code (updating-transform rValue name-map vars))
        (patt-code (updating-transform-pattern patt 'define-specimen 'nil name-map vars)))
    (values vars name-map `(let ((define-specimen ,rvalue-code)) ,patt-code define-specimen))))

(defmethod tail-transform-expr ((expr |DefineExpr|) tail-exprs name-map &aux vars)
  (destructuring-bind (patt rValue) (node-elements expr)
    (let* ((rvalue-code (updating-transform rValue name-map vars))
           (patt-code (tail-transform-patterns (list patt) tail-exprs rvalue-code 'nil name-map)))
      (make-lets vars patt-code))))

(def-expr-transformation |EscapeExpr| (outer-name-map hatch body opt-arg-pattern opt-catcher)
  (assert (eql (null opt-arg-pattern) (null opt-catcher)))
  (let* ((escape-name-map outer-name-map)
         (catch-name-map outer-name-map)
         (escape-vars ())
         (catch-vars ())
         (etag-var (gensym "ESCAPE-TAG-"))
         (block-sym (gensym "ESCAPE-CATCH-BLOCK-"))
         (catch-value-var (gensym "ESCAPE-CATCH-VALUE-"))
         (hatch-code (updating-transform-pattern hatch
                       `(make-instance 'elib:ejector 
                         :catch-tag ,etag-var
                         :label ',(pattern-opt-noun hatch)) 'nil
                       escape-name-map
                       escape-vars))
         (body-code (updating-transform body escape-name-map escape-vars))
         (catch-patt-code (when opt-arg-pattern (updating-transform-pattern opt-arg-pattern catch-value-var 'nil catch-name-map catch-vars)))
         (catch-body-code (when opt-catcher (updating-transform opt-catcher catch-name-map catch-vars))))
    (if opt-catcher
      (values () outer-name-map
        `(block ,block-sym
          ,(make-lets escape-vars
            `(let ((,etag-var (make-symbol ,(write-to-string hatch))))
              ,hatch-code
              (let ((,catch-value-var 
                      (catch ,etag-var (return-from ,block-sym ,body-code))))
                ,(make-lets catch-vars
                  `(progn
                    ,catch-patt-code
                    ,catch-body-code)))))))
      (scope-box
        escape-vars
        outer-name-map
        `(let ((,etag-var (make-symbol ,(write-to-string hatch))))
          ,hatch-code
          (catch ,etag-var
            ,body-code))))))

(def-expr-transformation |FinallyExpr| (name-map attempt unwinder)
  (let* ((attempt-code (nth-value 2 (scope-box-transform name-map attempt)))
         (unwinder-code (nth-value 2 (scope-box-transform name-map unwinder))))
    (values () name-map 
      `(unwind-protect
        ,attempt-code
        ,unwinder-code))))

(def-expr-transformation |HideExpr| (name-map sub) 
  (scope-box-transform name-map sub))

(def-expr-transformation |IfExpr| (outer-name-map test then else)
  (let* ((block-sym (gensym))
         (then-name-map (inner-scope-nest outer-name-map))
         (else-name-map (inner-scope-nest outer-name-map))
         (test-vars ())
         (test-code (updating-transform test then-name-map test-vars))
         (then-vars test-vars)
         (then-code (updating-transform then then-name-map then-vars))
         (else-vars ())
         (else-code (updating-transform else else-name-map else-vars)))
    (values () outer-name-map
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

(def-expr-transformation |LiteralExpr| (onm data)
  (values () onm `',data))

(def-expr-transformation |MatchBindExpr| (name-map specimen pattern &aux
    specimen-vars pattern-vars)
  (let* ((tag-sym (gensym "E-MATCH-BIND-TAG-VAR-"))
         (problem-var (make-symbol "PROBLEM"))
         (broken-ref-var (make-symbol "BROKEN-REF"))
         (specimen-code (updating-transform specimen name-map specimen-vars))
         (patt-code (updating-transform-pattern pattern specimen-code `(catch-tag ,tag-sym) name-map pattern-vars)))
    (values (concatenate 'list specimen-vars pattern-vars) name-map 
      `(block match-bind
        (let ((,tag-sym (gensym "E-MATCH-BIND-TAG-"))) 
          (let* ((,problem-var (catch ,tag-sym
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
            elib:+e-false+))))))


(def-expr-transformation |MetaContextExpr| (onm
    &aux (prefix (inner-scope-fqn-prefix onm)))
  (values () onm `',(e-named-lambda "org.erights.e.elang.scope.StaticContext"
    (:|__printOn/1| (tw)
      (e-coercef tw +the-text-writer-guard+)
      (e. tw |print| "<static context>"))
    (:|getFQNPrefix/0| ()
      prefix))))

(def-expr-transformation |MetaStateExpr| (onm)
  (values () onm
    `(e. ',+the-make-const-map+ |fromPairs|
      (vector ,@(inner-scope-meta-state-bindings onm)))))

(def-expr-transformation |NounExpr| (onm varName)
  (values () onm (binding-get-code (inner-scope-noun-binding onm varName))))

(defun method-body (name-map method args-var &aux vars)
  (destructuring-bind (docComment verb patterns optResultGuard body) (node-elements method)
    (declare (ignore docComment verb))
    (setf name-map (inner-scope-nest name-map))
    (let* ((arg-symbols (loop for i below (length patterns)
                              collect (intern (format nil "arg~A" i))))
           (code `(destructuring-bind (,@arg-symbols) ,args-var
                    ,@(loop for pat across (coerce patterns 'vector)
                            and sym in arg-symbols
                            collect (updating-transform-pattern pat sym nil name-map vars))
                    ,(if (null optResultGuard) 
                       (updating-transform body name-map vars)
                       `(e. ,(updating-transform optResultGuard name-map vars) |coerce| ,(updating-transform body name-map vars) nil)
                       ))))
      (make-lets vars code))))

(defun miranda-case-maybe (mverb methods &rest body)
  (unless (find-if (lambda (method &aux (e (node-elements method)))
                     (eq mverb (e-util:mangle-verb (cadr e) (length (caddr e)))))
                   methods)
    `(((,mverb) ,@body))))

(defun opt-matcher-body (name-map matcher remaining-code
    &aux vars)
  (if (null matcher)
    remaining-code
    (destructuring-bind (pattern body) (node-elements matcher)
      (setf name-map (inner-scope-nest name-map))
      (let* ((tag-sym (gensym "E-MATCH-TAG-VAR-"))
             (patt-code (updating-transform-pattern pattern `(vector (e-util:unmangle-verb mverb) (coerce args 'vector)) `(catch-tag ,tag-sym) name-map vars))
             (body-code (updating-transform body name-map vars)))
        `(block method-matcher
          (let ((,tag-sym (gensym "E-MATCH-TAG-"))) 
            ,(make-lets vars
              `(catch ,tag-sym
                ,patt-code
                ; if we reach here, the pattern matched
                (return-from 
                  method-matcher
                  ,body-code)))
            ; if we reach here, the ejector was used
            ,remaining-code))))))

(defun methodical-object-body (self-fsym name-map methods matcher qualified-name approvers-sym doc-comment
    &aux (simple-name (simplify-fq-name qualified-name))
         (type-desc
           (e. +the-make-type-desc+ |run|
             doc-comment
             qualified-name
             #()
             #()
             (concatenate 'vector
               (map 'vector (lambda (x) (aref x 1))
                 ; XXX now that the TypeDesc constructor has changed, reconsider this: e.g. maybe TypeDesc's CL-level initarg should take the list instead of map, and so message-pairs-to-map-including-miranda-messages gets changed to be non-pairs; also perhaps this type desc should be made with make-instance
                 elib::+miranda-message-entries+)
               (loop for method across methods
                 for (doc-comment verb patterns opt-result-guard nil) = (node-elements method)
                 collect
                   (make-instance 'message-desc
                     :verb verb
                     :doc-comment doc-comment
                     :params (map 'vector #'pattern-to-param-desc patterns)
                     :opt-result-guard (opt-guard-expr-to-safe-opt-guard opt-result-guard)))))))
  `(case mverb
    ,@(loop for method across methods
            for (nil verb patterns nil nil) = (node-elements method)
            collect
              `((,(e-util:mangle-verb verb (length patterns)))
                ,(method-body name-map method 'args)))
    ,@(miranda-case-maybe :|__printOn/1| methods `(destructuring-bind (tw) args
      (e. (e-coerce tw +the-text-writer-guard+) |print| "<" ',simple-name ">")))
    ,@(miranda-case-maybe :|__getAllegedType/0| methods `(destructuring-bind () args
      ',type-desc))
    ,@(miranda-case-maybe :|__respondsTo/2| methods `(destructuring-bind (verb arity) args
      (as-e-boolean
        (or (member (e-util:mangle-verb verb arity)
                  ',(loop for method across methods
                          for (nil verb patterns nil nil) = (node-elements method)
            collect (e-util:mangle-verb verb (length patterns))))
            (e-is-true (elib:miranda #',self-fsym mverb args nil))))))
    ((elib:audited-by-magic-verb) (destructuring-bind (auditor) args
      (if (position auditor ,approvers-sym :test #'elib:eeq-is-same-ever) t)))
    (otherwise 
      (elib:miranda #',self-fsym mverb args (lambda ()
        ,(opt-matcher-body name-map matcher
          `(error "No such method: ~W#~A" ',qualified-name mverb)))))))

(defun plumbing-object-body (self-fsym name-map methods matcher qualified-name approvers-sym doc-comment)
  (declare (ignore self-fsym methods doc-comment))
  `(case mverb
    ; XXX remove duplication of audited-by-magic-verb implementation
    ((elib:audited-by-magic-verb) (destructuring-bind (auditor) args
      (if (position auditor ,approvers-sym :test #'elib:eeq-is-same-ever) t)))
    ; XXX should propagate match failure exception instead
    (otherwise 
      ,(opt-matcher-body name-map matcher
        `(error "Plumbing match failure: ~W#~A" ',qualified-name mverb)))))

(defmacro updating-fully-qualify-name (inner-scope-place qn-form
    &aux (qn-var (gensym)) (scope-var (gensym)))
  ; This is a macro because it will need to update the inner scope once inner scopes have 'anonymos object serial number' information
  `(let ((,qn-var ,qn-form)
         (,scope-var ,inner-scope-place))
    (cond
      ((string= ,qn-var "_") ; XXX numeric tag
        (concatenate 'string (inner-scope-fqn-prefix ,scope-var) "_"))
      ((and (string/= ,qn-var "") (string= ,qn-var "$" :end1 1))
        (concatenate 'string (inner-scope-fqn-prefix ,scope-var) (subseq ,qn-var 1)))
      (t
        ; XXX the error case comes from Java-E structure (separated isFQName). we should find out why Java-E bothers separating it, and decide whether to use the same structure here
        ,qn-var
        #-(and) (error "Unrecognized qualified name: ~A (being qualified with prefix ~A)" (e-quote ,qn-var) (e-quote (inner-scope-fqn-prefix ,scope-var)))))))

(def-expr-transformation |ObjectExpr| (name-map doc-comment qualified-name auditor-exprs eScript
    &aux vars
         (this-expr (make-instance '|ObjectExpr| :elements (list doc-comment qualified-name auditor-exprs eScript)))) ; XXX need a this argument
  (destructuring-bind (methods matcher) (node-elements eScript)
    (let* ((approvers-sym (make-symbol "APPROVERS"))
           (witness-sym (make-symbol "WITNESS"))
           (witness-ok-sym (make-symbol "WITNESS-OK"))
           (witness-check-form
             `(assert ,witness-ok-sym ()
                "~A is out of scope" (e-quote ,witness-sym)))
           (has-auditors (> (length auditor-exprs) 0))
           (auditor-forms 
             (loop for auditor-expr across (coerce auditor-exprs 'vector) collect
               `(funcall ,witness-sym :|ask/1| ,(updating-transform auditor-expr name-map vars))))
           (fqn (updating-fully-qualify-name name-map qualified-name))
           (self-fsym (make-symbol fqn))
           (inner-name-map
             (make-instance 'object-inner-scope
               :nouns (coerce (e. (e. (e. this-expr |staticScope|) |namesUsed|) |getKeys|) 'list)
               :rest (make-instance 'prefix-inner-scope 
                       :fqn-prefix (concatenate 'string fqn "$")
                       :rest name-map))))
      ;(prin1 (type-of auditor-exprs))
      (values vars name-map
        `(let* ((,approvers-sym ())
                ,@(when has-auditors
                  `((,witness-sym nil)
                    (,witness-ok-sym t))))
          ; XXX gack. less setf: the witness is self-referential, so it'll need to be either e-named-lambda-in-LABELS somehow, or a promise
          ,@(when has-auditors
            `((setf ,witness-sym (e-named-lambda "org.erights.e.elang.evm.AuditWitness"
                (:|__printOn/1| (tw)
                  (e-coercef tw +the-text-writer-guard+)
                  (e. tw |print| 
                    "<"
                    (if ,witness-ok-sym "" "closed ")
                    "Witness for "
                    ',(e-util:aan fqn)
                    ">")
                  nil)
                (:|ask/1| (other-auditor)
                  "Audits the object with the given auditor. XXX describe behavior upon false/throw returns from auditor"
                  ,witness-check-form
                  (if (e-is-true (e. other-auditor |audit| ',this-expr ,witness-sym))
                    (push other-auditor ,approvers-sym))
                  nil)
                (:|getSlot/1| (slot-name)
                  "Returns the named slot in the audited object's lexical scope.

XXX This is an excessively large authority and will probably be replaced."
                  ,witness-check-form
                  ; XXX this is a rather big authority to grant auditors - being (eventually required to be) DeepFrozen themselves, they can't extract information, but they can send messages to the slot('s value) to cause undesired effects

                  (e. ,(nth-value 2 (transform (make-instance '|MetaStateExpr| :elements '()) name-map))
                      |get| (e. "&" |add| slot-name))
                  #-(and) (cond
                    ,@(loop for (name . binding) in (inner-scope-bindings name-map) collect
                      `((string= slot-name ,name) ,(binding-get-slot-code binding)))
                    (t (error "no such slot: ~A" slot-name))))))
              ,@auditor-forms))
          (labels ((,self-fsym (mverb &rest args)
                      ,(funcall
                        (if methods
                          #'methodical-object-body
                          #'plumbing-object-body)
                        self-fsym inner-name-map methods matcher fqn approvers-sym doc-comment)))
            ,@(when has-auditors `((setf ,witness-ok-sym nil)))
            #',self-fsym))))))

(def-expr-transformation |SeqExpr| (name-map &rest subs &aux (vars ()))
  (let ((form `(progn ,@(mapcar (lambda (x) (updating-transform x name-map vars)) subs))))
    (values vars name-map form)))

(defmethod tail-transform-expr ((expr |SeqExpr|) tail-exprs name-map)
  (tail-transform-exprs (append (node-elements expr) tail-exprs) name-map))


(def-expr-transformation |SlotExpr| (onm noun-expr)
  (assert (typep noun-expr '|NounExpr|))
  (values () onm (binding-get-slot-code (inner-scope-noun-binding onm (car (node-elements noun-expr))))))


(def-patt-transformation |CdrPattern| (specimen-form ejector-spec name-map list-pattern rest-pattern &aux (vars ()))
  (let* ((coerced-sym (gensym "VECTOR"))
         (list-patt-length (length (node-elements list-pattern)))
         (list-patt-code (updating-transform-pattern list-pattern `(subseq ,coerced-sym 0 ,list-patt-length) ejector-spec name-map vars))
         (rest-patt-code (updating-transform-pattern rest-pattern `(subseq ,coerced-sym ,list-patt-length) ejector-spec name-map vars)))
    (values vars name-map
      `(let ((,coerced-sym (e-coerce-native ,specimen-form 'vector ,(opt-ejector-make-code ejector-spec))))
        (unless (>= (length ,coerced-sym) ,list-patt-length)
          ,(eject-code ejector-spec `(make-condition 'simple-error
                                                     :format-control "a ~A size list doesn't match a >= ~A size list pattern"
                                                     :format-arguments (list (length ,coerced-sym) ,list-patt-length))))
        (progn
          ,list-patt-code
          ,rest-patt-code)))))

(defun var-binding-impl (opt-guard-code body)
  (if opt-guard-code
    `(make-instance 'elib:e-guarded-slot :value ,body :guard ,opt-guard-code)
    `(make-instance 'elib:e-var-slot :value ,body)))

(defun final-binding-impl (opt-guard-code body)
  (assert (null opt-guard-code))
  `(make-instance 'elib:e-simple-slot :value ,body))

(defun slot-binding-impl (opt-guard-code body)
  (assert (null opt-guard-code))
  body)

(defun binding-pattern-impl (slot-expr-func coerce-immediately specimen-form ejector-spec name-map nounExpr optGuardExpr
    &aux (vars ()) sym)
  (let ((guard-code (if (not (null optGuardExpr))
                      (updating-transform optGuardExpr name-map vars))))
    (multiple-value-setq (sym name-map) (pick-slot-symbol (car (node-elements nounExpr)) name-map))
    (values 
      (cons sym vars)
      name-map
      `(setq ,sym ,(funcall slot-expr-func
        (if (not coerce-immediately)
          guard-code)
        (if (and guard-code coerce-immediately)
          `(e. ,guard-code |coerce|
                           ,specimen-form
                           ,(opt-ejector-make-code ejector-spec))
          specimen-form))))))


; old code for FinalPattern: (binding-pattern-impl #'final-binding-impl t specimen-form ejector-spec name-map nounExpr optGuardExpr)
; XXX throw out final-binding-impl; review other binding kinds for simplar optimizations

(def-patt-transformation |FinalPattern| (specimen-form ejector-spec name-map nounExpr optGuardExpr &aux vars)
  (let ((guard-code (if (not (null optGuardExpr))
                      (updating-transform optGuardExpr name-map vars)))
        (binding nil))
    (multiple-value-setq (binding name-map) (pick-binding-symbol (car (node-elements nounExpr)) name-map))
    (values (cons binding vars) name-map 
      `(setq ,(binding-get-code binding)
                  ,(if guard-code
                    `(e. ,guard-code |coerce|
                                     ,specimen-form
                                     ,(opt-ejector-make-code ejector-spec))
                    specimen-form)))))

(defmethod tail-transform-pattern ((patt |FinalPattern|) tail-patts tail-exprs specimen-form ejector-spec name-map &aux vars)
  (destructuring-bind (nounExpr optGuardExpr) (node-elements patt)
  (let ((guard-code (if (not (null optGuardExpr))
                      (updating-transform optGuardExpr name-map vars)))
        (binding nil))
    (multiple-value-setq (binding name-map) (pick-binding-symbol (car (node-elements nounExpr)) name-map))
    (make-lets vars
      `(let ((,(binding-get-code binding)
                ,(if guard-code
                  `(e. ,guard-code |coerce|
                                   ,specimen-form
                                   ,(opt-ejector-make-code ejector-spec))
                  specimen-form)))
        ,(tail-transform-patterns tail-patts tail-exprs specimen-form ejector-spec name-map))))))

(def-patt-transformation |IgnorePattern| (specimen-form ejector-spec name-map)
  (values () name-map specimen-form))

(def-patt-transformation |ListPattern| (specimen-form ejector-spec name-map &rest subs &aux (vars ()))
  (let* ((pattern-arity (length subs))
         (sub-forms (loop for i below pattern-arity
                          and sub in subs
                          collect (updating-transform-pattern sub `(aref slist ,i) ejector-spec name-map vars))))
    (values vars name-map 
      `(let ((slist (e-coerce-native ,specimen-form 'vector ,(opt-ejector-make-code ejector-spec))))
        (unless (eql (length slist) ,pattern-arity)
          ,(eject-code ejector-spec `(make-condition 'simple-error
                                                     :format-control "a ~A size list doesn't match a ~A size list pattern"
                                                     :format-arguments (list (length slist) ,pattern-arity))))
        ,@sub-forms))))

(def-patt-transformation |SlotPattern| (specimen-form ejector-spec name-map &rest args)
  (apply #'binding-pattern-impl #'slot-binding-impl t specimen-form ejector-spec name-map args))

(def-patt-transformation |SuchThatPattern| (specimen-form ejector-spec name-map sub-patt test &aux (vars ()))
  (let* ((patt-code (updating-transform-pattern sub-patt specimen-form ejector-spec name-map vars))
         (test-code (updating-transform test name-map vars)))
    (values vars name-map
      `(progn
        ,patt-code
        (if (not (e-is-true ,test-code)) 
          ,(eject-code ejector-spec '(make-condition 'simple-error :format-control "such-that expression was false")))))))

(def-patt-transformation |VarPattern| (specimen-form ejector-spec name-map &rest args)
  (apply #'binding-pattern-impl #'var-binding-impl nil specimen-form ejector-spec name-map args))

