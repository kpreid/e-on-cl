; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

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

(defun catch-expr-resignal (condition)
  "This function exists to make it clear in a backtrace that the condition did not originate at this location."
  (error condition))

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

(defun miranda-case-maybe (mverb methods &rest body)
  (unless (find-if (lambda (method &aux (e (node-elements method)))
                     (eq mverb (e-util:mangle-verb (cadr e) (length (caddr e)))))
                   methods)
    `(((,mverb) ,@body))))

(defun matcher-body (layout matcher remaining-code
    &aux vars)
  (destructuring-bind (pattern body) (node-elements matcher)
    (setf layout (scope-layout-nest layout))
    (let* ((pattern-eject-block (gensym "E-CATCH-PATTERN-BLOCK-"))
           (patt-code (updating-transform-pattern pattern `(vector (e-util:unmangle-verb mverb) (coerce args 'vector)) `(eject-function ,(format nil "~A matcher" (scope-layout-fqn-prefix layout)) (lambda (v) (return-from ,pattern-eject-block v))) layout vars))
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

(defun matchers-body (layout matchers remaining-code)
  (labels ((m-b-list (matchers)
             (if matchers
               (matcher-body layout 
                             (first matchers) 
                             (m-b-list (rest matchers)))
               remaining-code)))
    (m-b-list (coerce matchers 'list))))

(defun methodical-object-body (self-fsym layout methods matchers qualified-name approvers-sym doc-comment
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
                     :opt-result-guard (opt-guard-expr-to-safe-opt-guard opt-result-guard)))))))
  `(case mverb
    ,@(loop for method across methods
            for (nil verb patterns nil nil) = (node-elements method)
            collect
              `((,(e-util:mangle-verb verb (length patterns)))
                ,(method-body layout method 'args)))
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
        ,(matchers-body layout matchers
          `(error "no such method: ~A#~A" ',qualified-name mverb)))))))

(defun plumbing-object-body (self-fsym layout methods matchers qualified-name approvers-sym doc-comment)
  (declare (ignore self-fsym methods doc-comment))
  `(case mverb
    ; XXX remove duplication of audited-by-magic-verb implementation
    ((elib:audited-by-magic-verb) (destructuring-bind (auditor) args
      (if (position auditor ,approvers-sym :test #'elib:eeq-is-same-ever) t)))
    ; XXX should propagate match failure exception instead
    (otherwise 
      ,(matchers-body layout matchers
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

(def-expr-transformation |ObjectExpr| (layout doc-comment qualified-name auditor-exprs eScript
    &aux vars
         (this-expr (make-instance '|ObjectExpr| :elements (list doc-comment qualified-name auditor-exprs eScript)))) ; XXX need a this argument
  (destructuring-bind (opt-methods matchers) (node-elements eScript)
    (let* ((approvers-sym (make-symbol "APPROVERS"))
           (witness-sym (make-symbol "WITNESS"))
           (witness-ok-sym (make-symbol "WITNESS-OK"))
           (witness-check-form
             `(assert ,witness-ok-sym ()
                "~A is out of scope" (e-quote ,witness-sym)))
           (has-auditors (> (length auditor-exprs) 0))
           (auditor-forms 
             (loop for auditor-expr across (coerce auditor-exprs 'vector) collect
               `(funcall ,witness-sym :|ask/1| ,(updating-transform auditor-expr layout vars))))
           (fqn (updating-fully-qualify-name layout qualified-name))
           (self-fsym (make-symbol fqn))
           (inner-layout
             (make-instance 'object-scope-layout
               :nouns (coerce (e. (e. (e. this-expr |staticScope|) |namesUsed|) |getKeys|) 'list)
               :rest (make-instance 'prefix-scope-layout 
                       :fqn-prefix (concatenate 'string fqn "$")
                       :rest layout))))
      ;(prin1 (type-of auditor-exprs))
      (values vars layout
        `(let* ((,approvers-sym ())
                ,@(when has-auditors
                  `((,witness-sym nil)
                    (,witness-ok-sym t))))
          ; XXX gack. less setf: the witness is self-referential, so it'll need to be either e-lambda-in-LABELS somehow, or a promise
          ,@(when has-auditors
            `((setf ,witness-sym (e-lambda "org.erights.e.elang.evm.AuditWitness" ()
                (:|__printOn| (tw)
                  (e-coercef tw +the-text-writer-guard+)
                  (e. tw |print| 
                    "<"
                    (if ,witness-ok-sym "" "closed ")
                    "Witness for "
                    ',(e-util:aan fqn)
                    ">")
                  nil)
                (:|ask| (other-auditor)
                  "Audits the object with the given auditor. XXX describe behavior upon false/throw returns from auditor"
                  ,witness-check-form
                  (if (e-is-true (e. other-auditor |audit| ',this-expr ,witness-sym))
                    (push other-auditor ,approvers-sym))
                  nil)
                (:|getSlot| (slot-name)
                  "Returns the named slot in the audited object's lexical scope.

XXX This is an excessively large authority and will probably be replaced."
                  ,witness-check-form
                  ; XXX this is a rather big authority to grant auditors - being (eventually required to be) DeepFrozen themselves, they can't extract information, but they can send messages to the slot('s value) to cause undesired effects
                  ;; XXX this is also hugely inefficient, isn't it? we're constructing the full meta.state, and then throwing away all but one element. figure out why the alternate method below was taken out -- lack of hashing?
                  (e. ,(nth-value 2 (transform (make-instance '|MetaStateExpr| :elements '()) layout))
                      |get| (e. "&" |add| slot-name))
                  #-(and) (cond
                    ,@(loop for (name . binding) in (scope-layout-bindings layout) collect
                      `((string= slot-name ,name) ,(binding-get-slot-code binding)))
                    (t (error "no such slot: ~A" slot-name))))))
              ,@auditor-forms))
          (labels ((,self-fsym (mverb &rest args)
                      ,(funcall
                        (if opt-methods
                          #'methodical-object-body
                          #'plumbing-object-body)
                        self-fsym inner-layout opt-methods matchers fqn approvers-sym doc-comment)))
            ,@(when has-auditors `((setf ,witness-ok-sym nil)))
            #',self-fsym))))))

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

