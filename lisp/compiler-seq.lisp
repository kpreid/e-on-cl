; Copyright 2005-2008 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang.compiler.seq)

(defgeneric sequence-expr (node layout result)
  (:documentation "Compile an E expression into a series of LET* binding clauses, and return the clauses and the updated scope layout. The symbol in RESULT will be bound to the return value of the node."))

(defgeneric sequence-patt (node layout specimen ejector-binding)
  (:documentation "Compile an E pattern into a series of LET* binding clauses, and return the clauses and the updated scope layout. The result will evaluate the SPECIMEN form exactly once, unless it is a symbol."))

(defgeneric inline-expr (node layout)
  (:documentation "Compile an E expression into a form. This must not be used if bindings from the expression may be referenced later."))

;; XXX the below two methods are a potential infinite recursion

(defmethod inline-expr (node layout
    &aux (result (gensym "R")))
  "fall to general compilation"
  (sequence-to-form (sequence-expr node layout result) result))

(defmethod sequence-expr (node layout result)
  (if (inlinable node)
    (values `((,result ,(inline-expr node layout)))
            layout)
    (call-next-method)))

(defun inlinable (node)
  (zerop (e. (e. (e. node |staticScope|) |outNames|) |size|)))

(defun safe-inline-expr (node layout)
  (unless (inlinable node)
    (error "~A cannot be inlined since it defines ~A"
      node
      (e. (e. (e. node |staticScope|) |outNames|) |getKeys|)))
  (inline-expr node layout))

;;; --- checks

(defmethod sequence-expr :around (node layout result)
  (multiple-value-bind (seq layout-after) (call-next-method)
    (when (some (complement #'first) seq)
      (error "Malformed sequence (NIL as variable) from ~S in ~S to ~S:~%~S" node layout result seq))
    (values seq layout-after)))

;;; ---

;; The bizarre use of destructuring-bind is so that (declare (ignore)) works unsurprisingly in the body.
;; XXX write a macro-generating macro for these

(defmacro define-sequence-expr (class (layout-var result-var &rest elements-lambda-list) &body body
    &aux (node-var (gensym)))
  `(defmethod sequence-expr ((,node-var ,class) ,layout-var ,result-var)
    (destructuring-bind (,layout-var ,result-var ,@elements-lambda-list) (list* ,layout-var ,result-var (node-elements ,node-var))
      ,@body)))

(defmacro define-sequence-patt (class (layout-var specimen-var ejector-var &rest elements-lambda-list) &body body
    &aux (node-var (gensym)))
  `(defmethod sequence-patt ((,node-var ,class) ,layout-var ,specimen-var ,ejector-var)
    (destructuring-bind (,layout-var ,specimen-var ,ejector-var ,@elements-lambda-list) (list* ,layout-var ,specimen-var ,ejector-var (node-elements ,node-var))
      ,@body)))

(defmacro define-inline-expr (class (layout-var &rest elements-lambda-list) &body body
    &aux (node-var (gensym)))
  `(defmethod inline-expr ((,node-var ,class) ,layout-var)
    (destructuring-bind (,layout-var ,@elements-lambda-list) (list* ,layout-var (node-elements ,node-var))
      ,@body)))

(defmacro updating-sequence-expr (node layout result &key may-inline
    &aux (cv (gensym)) (lv (gensym)))
  "if MAY-INLINE is true, may replace RESULT with a form to be evaluated exactly once"
  (let ((general-form
         `(multiple-value-bind (,cv ,lv) (sequence-expr ,node ,layout ,result)
            (setf ,layout ,lv)
            ,cv)))
    (if may-inline
      `(if (and ,may-inline (inlinable ,node))
        (progn
          (setf ,result (inline-expr ,node ,layout))
          '())
        ,general-form)
      general-form)))

(defmacro updating-sequence-patt (node layout specimen ejector-binding
    &aux (cv (gensym)) (lv (gensym)))
  `(multiple-value-bind (,cv ,lv) (sequence-patt ,node ,layout ,specimen ,ejector-binding)
    (setf ,layout ,lv)
    ,cv))

(defmacro updating-block-pattern-entry (node layout specimen label exit-operator
    &aux (bindingv (gensym)) (bseqv (gensym)) (nodev (gensym)))
  `(let ((,nodev ,node))
     (multiple-value-bind (,bindingv ,bseqv) (ejector-binding ,label ,exit-operator ,nodev)
       (append ,bseqv (updating-sequence-patt ,nodev ,layout ,specimen ,bindingv)))))

(defun ejector-binding (label exit-operator pattern)
  (if (pattern-reifies-ejector pattern)
    (let ((g (gensym label)))
      (values
        (make-instance 'direct-def-binding :symbol g)
        `((,g (ejector ,label (lambda (v) (,exit-operator v)))))))
    (make-instance 'block-binding :operator exit-operator
                                  :label label)))

;;; --- ---

(defun naive-sequence-to-form (seq value-form)
  (map nil (lambda (x)
             (check-type x (cons symbol (cons t null)))) 
           seq)
  `(let* ,seq
     (declare (ignorable ,@(mapcar #'first seq)))
     ,value-form))
     
(defun sequence-to-form (seq value-form)
  (let ((last (first (last seq))))
    (cond 
      ((and last
            (symbolp value-form)
            (eql value-form (first last)))
        ;; transforms (let (... (a #)) a) into (let (...) #)
        (sequence-to-form (butlast seq) (second last)))
      ((null seq)
        ;; empty sequence, so just use the form
        value-form)
      (t 
        (naive-sequence-to-form seq value-form)))))

(defun leaf-sequence (node layout)
  (inline-expr node layout))

(defun hide-sequence (node layout)
  "for sequences within scope boxes"
  (leaf-sequence node (scope-layout-nest layout)))

(defmacro with-nested-scope-layout ((layout-var) &rest body)
  ;; inhibits assignments escaping as well as adding the box
  `(let ((,layout-var (scope-layout-nest ,layout-var)))
    ,@body))

;;; --- ---

(defun sequence-e-to-cl (expr layout outer-scope-form)
  (let ((rv (gensym "EVAL-RESULT")))
    (sequence-to-form
      (updating-sequence-expr expr layout rv)
      `(values ,rv ,(delta-extract-outer-scope layout expr outer-scope-form)))))

;;; --- Used by generated code ---

(defmacro robust-block ((op-name ejector-label) &body body)
  "Like BLOCK, but tracks dynamic extent so as to better approximate conformance with CLHS 5.2. OP-NAME is bound to a local macro which, if invoked out of scope, will signal E.ELIB:EJECTOR-EXTENT-ERROR with (unevaluated) label EJECTOR-LABEL."
  (let ((block-name (gensym "RB"))
        (valid (gensym "VALID-RB")))
    `(block ,block-name
       (let ((,valid t))
         (unwind-protect
           (macrolet ((,op-name (&optional value)
                        `(if ,',valid
                           (return-from ,',block-name ,value)
                           (error 'ejector-extent-error
                             :ejector-label ',',ejector-label))))
             (locally ,@body))
           (setf ,valid nil))))))

;;; --- Expressions ---

(define-sequence-expr |AssignExpr| (layout result noun-expr value
    &aux (value-var (gensym "ASSIGNEE")))
  (check-type noun-expr |NounExpr|)
  (values
    (let ((noun (first (node-elements noun-expr))))
      (append (updating-sequence-expr value layout value-var)
              `((,result (progn
                           ,(binding-set-code (scope-layout-noun-binding layout noun) value-var)
                           ,value-var)))))
    layout))

(define-sequence-expr |BindingExpr| (layout result noun-expr)
  (check-type noun-expr |NounExpr|)
  (values
    `((,result
       ,(binding-reify-code (scope-layout-noun-binding layout (first (node-elements noun-expr))))))
    layout))

(define-sequence-expr |CallExpr| (layout result recipient verb &rest args
    &aux (rec-var (gensym "REC"))
         (arg-vars (loop for nil in args collect (gensym "ARG"))))
  (values
    (append (updating-sequence-expr recipient layout rec-var :may-inline (every #'inlinable args))
            (loop for (arg . arg-expr-tail) on args
                  for arg-var-cell on arg-vars
                  append (updating-sequence-expr arg layout (first arg-var-cell) :may-inline (every #'inlinable arg-expr-tail) #| XXX O(n^2) |#))
            `((,result (e. ,rec-var ,verb ,@arg-vars))))
    layout))

(define-sequence-expr |CatchExpr| (layout result attempt catch-pattern catch-body
    &aux (pattern-eject-block (gensym "CATCH-PATTERN-"))
         (condition-var (gensym "CONDITION"))
         (catch-outer (gensym "CATCH-OUTER-")))
  (values
    `((,result
       (block ,catch-outer
         (handler-case
            ,(hide-sequence attempt layout)
            (e-catchable-condition (,condition-var)
              (robust-block (,pattern-eject-block "catch-pattern")
                ,(with-nested-scope-layout (layout)
                  (sequence-to-form 
                    (updating-block-pattern-entry catch-pattern layout `(transform-condition-for-e-catch ,condition-var) "catch-pattern" pattern-eject-block)
                    `(return-from ,catch-outer ,(leaf-sequence catch-body layout)))))
              (%catch-expr-resignal ,condition-var))))))
    layout))

(define-sequence-expr |DefineExpr| (layout result pattern opt-ejector value
    &aux (ej (gensym "DEFEJ")) (value-var result))
  (values
    (append ;; if there is no ejector, inlining 
            (if opt-ejector
              (updating-sequence-expr opt-ejector layout ej :may-inline nil)
              '())
            (updating-sequence-expr value layout value-var :may-inline (not opt-ejector))
            (unless (eql value-var result)
              `((,result ,value-var)))
            (updating-sequence-patt pattern layout result
                                    (if opt-ejector
                                      (make-instance 'direct-def-binding :symbol ej)
                                      +throw-binding+)))
    layout))

(defgeneric pattern-has-no-side-effects (node)
  (:documentation "Pattern failure counts as a side effect."))
(defmethod pattern-has-no-side-effects ((node |Pattern|)) nil)
(defmethod pattern-has-no-side-effects ((node |IgnorePattern|))
  (assert (null (node-elements node))) ; IgnorePattern may get a guard in the future; this is so we don't forget to change this to fit
  t)
(defmethod pattern-has-no-side-effects ((node |FinalPattern|))
  (null (ref-shorten (e. node |getOptGuardExpr|))))
(defmethod pattern-has-no-side-effects ((node |VarPattern|))
  (null (ref-shorten (e. node |getOptGuardExpr|))))
(defmethod pattern-has-no-side-effects ((node |BindingPattern|))
  t)

(define-sequence-expr |EscapeExpr| (layout result ejector-patt body opt-catch-pattern opt-catch-body
    &aux (outer-block (gensym "ESCAPE"))
         (ejector-block (gensym "ESCAPE-BODY-"))
         (catch-value-var (gensym "CAUGHT"))
         (inner-result-var (gensym "ESCRESULT"))
         (body-scope (e. body |staticScope|)))
  (when (and (pattern-has-no-side-effects ejector-patt)
             (not (e-is-true (e. body-scope |uses| 
                               (e. (e. ejector-patt |getNoun|) |getName|)))))
    ;; XXX condition assumes that all no-side-effects patterns have a noun like that. should retrieve from static scope instead
    #+(or) 
    (efuncall e.knot:+sys-trace+ (format nil "triggered ejector optimization for ~S in ~S" ejector-patt (scope-layout-fqn-prefix layout)))
    (return-from sequence-expr ;; XXX dependence on existence of block
      (values `((,result ,(hide-sequence body layout))) layout)))
  (values
    (labels ((body-form (ejector-block)
              (sequence-to-form 
                (with-nested-scope-layout (layout)
                  (append (updating-sequence-patt 
                            ejector-patt 
                            layout 
                            `(ejector ',(pattern-opt-noun ejector-patt) 
                                      (lambda (v) (,ejector-block v)))
                            +throw-binding+)
                          (updating-sequence-expr body layout inner-result-var)))
                inner-result-var)))
      `((,result 
         (robust-block (,outer-block ,(pattern-opt-noun ejector-patt))
           ,(if opt-catch-pattern
             `(let ((,catch-value-var 
                     (robust-block (,ejector-block ,(pattern-opt-noun ejector-patt))
                       (,outer-block ,(body-form ejector-block)))))
               (declare (ignorable ,catch-value-var))
               ,(sequence-to-form 
                  (with-nested-scope-layout (layout)
                    (append (updating-block-pattern-entry 
                              opt-catch-pattern 
                              layout
                              catch-value-var
                              (format nil "~A catch block pattern" 
                                          (pattern-opt-noun ejector-patt)) 
                              `(lambda (v)
                                 (declare (ignore v))
                                 (,outer-block ,catch-value-var)))
                            (updating-sequence-expr opt-catch-body layout result)))
                  result))
             (body-form outer-block))))))
    layout))

(define-sequence-expr |FinallyExpr| (layout result attempt-body unwind-body)
  (values
    `((,result
       (unwind-protect
         ,(hide-sequence attempt-body layout)
         ,(hide-sequence unwind-body layout))))
    layout))

;; XXX define inlinably
(define-sequence-expr |HideExpr| (layout result body)
  (values
    (with-nested-scope-layout (layout)
      (updating-sequence-expr body layout result))
    layout))
    
(define-sequence-expr |IfExpr| (layout result test then else
    &aux (block-name (gensym))
         (test-result (gensym)))
  (values
    `((,result 
       (block ,block-name
         ,(with-nested-scope-layout (layout)
            (sequence-to-form 
              (updating-sequence-expr test layout test-result :may-inline t)
              `(when (e-is-true ,test-result)
                 (return-from ,block-name ,(leaf-sequence then layout)))))
         ,(hide-sequence else layout))))
    layout))

(define-inline-expr |LiteralExpr| (layout value)
  (declare (ignore layout))
  `(quote ,value))

(define-sequence-expr |MetaContextExpr| (layout result)
  (values
    ;; XXX revise define-sequence-expr so the source-span can be accessed
    `((,result ',(scope-layout-static-context layout #| :source-span ... |#)))
    layout))

(define-sequence-expr |MetaStateExpr| (layout result)
  (values
    `((,result (e. +the-make-const-map+
                   |fromPairs|
                   (vector ,@(scope-layout-meta-state-bindings layout)))))
    layout))

(define-inline-expr |NounExpr| (layout noun)
  (binding-get-code (scope-layout-noun-binding layout noun)))

(defglobal +seq-object-generators+
  '(:method-body  seq-method-body
    :matcher-body seq-matcher-body))

(defmacro cheap-destructuring-bind ((&rest vars) list &body body)
  "Like DESTRUCTURING-BIND, except that it does not check the length of the input list, and does not support destructuring to anything but a simple list of variables."
  (if vars
    (loop with source = (gensym)
          for (var . more) on vars
          collect `(,var (first ,source)) into let-clauses
          when more
            collect `(,source (rest ,source)) into let-clauses
          finally (return `(let* ((,source ,list)
                                  ,@let-clauses) 
                             ,@body)))
    (let ((body-form `(locally ,@body)))
      (if (symbolp list)
        body-form
        `(progn ,list ,body-form)))))

(defun seq-method-body (layout method args-var)
  (check-type method |EMethod|)
  (destructuring-bind (doc-comment verb patterns opt-result-guard body) (node-elements method)
    (declare (ignore doc-comment verb))
    (let* ((result-var (gensym "RESULT"))
           (guard-var (gensym "RESULT-GUARD"))
           (arg-symbols (loop for i below (length patterns)
                              collect (make-symbol (format nil "ARG~A" i)))))
      `(cheap-destructuring-bind (,@arg-symbols) ,args-var
        (declare (ignorable ,@arg-symbols))
        ,(sequence-to-form
          (append (loop for arg-patt across patterns
                        for arg-var in arg-symbols
                        append (updating-sequence-patt arg-patt
                                                       layout
                                                       arg-var
                                                       +throw-binding+))
                  (when opt-result-guard
                    (updating-sequence-expr opt-result-guard layout guard-var :may-inline (inlinable body)))
                  (updating-sequence-expr body layout result-var :may-inline t))
          (if opt-result-guard
            `(e. ,guard-var |coerce| ,result-var +the-thrower+)
            result-var))))))

(defun seq-matcher-body (layout matcher mverb-var args-var remaining-code)
  (check-type matcher |EMatcher|)
  (destructuring-bind (pattern body) (node-elements matcher)
    (let* ((pattern-eject-block (gensym "MATCHER-SKIP"))
           (matcher-block (gensym "MATCHERS"))
           (result-var (gensym "RESULT"))
           (pair-var (gensym "MATCH-ARG"))
           (ej-name (format nil "~A matcher" (scope-layout-fqn-prefix layout))))
      `(block ,matcher-block
        (robust-block (,pattern-eject-block ,ej-name)
          ,(sequence-to-form
             (append
               `((,pair-var (vector (unmangle-verb ,mverb-var) (coerce ,args-var 'vector))))
               (updating-block-pattern-entry pattern layout pair-var ej-name pattern-eject-block)
               (updating-sequence-expr body layout result-var :may-inline t))
             `(return-from ,matcher-block ,result-var)))
        ;; if we reach here, the ejector was used
        ,remaining-code))))

(defgeneric object-binding (pattern layout object-var opt-as-auditor-var))

(defmethod object-binding ((pattern |IgnorePattern|) layout object-var opt-as-auditor-var)
  (values '() layout))

(defmethod object-binding ((pattern |FinalPattern|) layout object-var opt-as-auditor-var)
  (destructuring-bind (noun-expr opt-guard) (node-visitor-arguments pattern)
    (let ((noun (e. noun-expr |getName|)))
      (assert (null opt-guard))
      (values
        '()
        (scope-layout-bind layout noun
          (if opt-as-auditor-var
            (make-instance 'direct-def-binding :symbol object-var
                                               :noun noun
                                               :guard-var opt-as-auditor-var)
            (make-instance 'direct-def-binding :symbol object-var
                                               :noun noun)))))))

(define-sequence-expr |ObjectExpr| (layout result doc-comment pattern auditors script
    &aux (object-var (gensym (or (pattern-opt-noun pattern) "G")))
         (needs-self-reference (or (e.elang::usesp pattern script))))
  (multiple-value-bind (auditor-exprs as-p) (unpack-auditors auditors)
    (let ((auditor-vars (loop for nil across auditor-exprs collect (gensym "AUDITOR"))))
      (assert (not (e.elang::usesp pattern auditor-exprs)))
      (let* ((pattern-seq 
               (multiple-value-bind (c l) 
                   (object-binding pattern layout object-var 
                                   (when as-p (elt auditor-vars 0)))
                 (setf layout l)
                 c))
             (auditors-seq 
              (let ((layout layout))
                ;; auditor exprs are disallowed from mentioning the object.
                ;; if first expr is an 'as' then it must not be inlined since
                ;; its var is the binding's guard var.
                (loop for (auditor-expr . auditor-expr-tail) on (coerce auditor-exprs 'list)
                      for auditor-var-cell on auditor-vars
                      for inline-ok = (not as-p) then t
                      append (updating-sequence-expr auditor-expr layout (car auditor-var-cell) :may-inline (and inline-ok (every #'inlinable auditor-expr-tail))))))
             (object-form (object-form +seq-object-generators+
                                       layout 
                                       (make-instance '|ObjectExpr| :elements (list doc-comment pattern auditors script))
                                       auditor-vars)))
        (values
          ;; ASSUMPTION: the auditors cannot reference the pattern (checked by assert above (and Kernel-E rules?)). This allows us to put the pattern after the auditors, which is necessary for the 'as' guard reference and for the non-self-referencing optimized case
          (if needs-self-reference
            ;; if the object refers to itself, we need to establish its binding before evaluating it
            (append
              `((,object-var 'the-compiler-is-broken-if-you-have-this))
              auditors-seq
              pattern-seq
              `((,result
                  (setf ,object-var
                        ,object-form))))
            ;; if the object doesn't refer to itself, we don't need any assignment gimmicks
            ;; ASSUMPTION: reordering is OK, because side effects are only within one sequence (the object-form's execution of auditors)
            (append auditors-seq
                    `((,object-var ,object-form))
                    pattern-seq
                    `((,result ,object-var))))
          layout))))
  )

(define-sequence-expr |SeqExpr| (layout result &rest subs)
  (values
    (loop for (sub . next) on subs append
      ;; this would be inlinable if we evaluated the SEQ variables -- XXX figure out if this could produce better code
      (updating-sequence-expr sub layout (if next 
                                           (gensym "SEQ")
                                           result)))
    layout))

;;; --- Patterns ---

(defgeneric pattern-reifies-ejector (pattern)
  (:documentation "Whether the given pattern needs a reified ejector; that is, whether it invokes BINDING-GET-CODE on its EJECTOR-BINDING.")
  (:method ((p |IgnorePattern|))  nil)
  (:method ((p |BindingPattern|)) nil)
  (:method ((p |ViaPattern|))     t)
  (:method ((p |ListPattern|))
    ;; XXX look into making the coerce-to-list such that it doesn't use a reified ejector
    #+(or) (some #'pattern-reifies-ejector (e. p |getSubs|))
    t)
  (:method ((p |FinalPattern|)) (not (null (e. p |getOptGuardExpr|))))
  (:method ((p |VarPattern|))   (not (null (e. p |getOptGuardExpr|)))))

(define-sequence-patt |IgnorePattern| (layout specimen ejector-binding)
  (declare (ignore specimen ejector-binding))
  (values '() layout))

(defun %make-list-pattern-arity-error (has wants)
  (make-condition 'simple-error
    :format-control "a ~A size list doesn't match a ~A size list pattern"
    :format-arguments (list has wants)))

(define-sequence-patt |ListPattern| (layout specimen ejector-binding &rest patterns
    &aux (coerced (gensym "ELIST"))
         (pattern-arity (length patterns)))
  (values
    (append `((,coerced 
               (let ((,coerced (e-coerce-native ,specimen 'vector ,(binding-get-code ejector-binding))))
                 (if (eql (length ,coerced) ,pattern-arity)
                   ,coerced
                   ,(eject-via-binding-code ejector-binding `(%make-list-pattern-arity-error (length ,coerced) ',pattern-arity))))))
            (loop for patt in patterns
                  for i from 0
                  append (updating-sequence-patt patt
                                                 layout
                                                 `(aref ,coerced ',i)
                                                 ejector-binding)))
    layout))

(define-sequence-patt |ViaPattern| (layout specimen ejector-binding function pattern
    &aux (function-var (gensym "VFN"))
         (post-specimen-var (gensym "VSP")))
  (values
    (append (updating-sequence-expr function layout function-var)
            `((,post-specimen-var 
               (efuncall ,function-var ,specimen
                                       ,(binding-get-code ejector-binding))))
            (updating-sequence-patt pattern layout post-specimen-var ejector-binding))
    layout))

;;; --- Binding patterns ---

(defun final-sequence-binding (noun layout specimen-var ejector-binding guard-var
    &aux (coerced-var (make-symbol noun)) #| must be deterministic |#)
  (if guard-var
    (values
      `((,coerced-var (e. ,guard-var |coerce| ,specimen-var ,(binding-get-code ejector-binding))))
      (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol coerced-var :guard-var guard-var :noun noun)))
    (if (symbolp specimen-var) ;; XXX really means no-side-effects-p
      (values
        '()
        (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol specimen-var :noun noun)))
      (values
        `((,coerced-var ,specimen-var))
        (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol coerced-var :noun noun))))))

(defun var-sequence-binding (noun layout specimen-var ejector-binding guard-var
    &aux (value-var (gensym (concatenate 'string "var " noun)))
         (broken-var (gensym "BROKEN")))
  (values
    (append `((,broken-var nil))
            (if guard-var
              `((,value-var (e. ,guard-var |coerce| ,specimen-var ,(binding-get-code ejector-binding))))
              `((,value-var ,specimen-var))))
    (scope-layout-bind layout noun (make-instance 'direct-var-binding :value-symbol value-var :guard-symbol guard-var :broken-symbol broken-var :noun noun))))

(defun binding-sequence-binding (noun layout specimen-var ejector-binding syntactic-guard-var
    &aux (binding-var (gensym (concatenate 'string "&&" noun)))
         (slot-var (gensym (concatenate 'string "&" noun)))
         (guard-var (gensym (concatenate 'string "&" noun "-guard"))))
  (declare (ignore ejector-binding))
  (assert (null syntactic-guard-var))
  (values
    ; XXX this code feels like it should be merged with the compiled-file wrapper
    `((,binding-var (e-coerce ,specimen-var 'coerced-slot))
      (,slot-var (eelt ,binding-var))
      (,guard-var (e. ,binding-var |getGuard|)))
    (scope-layout-bind layout noun
      (make-instance 'lexical-slot-binding :symbol slot-var
                                           :guard-var guard-var))))

(defun sequence-binding-pattern (fn specimen ejector-binding layout noun-expr &optional opt-guard-expr
    &aux (guardv (gensym "FINAL-GUARD")))
  (check-type noun-expr |NounExpr|)
  (let ((noun (first (node-elements noun-expr))))
    (values
      (append (if opt-guard-expr
                (updating-sequence-expr opt-guard-expr layout guardv :may-inline nil)
                `())
              (multiple-value-bind (seq layout2) (funcall fn noun layout specimen ejector-binding (if opt-guard-expr guardv))
                (setf layout layout2)
                seq))
      layout)))

(define-sequence-patt |FinalPattern| (layout specimen ejector-binding &rest noun-details)
  (apply #'sequence-binding-pattern #'final-sequence-binding specimen ejector-binding layout noun-details))

(define-sequence-patt |BindingPattern| (layout specimen ejector-binding &rest noun-details)
  (apply #'sequence-binding-pattern #'binding-sequence-binding specimen ejector-binding layout noun-details))

(define-sequence-patt |VarPattern| (layout specimen ejector-binding &rest noun-details)
  (apply #'sequence-binding-pattern #'var-sequence-binding specimen ejector-binding layout noun-details))
