; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang.compiler.seq)

(defgeneric sequence-expr (node layout result)
  (:documentation "Compile an E expression into a series of LET* binding clauses, and return the clauses and the updated scope layout. The symbol in RESULT will be bound to the return value of the node."))

(defgeneric sequence-patt (node layout specimen ejector-spec)
  (:documentation "Compile an E pattern into a series of LET* binding clauses, and return the clauses and the updated scope layout."))

;; The bizarre use of destructuring-bind is so that (declare (ignore)) works unsurprisingly in the body.

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

(defmacro updating-sequence-expr (node layout result
    &aux (cv (gensym)) (lv (gensym)))
  `(multiple-value-bind (,cv ,lv) (sequence-expr ,node ,layout ,result)
    (setf ,layout ,lv)
    ,cv))

(defmacro updating-sequence-patt (node layout specimen ejector-spec
    &aux (cv (gensym)) (lv (gensym)))
  `(multiple-value-bind (,cv ,lv) (sequence-patt ,node ,layout ,specimen ,ejector-spec)
    (setf ,layout ,lv)
    ,cv))

;;; --- sequence variable tags ---

;; XXX the sv-preserve/sv-temporary system is Not The Right Thing. instead, our let*-clause language should be extended to have 'end of uses of variable' declaration, which permits but does not require processors to take it out of scope.

(defgeneric sv-preserve (tag)
  (:documentation "If false, this variable need not be kept in scope. Used by match-bind."))

(defmethod sv-preserve ((tag (eql 'nil)))
  t)
(defmethod sv-preserve ((tag (eql 'sv-temporary)))
  nil)

;;; --- ---

(defun sequence-to-form (seq value-form)
  `(let* ,(mapcar (lambda (x) (subseq x 0 2)) seq)
     (declare (ignorable ,@(mapcar #'first seq)))
     ,value-form))

(defun leaf-sequence (node layout
    &aux (result (gensym "R")))
  (sequence-to-form (sequence-expr node layout result) result))

(defun hide-sequence (node layout)
  "for sequences within scope boxes"
  (leaf-sequence node (scope-layout-nest layout)))

(defmacro with-nested-scope-layout ((layout-var) &rest body)
  ;; inhibits assignments escaping as well as adding the box
  `(let ((,layout-var (scope-layout-nest ,layout-var)))
    ,@body))

;;; --- ---

(defun sequence-e-to-cl (expr outer-scope)
  (let ((layout (outer-scope-to-layout outer-scope))
        (rv (gensym "EVAL-RESULT")))
    (sequence-to-form
      (updating-sequence-expr expr layout rv)
      `(values ,rv ,(delta-extract-outer-scope layout expr outer-scope)))))

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

(define-sequence-expr |CallExpr| (layout result recipient verb &rest args
    &aux (rec-var (gensym "REC"))
         (arg-vars (loop for arg in args collect (gensym "ARG"))))
  (values
    (append (updating-sequence-expr recipient layout rec-var)
            (loop for arg in args
                  for arg-var in arg-vars
                  append (updating-sequence-expr arg layout arg-var))
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
              (block ,pattern-eject-block
                ,(with-nested-scope-layout (layout)
                  (sequence-to-form 
                    (updating-sequence-patt catch-pattern layout `(transform-condition-for-e-catch ,condition-var) `(eject-function "catch-pattern" (lambda (v) (return-from ,pattern-eject-block v))))
                    `(return-from ,catch-outer ,(leaf-sequence catch-body layout)))))
              (%catch-expr-resignal ,condition-var))))))
    layout))

(define-sequence-expr |DefineExpr| (layout result pattern value opt-ejector
    &aux (ej (gensym "DEFEJ")))
  (values
    (append (updating-sequence-expr value layout result)
            (if opt-ejector
              (updating-sequence-expr opt-ejector layout ej)
              `((,ej nil sv-temporary)))
            (updating-sequence-patt pattern layout result `(ejector ,ej)))
    layout))

(define-sequence-expr |EscapeExpr| (layout result ejector-patt body opt-catch-pattern opt-catch-body
    &aux (outer-block (gensym "ESCAPE"))
         (ejector-block (gensym "ESCAPE-BODY-"))
         (ejector-var (gensym "ESCAPE-EJ-"))
         (catch-value-var (gensym "CAUGHT"))
         (inner-result-var (gensym "ESCRESULT")))
  (values
    (labels ((body-form (ejector-block outer-block)
              (sequence-to-form 
                (with-nested-scope-layout (layout)
                  (append `((,ejector-var (ejector ',(pattern-opt-noun ejector-patt) (lambda (v) (return-from ,ejector-block v)))))
                          (updating-sequence-patt ejector-patt layout ejector-var nil)
                          (updating-sequence-expr body layout inner-result-var)))
                `(return-from ,outer-block ,inner-result-var))))
      `((,result 
         (block ,outer-block
           ,(if opt-catch-pattern
             `(let ((,catch-value-var 
                     (block ,ejector-block
                       ,(body-form ejector-block outer-block))))
               (declare (ignorable ,catch-value-var))
               ,(sequence-to-form 
                  (with-nested-scope-layout (layout)
                    (append (updating-sequence-patt opt-catch-pattern layout catch-value-var nil)
                            (updating-sequence-expr opt-catch-body layout result)))
                  result))
             (body-form outer-block outer-block))))))
    layout))

(define-sequence-expr |FinallyExpr| (layout result attempt-body unwind-body)
  (values
    `((,result
       (unwind-protect
         ,(hide-sequence attempt-body layout)
         ,(hide-sequence unwind-body layout))))
    layout))

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
              (updating-sequence-expr test layout test-result)
              `(when (e-is-true ,test-result)
                 (return-from ,block-name ,(leaf-sequence then layout)))))
         ,(hide-sequence else layout))))
    layout))

(define-sequence-expr |LiteralExpr| (layout result value)
  (values
    `((,result (quote ,value)))
    layout))

;; Um--I blame the ugliness of the implementation on the ugliness of
;; the definition.
;; XXX it would be better to use multiple values instead of an explicit
;; list - we can do this if we change the sequence format to allow
;; multiple-value binding
(define-sequence-expr |MatchBindExpr| (layout result specimen pattern
    &aux (list-block (gensym "MB-PATTERN-LIST"))
         (exit-block (gensym "MB-PATTERN-EXIT"))
         (specimen-var (gensym "SPECIMEN"))
         (values-var (gensym "MB-VALUES")))
  (values
    (let* ((specimen-seq (updating-sequence-expr specimen layout specimen-var))
           (pattern-seq (updating-sequence-patt 
                          pattern layout specimen-var 
                          `(eject-function 
                             "match-bind" 
                             (lambda (v) 
                               (return-from ,exit-block v)))))
           (pattern-vars (mapcar #'first
                                 (remove-if-not #'sv-preserve
                                                pattern-seq
                                                :key #'third))))
      (append specimen-seq
              ;; squeeze all the vars from the pattern into a list
              `((,values-var
                 (block ,list-block 
                   ;; constructing the *failure* value - will never
                   ;; reach here on success
                   (cons +e-false+ 
                         (make-list ,(length pattern-seq) :initial-element 
                           (make-unconnected-ref 
                             (block ,exit-block
                               ;; this is the normal case - on success,
                               ;; the return-from jumps out of the
                               ;; failure list construction
                               ,(sequence-to-form pattern-seq
                                  `(return-from ,list-block
                                     (list +e-true+ 
                                           ,@pattern-vars))))))))
                 sv-temporary))
              ;; destructure the list back to the expected vars
              (loop for var in (cons result pattern-vars)
                    append `((,var (first ,values-var))
                             (,values-var (rest ,values-var) sv-temporary)))))
    layout))

(define-sequence-expr |MetaContextExpr| (layout result)
  (values
    (let ((prefix (scope-layout-fqn-prefix layout)))
      `((,result 
         ',(e-lambda "org.erights.e.elang.scope.StaticContext" ()
              (:|__printOn| (tw)
                (e-coercef tw +the-text-writer-guard+)
                (e. tw |print| "<static context>"))
              (:|getFQNPrefix| ()
                prefix)))))
    layout))

(define-sequence-expr |MetaStateExpr| (layout result)
  (values
    `((,result (e. ',+the-make-const-map+ |fromPairs|
                 (vector ,@(scope-layout-meta-state-bindings layout)))))
    layout))

(define-sequence-expr |NounExpr| (layout result noun)
  (values
    `((,result ,(binding-get-code (scope-layout-noun-binding layout noun))))
    layout))

(defglobal +seq-object-generators+
  '(:method-body  seq-method-body
    :matcher-body seq-matcher-body))

(defun seq-method-body (layout method args-var)
  (check-type method |EMethod|)
  (destructuring-bind (doc-comment verb patterns opt-result-guard body) (node-elements method)
    (declare (ignore doc-comment verb))
    (let* ((result-var (gensym "RESULT"))
           (guard-var (gensym "RESULT-GUARD"))
           (arg-symbols (loop for i below (length patterns)
                              collect (make-symbol (format nil "ARG~A" i)))))
      `(destructuring-bind (,@arg-symbols) ,args-var
        (declare (ignorable ,@arg-symbols))
        ,(sequence-to-form
          (append (loop for arg-patt across patterns
                        for arg-var in arg-symbols
                        append (updating-sequence-patt arg-patt
                                                       layout
                                                       arg-var
                                                       nil))
                  (when opt-result-guard
                    (updating-sequence-expr opt-result-guard layout guard-var))
                  (updating-sequence-expr body layout result-var))
          (if opt-result-guard
            `(e. ,guard-var |coerce| ,result-var nil)
            result-var))))))

(defun seq-matcher-body (layout matcher mverb-var args-var remaining-code)
  (check-type matcher |EMatcher|)
  (destructuring-bind (pattern body) (node-elements matcher)
    (setf layout (scope-layout-nest layout))
    (let* ((pattern-eject-block (gensym "MATCHER-SKIP"))
           (matcher-block (gensym "MATCHERS"))
           (result-var (gensym "RESULT"))
           (pair-var (gensym "MATCH-ARG")))
      `(block ,matcher-block
        (block ,pattern-eject-block
          ,(sequence-to-form
             (append
               `((,pair-var (vector (unmangle-verb ,mverb-var) (coerce ,args-var 'vector)) sv-temporary))
               (updating-sequence-patt pattern layout pair-var `(eject-function ,(format nil "~A matcher" (scope-layout-fqn-prefix layout)) (lambda (v) (return-from ,pattern-eject-block v))))
               (updating-sequence-expr body layout result-var))
             `(return-from ,matcher-block ,result-var)))
        ;; if we reach here, the ejector was used
        ,remaining-code))))

(define-sequence-expr |ObjectExpr| (layout result doc-comment qualified-name auditor-exprs script
    &aux (auditor-vars (loop for nil across auditor-exprs collect (gensym "AUDITOR")))
         (fqn (updating-fully-qualify-name layout qualified-name)))
  (values
    (append
      (loop for auditor-expr across auditor-exprs
            for auditor-var in auditor-vars
            append (updating-sequence-expr auditor-expr layout auditor-var))
      `((,result
         ,(object-form +seq-object-generators+
                       layout 
                       (make-instance '|ObjectExpr| :elements (list doc-comment qualified-name auditor-exprs script))
                       doc-comment
                       auditor-vars
                       script
                       fqn))))
    layout))

(define-sequence-expr |SeqExpr| (layout result &rest subs)
  (values
    (loop for (sub . next) on subs append
      (updating-sequence-expr sub layout (if next 
                                           (gensym "SEQ")
                                           result)))
    layout))

(define-sequence-expr |SlotExpr| (layout result noun-expr)
  (check-type noun-expr |NounExpr|)
  (values
    `((,result
       ,(binding-get-slot-code (scope-layout-noun-binding layout (first (node-elements noun-expr))))))
    layout))

;;; --- Patterns ---

(define-sequence-patt |CdrPattern| (layout specimen ejector-spec list-patt rest-patt
    &aux (coerced (gensym "CDRLIST"))
         (head-var (gensym "CDRHEAD"))
         (rest-var (gensym "CDRREST"))
         (min-arity (length (node-elements list-patt))))
  (values
    (append `((,coerced 
               (let ((,coerced (e-coerce-native ,specimen 'vector ,(opt-ejector-make-code ejector-spec))))
                 (if (>= (length ,coerced) ,min-arity)
                   ,coerced
                   ,(eject-code ejector-spec `(%make-cdr-pattern-arity-error (length ,coerced) ',min-arity))))
               sv-temporary))
            `((,head-var (subseq ,coerced 0 ',min-arity)))
            (updating-sequence-patt list-patt layout head-var ejector-spec)
            `((,rest-var (subseq ,coerced ',min-arity)))
            (updating-sequence-patt rest-patt layout rest-var ejector-spec))
    layout))

(define-sequence-patt |IgnorePattern| (layout specimen ejector-spec)
  (declare (ignore specimen ejector-spec))
  (values '() layout))

(defun %make-list-pattern-arity-error (has wants)
  (make-condition 'simple-error
    :format-control "a ~A size list doesn't match a ~A size list pattern"
    :format-arguments (list has wants)))

(defun %make-cdr-pattern-arity-error (has wants)
  (make-condition 'simple-error
    :format-control "a ~A size list doesn't match a >= ~A size list pattern"
    :format-arguments (list has wants)))

(defun %make-such-that-error ()
  ;; XXX improvement: we could print the test expression and the specimen in this message
  (make-condition 'simple-error :format-control "such-that expression was false"))

(define-sequence-patt |ListPattern| (layout specimen ejector-spec &rest patterns
    &aux (coerced (gensym "ELIST"))
         (pattern-arity (length patterns)))
  (values
    (append `((,coerced 
               (let ((,coerced (e-coerce-native ,specimen 'vector ,(opt-ejector-make-code ejector-spec))))
                 (if (eql (length ,coerced) ,pattern-arity)
                   ,coerced
                   ,(eject-code ejector-spec `(%make-list-pattern-arity-error (length ,coerced) ',pattern-arity))))
               sv-temporary))
            (loop for patt in patterns
                  for i from 0
                  for sub-specimen-var = (gensym (format nil "ELIST~A-" i))
                  append `((,sub-specimen-var (aref ,coerced ',i)))
                  append (updating-sequence-patt patt
                                                 layout
                                                 sub-specimen-var
                                                 ejector-spec)))
    layout))

(define-sequence-patt |SuchThatPattern| (layout specimen ejector-spec pattern test
    &aux (test-result (gensym "TEST")))
  (values
    (append (updating-sequence-patt pattern layout specimen ejector-spec)
            (updating-sequence-expr test layout test-result)
            `((,(gensym "JUNK")
               (unless (e-is-true ,test-result)
                 ,(eject-code ejector-spec `(%make-such-that-error)))
               sv-temporary)))
    layout))

;;; --- Binding patterns ---

(defun final-sequence-binding (noun layout specimen-var ejector-spec guard-var
    &aux (coerced-var (gensym (concatenate 'string "var " noun))))
  (if guard-var
    (values
      `((,coerced-var (e. ,guard-var |coerce| ,specimen-var ,(opt-ejector-make-code ejector-spec))))
      (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol coerced-var)))
    (values
      '()
      (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol specimen-var)))))

(defun var-sequence-binding (noun layout specimen-var ejector-spec guard-var
    &aux (slot-var (gensym (concatenate 'string "var " noun))))
  (declare (ignore ejector-spec))
  (values
    ;; XXX look into not always reifying var slots
    ;; XXX this will not use the ejector on coercion failure - find out whether this is a bug
    (if guard-var
      `((,slot-var (make-instance 'e-guarded-slot :value ,specimen-var :guard ,guard-var)))
      `((,slot-var (make-instance 'e-var-slot :value ,specimen-var))))
    (scope-layout-bind layout noun slot-var)))

(defun slot-sequence-binding (noun layout specimen-var ejector-spec guard-var
    &aux (slot-var (gensym (concatenate 'string "&" noun))))
  (if guard-var
    (values
      `((,slot-var (e. ,guard-var |coerce| ,specimen-var ,(opt-ejector-make-code ejector-spec))))
      (scope-layout-bind layout noun slot-var))
    (values
      '()
      (scope-layout-bind layout noun specimen-var))))

(defun sequence-binding-pattern (fn specimen ejector-spec layout noun-expr opt-guard-expr
    &aux (guardv (gensym "FINAL-GUARD")))
  (check-type noun-expr |NounExpr|)
  (let ((noun (first (node-elements noun-expr))))
    (values
      (append (if opt-guard-expr
                (updating-sequence-expr opt-guard-expr layout guardv)
                `())
              (multiple-value-bind (seq layout2) (funcall fn noun layout specimen ejector-spec (if opt-guard-expr guardv))
                (setf layout layout2)
                seq))
      layout)))

(define-sequence-patt |FinalPattern| (layout specimen ejector-spec &rest noun-details)
  (apply #'sequence-binding-pattern #'final-sequence-binding specimen ejector-spec layout noun-details))

(define-sequence-patt |SlotPattern| (layout specimen ejector-spec &rest noun-details)
  (apply #'sequence-binding-pattern #'slot-sequence-binding specimen ejector-spec layout noun-details))

(define-sequence-patt |VarPattern| (layout specimen ejector-spec &rest noun-details)
  (apply #'sequence-binding-pattern #'var-sequence-binding specimen ejector-spec layout noun-details))
