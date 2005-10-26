; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang.compiler.seq)

(defgeneric sequence-expr (node layout result)
  (:documentation "Compile an E expression into a series of LET* binding clauses, and return the clauses and the updated scope layout. The symbol in RESULT will be bound to the return value of the node."))

(defgeneric sequence-patt (node layout specimen ejector-spec)
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
  (= (e. (e. (e. node |staticScope|) |outNames|) |size|) 0))

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

(defmacro updating-sequence-patt (node layout specimen ejector-spec
    &aux (cv (gensym)) (lv (gensym)))
  `(multiple-value-bind (,cv ,lv) (sequence-patt ,node ,layout ,specimen ,ejector-spec)
    (setf ,layout ,lv)
    ,cv))

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
              (block ,pattern-eject-block
                ,(with-nested-scope-layout (layout)
                  (sequence-to-form 
                    (updating-sequence-patt catch-pattern layout `(transform-condition-for-e-catch ,condition-var) `(eject-function "catch-pattern" (lambda (v) (return-from ,pattern-eject-block v))))
                    `(return-from ,catch-outer ,(leaf-sequence catch-body layout)))))
              (%catch-expr-resignal ,condition-var))))))
    layout))

(define-sequence-expr |DefineExpr| (layout result pattern value opt-ejector
    &aux (ej (gensym "DEFEJ")) (value-var result))
  (values
    (append ;; if there is an ejector, inlining 
            (updating-sequence-expr value layout value-var :may-inline (not opt-ejector))
            (if opt-ejector
              (updating-sequence-expr opt-ejector layout ej :may-inline nil)
              '())
            (unless (eql value-var result)
              `((,result ,value-var)))
            (updating-sequence-patt pattern layout result (when opt-ejector `(ejector ,ej))))
    layout))

(define-sequence-expr |EscapeExpr| (layout result ejector-patt body opt-catch-pattern opt-catch-body
    &aux (outer-block (gensym "ESCAPE"))
         (ejector-block (gensym "ESCAPE-BODY-"))
         (catch-value-var (gensym "CAUGHT"))
         (inner-result-var (gensym "ESCRESULT")))
  (values
    (labels ((body-form (ejector-block)
              (sequence-to-form 
                (with-nested-scope-layout (layout)
                  (append (updating-sequence-patt 
                            ejector-patt 
                            layout 
                            `(ejector ',(pattern-opt-noun ejector-patt) (lambda (v) (return-from ,ejector-block v)))
                            nil)
                          (updating-sequence-expr body layout inner-result-var)))
                inner-result-var)))
      `((,result 
         (block ,outer-block
           ,(if opt-catch-pattern
             `(let ((,catch-value-var 
                     (block ,ejector-block
                       (return-from ,outer-block ,(body-form ejector-block)))))
               (declare (ignorable ,catch-value-var))
               ,(sequence-to-form 
                  (with-nested-scope-layout (layout)
                    (append (updating-sequence-patt opt-catch-pattern layout catch-value-var nil)
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

;; Um--I blame the ugliness of the implementation on the ugliness of
;; the definition.
;; XXX it would be better to use multiple values instead of an explicit
;; list - we can do this if we change the sequence format to allow
;; multiple-value binding
(define-sequence-expr |MatchBindExpr| (layout result specimen pattern
    &aux (list-block (gensym "MB-PATTERN-LIST"))
         (exit-block (gensym "MB-PATTERN-EXIT"))
         (specimen-var (gensym "SPECIMEN"))
         (values-var (gensym "MB-VALUES"))
         (problem-var (gensym "MB-PROBLEM")))
  (values
    (let* ((specimen-seq (updating-sequence-expr specimen layout specimen-var))
           (layout-before-pattern layout)
           (pattern-seq (updating-sequence-patt 
                          pattern layout specimen-var 
                          `(eject-function 
                             "match-bind" 
                             (lambda (v) 
                               (return-from ,exit-block v)))))
           (pattern-info
             (loop for (noun . binding) in 
                     (scope-layout-bindings-before
                       layout
                       layout-before-pattern)
                   for ei = (binding-exit-info binding problem-var)
                   do (when (some (complement #'first) ei)
                        (error "Malformed exit info (NIL as variable) ~
                                from ~S => ~S: ~S" 
                               noun binding ei))
                   append ei)))
      (append specimen-seq
              ;; squeeze all the vars from the pattern into a list
              `((,values-var
                 (block ,list-block 
                   ;; constructing the *failure* value - will never
                   ;; reach here on success
                   (cons +e-false+ 
                         (let ((,problem-var
                             (make-unconnected-ref 
                               (block ,exit-block
                                 ;; this is the normal case - on success,
                                 ;; the return-from jumps out of the
                                 ;; failure list construction
                                 ,(sequence-to-form pattern-seq
                                    `(return-from ,list-block
                                       (list +e-true+ 
                                             ,@(mapcar #'first pattern-info))))))))
                           (declare (ignorable ,problem-var))
                           (list ,@(mapcar #'second pattern-info)))))))
              ;; destructure the list back to the expected vars
              (loop for (var) in (cons (list result) pattern-info)
                    append `((,var (first ,values-var))
                             (,values-var (rest ,values-var))))))
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
                                                       nil))
                  (when opt-result-guard
                    (updating-sequence-expr opt-result-guard layout guard-var :may-inline (inlinable body)))
                  (updating-sequence-expr body layout result-var :may-inline t))
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
               `((,pair-var (vector (unmangle-verb ,mverb-var) (coerce ,args-var 'vector))))
               (updating-sequence-patt pattern layout pair-var `(eject-function ,(format nil "~A matcher" (scope-layout-fqn-prefix layout)) (lambda (v) (return-from ,pattern-eject-block v))))
               (updating-sequence-expr body layout result-var :may-inline t))
             `(return-from ,matcher-block ,result-var)))
        ;; if we reach here, the ejector was used
        ,remaining-code))))

(define-sequence-expr |ObjectExpr| (layout result doc-comment qualified-name auditor-exprs script
    &aux (auditor-vars (loop for nil across auditor-exprs collect (gensym "AUDITOR")))
         (fqn (updating-fully-qualify-name layout qualified-name)))
  (values
    (append
      (loop for (auditor-expr . auditor-expr-tail) on (coerce auditor-exprs 'list)
            for auditor-var-cell on auditor-vars
            append (updating-sequence-expr auditor-expr layout (car auditor-var-cell) :may-inline (every #'inlinable auditor-expr-tail)))
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
      ;; this would be inlinable if we evaluated the SEQ variables -- XXX figure out if this could produce better code
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
                   ,(eject-code ejector-spec `(%make-cdr-pattern-arity-error (length ,coerced) ',min-arity))))))
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

(define-sequence-patt |ListPattern| (layout specimen ejector-spec &rest patterns
    &aux (coerced (gensym "ELIST"))
         (pattern-arity (length patterns)))
  (values
    (append `((,coerced 
               (let ((,coerced (e-coerce-native ,specimen 'vector ,(opt-ejector-make-code ejector-spec))))
                 (if (eql (length ,coerced) ,pattern-arity)
                   ,coerced
                   ,(eject-code ejector-spec `(%make-list-pattern-arity-error (length ,coerced) ',pattern-arity))))))
            (loop for patt in patterns
                  for i from 0
                  append (updating-sequence-patt patt
                                                 layout
                                                 `(aref ,coerced ',i)
                                                 ejector-spec)))
    layout))

(define-sequence-patt |SuchThatPattern| (layout specimen ejector-spec pattern test
    &aux (test-result (gensym "TEST"))
         (specimen-var (gensym "STSP")))
  (values
    (append `((,specimen-var ,specimen))
            (updating-sequence-patt pattern layout specimen-var ejector-spec)
            (updating-sequence-expr test layout test-result :may-inline t)
            `((,(gensym "JUNK")
               (unless (e-is-true ,test-result)
                 ,(eject-code ejector-spec `(%make-such-that-error ,specimen-var ',test))))))
    layout))

;;; --- Binding patterns ---

(defun final-sequence-binding (noun layout specimen-var ejector-spec guard-var
    &aux (coerced-var (make-symbol noun)) #| must be deterministic |#)
  (if guard-var
    (values
      `((,coerced-var (e. ,guard-var |coerce| ,specimen-var ,(opt-ejector-make-code ejector-spec))))
      (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol coerced-var :noun noun)))
    (if (symbolp specimen-var) ;; XXX really means no-side-effects-p
      (values
        '()
        (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol specimen-var :noun noun)))
      (values
        `((,coerced-var ,specimen-var))
        (scope-layout-bind layout noun (make-instance 'direct-def-binding :symbol coerced-var :noun noun))))))

(defun var-sequence-binding (noun layout specimen-var ejector-spec guard-var
    &aux (value-var (gensym (concatenate 'string "var " noun)))
         (broken-var (gensym "BROKEN")))
  (values
    (append `((,broken-var nil))
            (if guard-var
              `((,value-var (e. ,guard-var |coerce| ,specimen-var ,(opt-ejector-make-code ejector-spec))))
              `((,value-var ,specimen-var))))
    (scope-layout-bind layout noun (make-instance 'direct-var-binding :value-symbol value-var :guard-symbol guard-var :broken-symbol broken-var :noun noun))))

(defun slot-sequence-binding (noun layout specimen-var ejector-spec guard-var
    &aux (slot-var (gensym (concatenate 'string "&" noun))))
  (if guard-var
    (values
      `((,slot-var (e. ,guard-var |coerce| ,specimen-var ,(opt-ejector-make-code ejector-spec))))
      (scope-layout-bind layout noun slot-var))
    (if (symbolp specimen-var)
      (values
        '()
        (scope-layout-bind layout noun specimen-var))
      (values
        `((,slot-var ,specimen-var))
        (scope-layout-bind layout noun slot-var)))))

(defun sequence-binding-pattern (fn specimen ejector-spec layout noun-expr opt-guard-expr
    &aux (guardv (gensym "FINAL-GUARD")))
  (check-type noun-expr |NounExpr|)
  (let ((noun (first (node-elements noun-expr))))
    (values
      (append (if opt-guard-expr
                (updating-sequence-expr opt-guard-expr layout guardv :may-inline nil)
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
