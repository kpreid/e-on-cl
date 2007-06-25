; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defun nl-miranda-case-maybe (verb smethods &rest body)
  (unless (find verb smethods :key (lambda (e) (smethod-mverb e 0)))
    `(((,verb) ,@body))))

(defmacro %fqn-prefix ()
  (format nil "~A$" (or *compile-file-truename* 
                        *load-truename* 
                        "<unknown-lisp>")))

(defmacro nest-fq-name ((qname) &body body &environment env)
  `(macrolet ((%fqn-prefix () 
               ,(join-fq-name (environment-fqn-prefix env)
                              (concatenate 'string qname "$"))))
    ,@body))

(defun environment-fqn-prefix (env)
  (macroexpand '(%fqn-prefix) env))

(defun e-lambda-expansion (smethods fqn documentation stamp-forms opt-otherwise-body
    &aux (self-func (make-symbol fqn))
         (self-expr `#',self-func)
         (stamps-sym (gensym "STAMPS"))
         (mverb-sym  (gensym "MVERB"))
         (args-sym   (gensym "ARGS"))
         (miranda-return-sym (gensym "FAIL")))
  (declare (string fqn documentation))
  `(let ((,stamps-sym (vector ,@stamp-forms)))
    (declare (ignorable ,stamps-sym)) ;; will be ignored if there is an explicit audited-by-magic-verb method
    (nest-fq-name (,fqn)
      (labels ((,self-func (,mverb-sym &rest ,args-sym)
        (flet ((e-lambda-type-desc () ; deliberately accessible
                 (make-instance 'type-desc
                   :doc-comment ',documentation
                   :fq-name ',fqn
                   :supers #()
                   :auditors #()
                   :message-types-v
                     (or-miranda-message-descs
                       ',(mapcan (lambda (e) (smethod-maybe-describe-fresh #'smethod-message-desc e)) smethods)))))
          (case ,mverb-sym
            ,@(loop for smethod in smethods
                    collect (smethod-case-entry smethod args-sym '() :type-name fqn))
            ,@(nl-miranda-case-maybe :|__printOn/1| smethods `(destructuring-bind (tw) ,args-sym
              (e-coercef tw +the-text-writer-guard+)
              (e. tw |print| ',(format nil "<~A>" (simplify-fq-name fqn)))
              nil))
            ,@(nl-miranda-case-maybe :|__getAllegedType/0| smethods `(destructuring-bind () ,args-sym
              (e-lambda-type-desc)))

            ,@(nl-miranda-case-maybe :|__respondsTo/2| smethods `(destructuring-bind (verb arity) ,args-sym
              (e-coercef verb 'string)
              (e-coercef arity '(integer 0))
              (as-e-boolean (or
                (member (e-util:mangle-verb verb arity) 
                        ',(mapcar (lambda (smethod) (smethod-mverb smethod 0)) 
                                  smethods))
                (e-is-true (elib:miranda ,self-expr ,mverb-sym ,args-sym #'funcall)))))) ;; XXX should invoke matcher
            ,@(nl-miranda-case-maybe 'elib:audited-by-magic-verb smethods `(destructuring-bind (auditor) ,args-sym
              (not (not (find auditor ,stamps-sym :test #'samep)))))
            (otherwise 
              (elib:miranda ,self-expr ,mverb-sym ,args-sym (lambda (,miranda-return-sym)
                (declare (ignorable ,miranda-return-sym))
                ,(if opt-otherwise-body
                   (smethod-body opt-otherwise-body `(cons ,mverb-sym ,args-sym) '() :type-name fqn)
                   `(funcall ,miranda-return-sym)))))))))
        ,self-expr))))

(defmacro efun (method-first &body method-rest &environment env)
  (e-lambda-expansion `((:|run| ,method-first ,@method-rest)) (join-fq-name (environment-fqn-prefix env) "$_") "" '() nil))

(defmacro e-lambda (qname (&rest options) &body smethods &environment env)
  "XXX document this

fqn may be NIL, a string, or a symbol, in which case the symbol is bound to the object itself."
  (check-type qname (or null string symbol))
  (when (typep qname '(and symbol (not null)))
    (return-from e-lambda 
      `(with-result-promise (,qname)
         (e-lambda ,(concatenate 'string "$" (symbol-name qname)) 
                   (,@options) ,@smethods))))
  (let ((fqn (join-fq-name (environment-fqn-prefix env) (or qname "_")))
        (stamp-forms '())
        (documentation ""))
    ;; Parse options
    (loop for (key value) on options by #'cddr do
      (case key
        (:stamped
          (when (find 'audited-by-magic-verb smethods :key (lambda (e) (smethod-mverb e 0)))
            ;; XXX make this test cleaner
            ;; XXX this test should be done in e-lambda-expansion instead of here, so that all forms of e-lambdas get it
            (error "Both ~S option and ~S specified in e-lambda ~S." :stamped 'audited-by-magic-verb fqn))
          (push value stamp-forms))
        (:doc
          (assert (string= documentation "") (documentation) "duplicate :doc option")
          (setf documentation value))
        (otherwise
          (error "Unrecognized option ~S ~S in e-lambda ~S."
                 key value fqn))))
    (check-type documentation string)
    ;; Extract 'otherwise' method
    (multiple-value-bind (otherwises specifics)
        (partition (lambda (x) (typep x '(cons (eql otherwise) t)))
                   smethods)
      (assert (<= (length otherwises) 1))
      ;; Expansion
      (e-lambda-expansion specifics fqn documentation stamp-forms (rest (first otherwises))))))
  
