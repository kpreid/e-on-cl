; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.elib)

(defvar *exercise-reffiness* nil
  "Set this true to test for code which is inappropriately sensitive to the presence of forwarding refs, by placing them around each argument to methods written in Lisp.")

(defun reffify-args (args)
  "See *EXERCISE-REFFINESS*."
  (mapcar (lambda (x) (make-instance 'resolved-ref :target x)) args))

(defun partition (test sequence &rest options)
  "Return, as two values, the elements of SEQUENCE satisfying the test and those not satisfying the test. Accepts all the options REMOVE-IF[-NOT] does."
  ;; http://paste.lisp.org/display/10050
  (let (not)
    (values
      (apply #'remove-if-not 
             #'(lambda (element)
               (not
                 (unless (funcall test element) 
                   (push element not))))
             sequence
             options)
      (nreverse not))))

(defun mangled-verb-p (thing)
  (and (typep thing 'keyword)
       (find #\/ (symbol-name thing))))

(defun smethod-mverb (smethod prefix-arity
    &aux (verb-or-mverb (first smethod)))
  (etypecase verb-or-mverb
    ((and symbol (or (not keyword) (satisfies mangled-verb-p)))
      verb-or-mverb)
    (keyword
      (let ((verb-string (symbol-name verb-or-mverb)))
        (e.util:mangle-verb
          verb-string
          (multiple-value-bind (min max)
              (progn
                (assert (cddr smethod) () "Inferring arity for function smethod not supported: ~S" smethod)
                (e.util:lambda-list-arguments-range (second smethod)))
            (assert (>= min prefix-arity) () "Method ~S has ~S parameters, which is not enough to accept ~S prefix argument~:P." verb-string min prefix-arity)
            (assert (= min max) () "Variable arguments not yet supported for smethod-case-entry arity inference")
            (- min prefix-arity)))))))

(defun smethod-case-entry (smethod args-sym prefix-args &key type-name
    &aux (mverb (smethod-mverb smethod (length prefix-args))))
  "Return a CASE clause whose key is a mangled-verb, suitable for implementing method dispatch for E objects.

The first element of the smethod is a string designator for the verb. If it contains a slash, it is treated as a mangled-verb (verb/arity); if not, the second element must be a lambda list, which is used to determine the number of arguments.

If there is exactly one further element, then it is an expression (typically #'...) which the returned clause will use in `(apply ,expr ,args-sym).

If there are two or more further elements, then they are a lambda list and implicit-progn forms which will be evaluated.

prefix-args is a list of forms which will be prepended to the arguments of the method body."
  (assert (not (eq (first smethod) 'otherwise)))
  `((,mverb) 
    ,(smethod-body (rest smethod) args-sym prefix-args :type-name type-name :verb-name mverb)))

(defun smethod-function-case-entry (smethod prefix-arity &key type-name
    &aux (mverb (smethod-mverb smethod prefix-arity)))
  (assert (not (eq (first smethod) 'otherwise)))
  `((,mverb)
    ,(smethod-function-form (rest smethod) :type-name type-name :verb-name mverb)))

(defun functional-smethod-body-p (body)
  (= 1 (length body)))

(defun delete-documentation (decls-and-forms)
  "Remove from DECLS-AND-FORMS a value that would be interpreted as a documentation string according to CLHS section 3.4.11, thus allowing it to be used as a body in forms like DESTRUCTURING-BIND."
  (loop with head = '()
        for (thing . rest) on decls-and-forms
        when (and (stringp thing) rest)
          do (return (nreconc head rest))
        do (push thing head)
        finally (return decls-and-forms)))

(defun convert-coercing-lambda-list (coercing-ll body)
  (let* ((ordinary-ll '())
         (lets '()))
    (loop with aux = nil
          for x in coercing-ll do
      (cond
        (aux
          (push x lets))
        ((and (typep x '(cons t (cons t null))))
         (let ((uncoerced (gensym (format nil "UNCOERCED-~A" x))))
           (push uncoerced ordinary-ll)
           (push `(,(first x) (e-coerce ,uncoerced ,(second x))) lets)))
        ((member x '(&aux))
         (setf aux t))
        ((member x '(&rest))
         (push x ordinary-ll))
        ((member x lambda-list-keywords)
         (error "lambda-list-keyword ~S in ~S unsupported by ~S" x coercing-ll 'convert-coercing-lambda-list))
        (t
          ;; so declarations and scoping work right
          (let ((g (gensym (format nil "ENTRY-~A" x))))
             (push g ordinary-ll)
             (push `(,x ,g) lets)))))
    (nreverse-here ordinary-ll)
    (nreverse-here lets)
    (let ((decls '())
          (doc nil))
      (loop while (typep body '(or (cons string               cons)
                                   (cons (cons (eql declare)) t))) do
        ;; (declare ...) or non-final string
        (if (typep (first body) 'string)
          (push (pop body) doc)
          (push (pop body) decls)))
      `(,ordinary-ll ,@doc (let* ,lets ,@(nreverse decls) ,@body)))))

(defun smethod-body (body args-sym prefix-args &key type-name verb-name)
  #-e.method-lambdas
    (declare (ignore type-name))
  (let ((args-form
         (if (keywordp verb-name)
           `(if *exercise-reffiness*
              (reffify-args ,args-sym)
              ,args-sym)
           args-sym)))
    #+e.method-lambdas 
      `(apply ,(smethod-function-form body :type-name type-name :verb-name verb-name) 
              ,@prefix-args ,args-form)
    #-e.method-lambdas
      (if (functional-smethod-body-p body)
        `(apply ,(first body) ,@prefix-args ,args-form)
        (destructuring-bind (post-ll &rest post-body)
            (convert-coercing-lambda-list (first body) (rest body))
          `(destructuring-bind ,post-ll
               (list* ,@prefix-args ,args-form)
             ,@(delete-documentation post-body))))))

(defun smethod-function-form (body &key type-name verb-name)
  "XXX transfer the relevant portions of smethod-case-entry's documentation here"
  (if (functional-smethod-body-p body)
    (first body)
    (let* ((name (format nil "~A#~A" type-name verb-name))
           (name-sym
             ; These are feature conditionals to remind me that they must be set at compile time anyway.
             #-e.intern-vtable-methods
               (make-symbol name)
             #+e.intern-vtable-methods
               (loop 
                 for i from 1
                 for free = name then (format nil "~A-dup-~A" name i)
                 while (find-symbol name :e.elib.vtable-methods) 
                 finally (intern free :e.elib.vtable-methods))))
      `(named-lambda ,name-sym ,@(convert-coercing-lambda-list (first body) (rest body))))))

(defun conventional-constant-symbol-p (specimen)
  (and (symbolp specimen)
       (let ((name (symbol-name specimen)))
         (and (> (length name) 2)
              (eql #\+ (elt name 0))
              (eql #\+ (elt name (1- (length name))))))
       (boundp specimen)))

(deftype global-constant ()
  '(or (cons (eql quote) (cons t null)) ; quote form
       keyword
       (satisfies conventional-constant-symbol-p) ; +foo+
       (not (or symbol cons)))) ; per CLHS 3.1.2.1.3

(defun coercing-lambda-list-item-to-param-desc (item)
  (etypecase item
    ((cons t (cons global-constant null))
     (destructuring-bind (name type-form) item
       (make-instance 'eval-param-desc
         :opt-name (guess-lowercase-string (symbol-name name))
         :form type-form)))
    ((cons t (cons t null))
     (make-instance 'param-desc
       :opt-name (guess-lowercase-string (symbol-name (first item)))))
    (symbol
     (make-instance 'param-desc
       :opt-name (guess-lowercase-string (symbol-name item))))))

(defun coercing-lambda-list-to-param-desc-vector (list arity prefix-arity
    &aux (end (or (position '&aux list) (length list))))
  (unless (= end (+ arity prefix-arity))
    (error "arity ~A + prefix-arity ~A doesn't match lambda list ~S" arity prefix-arity list))
  (coerce (loop 
    for i below arity
    for sym in (nthcdr prefix-arity list)
    do
      (when (member sym lambda-list-keywords)
        (error "constructing param-desc vector from lambda list ~S with keyword ~s not possible" list sym))
    collect 
      (coercing-lambda-list-item-to-param-desc sym)) 'vector))

(defun smethod-message-desc-pair (smethod &rest keys &key (prefix-arity 0) &allow-other-keys
    &aux (mverb (smethod-mverb smethod prefix-arity)))
  (vector (symbol-name mverb) 
          (apply #'smethod-message-desc smethod keys)))

(defun smethod-message-desc (smethod &key (prefix-arity 0)
    &aux (mverb (smethod-mverb smethod prefix-arity))
         (impl-desc (rest smethod)))
  (assert (mangled-verb-p mverb))
  (multiple-value-bind (verb arity) (e-util:unmangle-verb mverb)
    (make-instance 'message-desc
      :verb verb
        :doc-comment (find-if #'stringp impl-desc) ; XXX too lenient
      :params (if (rest impl-desc) ; therefore inline
                (coercing-lambda-list-to-param-desc-vector (first impl-desc) arity prefix-arity)
                (make-array arity :initial-element 
                  (make-instance 'param-desc))))))

(defun smethod-maybe-describe-fresh (function smethod &rest options)
  "messy."
  ;; XXX should be more like mverb-is-magic-p
  (if (typep (first smethod) '(and symbol (not keyword)))
    nil
    (list (apply function smethod options))))
