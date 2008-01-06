; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

; --- base ---

(defgeneric node-elements (node))
(defgeneric walk-node-scopes (node environment))
(defgeneric opt-node-property-getter (node field-keyword))
(defgeneric node-visitor-arguments (node))
(defgeneric node-class-arity (node-class))
(defgeneric node-class-element-types (node-class))
(defgeneric node-class-element-names (node-class))

;; XXX this macro used to be used on its own but is now only used by define-node-class - review and cleanup
(defmacro %def-node-maker (class-sym subnode-flags param-names param-types rest-p
    &aux (span-sym (gensym "SPAN"))
         (jlayout-sym (gensym "SCOPE-JLAYOUT"))
         (param-syms (mapcar (lambda (x) (make-symbol (string x))) param-names))
         (fqn (concatenate 'string "org.erights.e.elang.evm.make"
                                   (symbol-name class-sym))))
  `(setf 
    (get ',class-sym 'static-maker)
    (e-lambda 
        ,fqn
        (:stamped +thread-sharable-stamp+
         :stamped +selfless+
         :stamped +transparent-stamp+)
      (:|__optUncall| ()
        `#(,e.knot:+sharable-importer+ "get" #(,,fqn)))
      (:|asType| () 
        (type-specifier-to-guard ',class-sym))
      (:|getParameterSubnodeFlags| ()
        "Return a list of booleans which indicate whether the corresponding parameter (as used in makers and visitors) is a 'subnode' of the node (e.g. HideExpr's sole parameter) or not (e.g. LiteralExpr's sole parameter). XXX clarify"
        ',(map 'vector #'as-e-boolean subnode-flags))
      (,(locally
          (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
          (e-util:mangle-verb "run" (+ 2 (length param-types))))
        ((,span-sym '(or null source-span))
          ,@(mapcar (lambda (s ty) `(,s ',ty)) param-syms param-types)
          (,jlayout-sym 'null))
        (declare (ignore ,jlayout-sym))
        (make-instance ',class-sym
          :source-span ,span-sym
          :elements
          ,(if rest-p
            `(list* ,@(butlast param-syms) (coerce ,(car (last param-syms)) 'list))
            `(list ,@param-syms)))))))

(defun %setup-node-class (node-type rest-slot property-names dnm-types normal-prop-count)
  (let ((fqn (concatenate 'string "org.erights.e.elang.evm." (symbol-name node-type)))
        (element-types (make-element-attribute-list
                          dnm-types rest-slot
                          (lambda (last-type)
                            (assert (typep last-type '(cons (eql e-list))))
                            (second last-type))))
        (element-names
          (make-element-attribute-list
            property-names rest-slot
            (lambda (last-name)
              `(member-of ,last-name)))))
    (defmethod elib:cl-type-fq-name ((type (eql node-type)))
      fqn)
    
    (defmethod node-class-arity ((node-class (eql (find-class node-type))))
      (values normal-prop-count
              (if rest-slot nil normal-prop-count)))
    (defmethod node-class-element-types ((node-class (eql (find-class node-type))))
      "Returns the possibly-circular list of type specifiers for the type constraints of the elements of a node of this class."
      element-types)
    (defmethod node-class-element-names ((node-class (eql (find-class node-type))))
      "Returns the possibly-circular list of names of the elements of a node of this class."
      element-names)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-element-attribute-list (property-attribute-list rest-slot last-modifier)
    (if rest-slot
        (let ((rest-attrs (list (funcall last-modifier (first (last property-attribute-list))))))
          (setf (rest rest-attrs) rest-attrs)
          (nconc (butlast property-attribute-list) rest-attrs))
        property-attribute-list)))

(defmacro define-node-class (class-name (&rest superclasses) (&rest property-defs) &key rest-slot)
  (let ((normal-prop-count (- (length property-defs) (if rest-slot 1 0)))
        property-names
        subnode-flags 
        dnm-types)
    (loop for pd in property-defs do
      (destructuring-bind (property-name subnode-flag type &key) pd
        (push property-name property-names)
        (push subnode-flag subnode-flags)
        (push type dnm-types)))
    (nreverse-here property-names)
    (nreverse-here subnode-flags)
    (nreverse-here dnm-types)
    `(progn
      (defclass ,class-name ,superclasses ())
      
      (%setup-node-class ',class-name ',rest-slot ',property-names ',dnm-types ',normal-prop-count)
      
      (%def-node-maker ,class-name
        ,subnode-flags
        ,property-names
        ,dnm-types
        ,rest-slot)
      
      ,@(loop for property-name in property-names
              for i below normal-prop-count
              collect
                 `(defmethod opt-node-property-getter 
                      ((node ,class-name) 
                       (field-keyword (eql ',property-name)))
                    (declare (ignore field-keyword))
                    (lambda () (nth ',i (node-elements node)))))
      
      ,@(when rest-slot
        (destructuring-bind (property-name &rest rest) (car (last property-defs))
          (declare (ignore rest))
          `((defmethod opt-node-property-getter 
                ((node ,class-name) 
                 (field-keyword (eql ',property-name)))
              (declare (ignore field-keyword))
              (lambda () (coerce (nthcdr ',normal-prop-count (node-elements node)) 'vector))))))
      
      (defmethod node-visitor-arguments ((node ,class-name))
        ,(if rest-slot
          `(let ((elements (node-elements node)))
             (append (subseq elements 0 ',normal-prop-count)
                     (list (coerce (subseq elements ',normal-prop-count) 'vector))))
          `(node-elements node)))
      
      (defmethod opt-node-property-getter 
          ((node ,class-name) 
           (field-keyword t))
        (declare (ignore field-keyword))
        nil))))

;;; --- Node definitions ---

#+sbcl (sb-ext:unlock-package :e.elang.vm-node)

(defclass |ENode| () 
  ((elements  :initform (error "Can't have an ENode without elements.")
              :initarg :elements
              :accessor node-elements
              :type list)
   (static-scope :initform nil
                 :type (or null function)
                 :accessor node-computed-static-scope)
   (static-scope-lock :initform (make-lock) :accessor static-scope-lock)
   (source-span :initform nil
                :type (or null source-span)
                :initarg :source-span
                :accessor node-source-span)))

(defclass |EExpr| (|ENode|) ())
(defclass |Pattern| (|ENode|) ())

(define-node-class |AssignExpr|      (|EExpr|)
  ((:|noun|   t |EExpr|)
   (:|rValue| t |EExpr|)))
(define-node-class |CallExpr|        (|EExpr|)
  ((:|recipient| t |EExpr|)
   (:|verb| nil string) 
   (:|args| t (e-list |EExpr|)))
  :rest-slot t)
(define-node-class |CatchExpr|       (|EExpr|)
  ((:|attempt| t |EExpr|)
   (:|pattern| t |Pattern|)
   (:|catcher| t |EExpr|)))
(define-node-class |DefineExpr|      (|EExpr|)
  ((:|pattern| t |Pattern|)
   (:|optEjectorExpr| t (or null |EExpr|))
   (:|rValue| t |EExpr|)))
(define-node-class |EscapeExpr|      (|EExpr|)
  ((:|ejectorPattern| t |Pattern|)
   (:|body| t |EExpr|) 
   (:|optCatchPattern| t (or null |Pattern|)) 
   (:|optCatchBody| t (or null |EExpr|))))
(define-node-class |FinallyExpr|     (|EExpr|)
  ((:|attempt| t |EExpr|)
   (:|unwinder| t |EExpr|)))
(define-node-class |HideExpr|        (|EExpr|)
  ((:|block| t |EExpr|)))
(define-node-class |IfExpr|          (|EExpr|)
  ((:|test| t |EExpr|) 
   (:|then| t |EExpr|) 
   (:|else| t |EExpr|)))
(define-node-class |LiteralExpr|     (|EExpr|)
  ;; XXX loosen restriction to Data? different set of literals? include other CL number types?
  ((:|value| nil (or string character rational float64))))
(define-node-class |MetaContextExpr| (|EExpr|)
  ())
(define-node-class |MetaStateExpr|   (|EExpr|)
  ())
(define-node-class |NounExpr|        (|EExpr|)
  ((:|name| nil string)))
(define-node-class |ObjectExpr|      (|EExpr|)
  ((:|docComment| nil doc-comment)
   (:|pattern| t |Pattern|) 
   (:|auditorExprs| t (e-list |EExpr|)) 
   (:|script| t |EScriptoid|)))
(define-node-class |SeqExpr|         (|EExpr|)
  ((:|subs| t (e-list |EExpr|)))
  :rest-slot t)
(define-node-class |SlotExpr|        (|EExpr|)
  ((:|noun| t |EExpr|)))

(defclass |EMethodoid| (|ENode|) ()) ;; to support "to"
(define-node-class |EMethod|         (|EMethodoid|)
  ((:|docComment| nil doc-comment)
   (:|verb| nil string)
   (:|patterns| t (e-list |Pattern|))
   (:|optResultGuard| t (or null |EExpr|))
   (:|body| t |EExpr|)))

(defclass |EScriptoid| (|ENode|) ())
(define-node-class |EScript|         (|EScriptoid|)
  ((:|methods| t (e-list |EMethodoid|))
   (:|matchers| t (e-list |EMatcher|))))

(define-node-class |EMatcher|        (|EScriptoid|)
  ;; as a script, is plumbing
  ((:|pattern| t |Pattern|) 
   (:|body| t |EExpr|)))
                               
(define-node-class |IgnorePattern|   (|Pattern|)
  ())
(define-node-class |ListPattern|     (|Pattern|)
  ((:|subs| t (e-list |Pattern|)))
  :rest-slot t)
(define-node-class |ViaPattern| (|Pattern|)
  ((:|function| t |EExpr|)
   (:|pattern| t |Pattern|)))

(define-node-class |NounPattern|     (|Pattern|)
  ())

(define-node-class |FinalPattern|    (|NounPattern|)
  ((:|noun| t |EExpr|) 
   (:|optGuardExpr| t (or null |EExpr|))))
(define-node-class |SlotPattern|     (|NounPattern|)
  ((:|noun| t |EExpr|) 
   (:|optGuardExpr| t (or null |EExpr|))))
(define-node-class |VarPattern|      (|NounPattern|)
  ((:|noun| t |EExpr|) 
   (:|optGuardExpr| t (or null |EExpr|))))

(defclass |QuasiNode| (|ENode|)
  ; XXX allow inheritance of slot layouts and thus make all quasi-nodes share one definition
  ())

(define-node-class |QuasiLiteralNode| (|QuasiNode|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiPatternNode| (|QuasiNode|)
  ((:|index| nil (integer 0))))

(defclass |QuasiExpr| (|EExpr| |QuasiNode|)
  ())
(defclass |QuasiPattern| (|Pattern| |QuasiNode|)
  ())

(define-node-class |QuasiLiteralExpr| (|QuasiLiteralNode| |QuasiExpr|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiPatternExpr| (|QuasiPatternNode| |QuasiExpr|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiLiteralPatt| (|QuasiLiteralNode| |QuasiPattern|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiPatternPatt| (|QuasiPatternNode| |QuasiPattern|)
  ((:|index| nil (integer 0))))

#+sbcl (sb-ext:lock-package :e.elang.vm-node)

; --- general definitions for nodes ---

(defmethod print-object ((node |ENode|) stream)
  ;; imitating SBCL's hash-table printing
  (if (or (not *print-readably*) (not *read-eval*))
    (print-unreadable-object (node stream :type nil :identity nil)
      (format stream "~A ~W" (type-of node) 
                             (if (slot-boundp node 'elements)
                               (node-elements node)
                               ':.undefined-elements.)))
    (with-standard-io-syntax
          (format stream
                  "#.~W"
                  `(make-instance ',(class-name (class-of node))
                                  ,@(if (slot-boundp node 'elements)
                                      `(:elements ',(node-elements node))))))))

(defmethod make-load-form ((node |ENode|) &optional environment)
  (declare (ignore environment))
  `(locally (declare (notinline make-instance))
     ;; Without this NOTINLINE, SBCL 0.9.6 (at least) will waste time
     ;; and space generating an optimized constructor function for
     ;; this set of constant class-and-initargs.
     (make-instance ',(class-name (class-of node))
                    :elements ',(node-elements node))))

; --- constraints on nodes ---

(define-condition node-arity-error (error)
  ((class :initarg :class :reader attempted-node-class)
   (elements :initarg :elements :reader attempted-node-elements))
  (:report (lambda (condition stream &aux (*package* #.(find-package 
                                                         :e.elang.vm-node)))
             (multiple-value-call 
               #'format 
               stream 
               "Attempted to create a ~S with ~S children, ~:_~S, ~:_instead of ~S-~S." 
               (class-name (attempted-node-class condition))
               (length (attempted-node-elements condition))
               (attempted-node-elements condition)
               (node-class-arity (attempted-node-class condition))))))

(defmethod shared-initialize ((node |ENode|) slot-names &rest initargs &key elements &allow-other-keys)
  (let ((arity (length elements)))
    (multiple-value-bind (min max) (node-class-arity (class-of node))
      (unless (and (>= arity min)
                   (or (null max) (<= arity max)))
        (error 'node-arity-error :class (class-of node)
                                 :elements elements))))
  (apply #'call-next-method node slot-names
    :elements (loop for element in elements
                    for name in (node-class-element-names (class-of node))
                    for type in (node-class-element-types (class-of node))
                    collect (e-coerce element type
                                      (efun (condition-ref)
                                        (error "the ~S of a ~S must be a ~S (~A)" name (class-name (class-of node)) type condition-ref))))
    initargs))

(defun to-scope (nodoid)
  (etypecase nodoid
    (|ENode|
     (e. nodoid |staticScope|))
    ((and vector (not string))
     (sum-default-node-scopes nodoid))))
              
(defun usesp (defining using)
  "Return whether 'using' uses nouns defined by 'defining'.
List nodes will be assumed to be sequences."
  (let ((defining (to-scope defining))
        (using    (to-scope using)))
    (> (e. (e. (e. defining |outNames|)
               |and|
               (e. using |namesUsed|))
           |size|)
       0)))

(defmethod change-class ((node |ENode|) new-class &key &allow-other-keys)
  (declare (ignore new-class))
  (error "change-class is not a good idea for ENodes"))

;; (defmethod shared-initialize :before ((node |QuasiNode|) slot-names &key &allow-other-keys)
;;   (unless (typep node '(or |QuasiLiteralExpr| |QuasiLiteralPatt|
;;                            |QuasiPatternExpr| |QuasiPatternPatt|))
;;     (error "Attempt to construct a quasi-node of unspecific class ~S." (class-of node))))

(defmethod shared-initialize :after ((node |SeqExpr|) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (unless (> (length (funcall (opt-node-property-getter node :|subs|))) 0)
    (error "SeqExpr must have at least one subexpression")))

;;; --- node creation utilities ---

(declaim (inline mn mnp node-quote))

#-sbcl
(declaim (ftype (function ((or symbol class) &rest t) |ENode|)
                mn)
         (ftype (function ((or symbol class) (or null source-span |ENode|) &rest t)
                          |ENode|)
                mnp)
         (ftype (function (t) |ENode|) node-quote))

(defun mn (name &rest elements)
  (make-instance name :elements elements))
(defun mnp (name span &rest elements)
  (make-instance name
    :elements elements
    :source-span (etypecase span
                   (|ENode| (e. span |getOptSpan|))
                   ((or source-span null) span))))

(defun node-quote (value)
  (etypecase value
    ((or string number character) (mn '|LiteralExpr| value))
    ((eql #.+e-true+)  (mn '|NounExpr| "true"))
    ((eql #.+e-false+) (mn '|NounExpr| "false"))
    (null              (mn '|NounExpr| "null"))))

#+sbcl
(progn
  ;; declare constant foldability
  (sb-c:defknown mn
    ((or symbol class) &rest t)
    |ENode|
    (sb-c:unsafe sb-c:unwind sb-c:foldable))
  (sb-c:defknown mnp
    ((or symbol class) (or null source-span |ENode|) &rest t)
    |ENode|
    (sb-c:unsafe sb-c:unwind sb-c:foldable))
  (sb-c:defknown node-quote
    (t)
    |ENode|
    (sb-c:unsafe sb-c:unwind sb-c:foldable)))

; --- E-level methods ---

(defglobals +empty-static-scope+ +has-meta-static-scope+)

(def-vtable |ENode|
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (or (eql auditor +selfless+)
        (eql auditor +transparent-stamp+)
        (eql auditor +thread-sharable-stamp+)))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (let ((quote (e-is-true (e. tw |isQuoting|))))
      (when quote
        (e. tw |print| (e. this |quasiTypeTag|) "`"))
      (e. this |welcome|
        (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw))
      (when quote
        (e. tw |print| "`"))))
  (:|__optUncall| (this)
    ;; xxx if errors in Transparent uncalls should be vat-killing, this is one of them
    `#(,(let ((sm (get (observable-type-of this) 'static-maker)))
          (assert sm () "Missing maker for E-node class ~A" (observable-type-of this))
          sm)
       "run"
       #(nil ,@(node-visitor-arguments this) nil)))
  (:|asText/1| 'e-print)
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "e??")
  (:|staticScope| (this)
    "Return a static scope analysis of this subtree that doesn't depend on the enclosing context."
    (let ((static-scope (with-lock-held ((static-scope-lock this)) (node-computed-static-scope this))))
      (or static-scope
          ;; by releasing the lock here, we avoid blocking other threads. the
          ;; computation might harmlessly happen twice, though.
          (let ((new (walk-node-scopes this +empty-static-scope+)))
            (with-lock-held ((static-scope-lock this)) 
              (setf (node-computed-static-scope this) new))))))
  (:|getOptSpan/0| 'node-source-span)
  (:|substitute| (this (args 'vector))
    "Quasiliteral ValueMaker interface.

NOTE: There is a non-transparent optimization, with the effect that if args == [] and there are quasi-nodes in this tree, they will be returned unreplaced."
    (if (zerop (length args))
      this
      (e. this |welcome|
          (efuncall (e-import "org.erights.e.elang.visitors.makeQuasiSubstituteVisitor") 
                    args))))
  (:|asKernelE/0| 'kernelize)
  (:|welcome| (this visitor)
    (e-call
      visitor
      ;; XXX need to decide on a policy about nonkernel nodes
      ; xxx should we use our alleged type method instead of observable-type-of?
      (concatenate 'string "visit" (symbol-name (observable-type-of this))) 
      (cons this (node-visitor-arguments this))))
  (:|walkScopes| (this environment)
    "Traverse the scope structure of this node. See the org.erights.e.elang.evm.StaticWalkEnvironment interface."
    (walk-node-scopes this environment))
  (:|lnPrintOn| (this tw precedence)
    "Java-E compatibility"
    (e. tw |println|)
    (e. this |subPrintOn| tw precedence))
  (:|subPrintOn| (this tw precedence)
    "Java-E compatibility"
    (e. this |welcome|
      (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw precedence)))
  
  (:|requireKernelE/1| 'require-kernel-e)
  (:|requireFitsScope/2| 'e.knot:require-node-fits-scope))

(defun mverb-get-to-property-name (mverb)
  "Return \"foo\" if mverb is :|getFoo/0|, otherwise nil."
  (when (keywordp mverb)
    (let* ((name (symbol-name mverb))
           (length (length name)))
      (when (and (>= length #.(length "getX/0"))
                 (string= name "get" :end1 3)
                 (string= name "/0" :start1 (- length 2)))
        (concatenate 'string
          (string (char-downcase (char name 3)))
          (subseq name 4 (- length 2)))))))
    
(defmethod e-call-match (fail (rec |ENode|) mverb &rest args)
  (declare (ignore fail))
  (let* ((property (mverb-get-to-property-name mverb))
         (opt-getter (when property
                       (opt-node-property-getter rec 
                                                 (intern property :keyword)))))
    (cond 
      (opt-getter
        (funcall opt-getter))
      ((eql mverb :|__respondsTo/2|)
        (destructuring-bind (verb arity) args
          (as-e-boolean
            (let ((property (mverb-get-to-property-name (mangle-verb verb arity))))
              (when property (opt-node-property-getter rec (intern property :keyword)))))))
      (t
        (call-next-method)))))

(def-vtable |EExpr|
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "e")
  (:|eval| (this scope)
    ;; XXX kernelize here and in evalToPair is done for EoJ compatibility; is it the Right Thing?
    (eval-e (kernelize this) scope))
  (:|evalToPair| (this scope)
    "Evaluate this expression in the given outer scope and return a tuple of the return value and a scope containing any new bindings."
    (multiple-value-call #'vector (eval-e (kernelize this) scope))))

(def-vtable |Pattern|
  (:|getOptPrincipalNoun/0| #'pattern-opt-noun)
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "epatt"))

(defun pattern-qualified-name (pattern)
  (concatenate 'string "$" (or (pattern-opt-noun pattern) "_")))

(def-vtable |ObjectExpr|
  (:|asTypeDesc| (object-expr fqn-prefix)
    (e. +the-make-type-desc+ |run|
      (e. object-expr |getDocComment|)
      (join-fq-name fqn-prefix (pattern-qualified-name (e. object-expr |getPattern|)))
      #()
      #()
      (or-miranda-message-descs
        (loop for method across (let ((script (ref-shorten (e. object-expr |getScript|))))
                                  (etypecase script
                                    (|EScript| (ref-shorten (e. script |getMethods|)))
                                    (|EMatcher| #())))
          for (doc-comment verb patterns opt-result-guard nil) = (node-elements method)
          collect
            (make-instance 'message-desc
              :verb verb
              :doc-comment doc-comment
              :params (map 'vector #'pattern-to-param-desc patterns)
              :opt-result-guard (opt-guard-expr-to-safe-opt-guard opt-result-guard)))))))
     
; --- analysis ---

; XXX pattern-opt-noun and pattern-to-param-desc are looking similar - perhaps pattern-opt-noun should be defined as a wrapper

; XXX expose and test this
(defgeneric pattern-opt-noun (patt))

(defmethod pattern-opt-noun ((patt |Pattern|))
  nil)

(defmethod pattern-opt-noun ((patt |ViaPattern|))
  (pattern-opt-noun (e. patt |getPattern|)))

(defmethod pattern-opt-noun ((patt |NounPattern|))
  (let ((kernel-noun (e-macroexpand-all (e. patt |getNoun|))))
    (when (typep kernel-noun '|NounExpr|)
      (e. kernel-noun |getName|))))


(defgeneric pattern-to-param-desc (pattern))

(defmethod pattern-to-param-desc ((pattern |ENode|))
  (make-instance 'param-desc))

(defmethod pattern-to-param-desc ((pattern |NounPattern|))
  (make-instance 'param-desc
    :opt-name (e. (e. pattern |getNoun|) |getName|)
    :opt-guard (opt-guard-expr-to-safe-opt-guard (e. pattern |getOptGuardExpr|))))

(defmethod pattern-to-param-desc ((patt |ViaPattern|))
  (pattern-to-param-desc (e. patt |getPattern|)))

;; This is a class so that instances may be written to a compiled file.
(defclass false-guard ()
  ((text :initarg :text :type string :accessor %false-guard-text))
  (:documentation "This is a false Guard created by Miranda __getAllegedType() representing the name of the guard expression in this object, but not the actual guard since it would be sometimes a security problem and/or impossible due to the guard expression not being constant over the life of the object."))

(defmethod make-load-form ((a false-guard) &optional environment)
  (make-load-form-saving-slots a :environment environment))
  
(def-vtable false-guard
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +deep-frozen-stamp+))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |write| (%false-guard-text this))))

(defun opt-guard-expr-to-safe-opt-guard (opt-guard-expr
    &aux (opt-guard-string
           (if opt-guard-expr 
             (if (typep (ref-shorten opt-guard-expr) '|NounExpr|)
               ;; optimization
               (e. opt-guard-expr |getName|)
               (e-print opt-guard-expr)))))
  (if opt-guard-string
    (make-instance 'false-guard :text opt-guard-string)))

; --- static scopes ---

(defun extend-ss (ss node kind label)
  ;; muffle non-constant keyword argument warning
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  ;; xxx constructs useless intermediate scope -- review later
  (e. ss |add|
    (make-static-scope kind
      (e. +the-make-const-map+ |fromPairs|
        `#(#(,label ,node))))))

(defun make-static-scope (&key
    (has-meta-state-expr +e-false+)
    (has-outer-meta-state-expr +e-false+)
    (def-names  (e. #() |asMap|))
    (var-names  (e. #() |asMap|))
    (read-names (e. #() |asMap|))
    (set-names  (e. #() |asMap|)))
  (assert (not (and (e-is-true has-outer-meta-state-expr) 
                    (not (e-is-true has-meta-state-expr)))))
  (with-result-promise (self)
    (e-lambda "org.erights.e.elang.evm.StaticScope" (:stamped +selfless+
                                                     :stamped +transparent-stamp+)
      (:|__printOn| ((tw +the-text-writer-guard+))
        (e. tw |print| "<" (e. set-names  |getKeys|) " := "
                           (e. read-names |getKeys|) " =~ "
                           (e. def-names  |getKeys|) " + var "
                           (e. var-names  |getKeys|) 
                           (cond
                             ((e-is-true has-outer-meta-state-expr)
                              ", meta.getState()")
                             ((e-is-true has-meta-state-expr)
                              ", { meta.getState() }")
                             (t 
                              "")) 
                       ">"))
      (:|__optUncall| ()
        `#(,+the-make-static-scope+
           "run"
           #(,set-names ,read-names 
             ,def-names ,var-names 
             ,has-meta-state-expr)))
      (:|add| (right &aux (left self))
        "Yields the sequential composition of the two scopes."
        (let ((left-out   (e. left |outNames|))
              (left-def   def-names)
              (left-var   var-names)
              (left-read  read-names)
              (left-set   set-names)
              (left-meta  has-meta-state-expr)
              (left-oeta  has-outer-meta-state-expr)
              (right-def  (e. right |defNames|))
              (right-var  (e. right |varNames|))
              (right-read (e. right |namesRead|))
              (right-set  (e. right |namesSet|))
              (right-meta (e. right |hasMetaStateExpr|))
              (right-oeta (e. right |hasOuterMetaStateExpr|)))
          (make-static-scope
            :def-names  (e. left-def  |or| right-def)
            :var-names  (e. left-var  |or| right-var)
            :read-names (e. left-read |or| (e. right-read |butNot| left-out))
            :set-names  (e. left-set  |or| (e. right-set  |butNot| left-out))
            :has-meta-state-expr (e. left-meta |or| right-meta)
            :has-outer-meta-state-expr (e. left-oeta |or| right-oeta))))
      (:|namesRead|        () read-names)
      (:|namesSet|         () set-names)
      (:|namesUsed|        () (e. read-names |or| set-names))
      (:|defNames|         () def-names)
      (:|varNames|         () var-names)
      (:|outNames|         () (e. def-names |or| var-names))
      (:|hasMetaStateExpr| () 
        "Whether the node contains a MetaStateExpr."
        ;; XXX after hasOuterMetaStateExpr is implemented, review whether this is needed at all
        has-meta-state-expr)
      (:|hasOuterMetaStateExpr| () 
        "Whether the node contains a MetaStateExpr not enclosed in an object expression."
        has-outer-meta-state-expr)
      (:|hide| () 
        "Yields the StaticScope of this expression as seen enclosed in a block, i.e. hiding definitions."
        (make-static-scope 
          :read-names read-names
          :set-names set-names
          :has-meta-state-expr has-meta-state-expr
          :has-outer-meta-state-expr has-outer-meta-state-expr))
      (:|script| ()
        "Yields the StaticScope of this expression as seen enclosed in an EScript, i.e. hiding definitions and outer meta.getState()."
        (make-static-scope 
          :read-names read-names
          :set-names set-names
          :has-meta-state-expr has-meta-state-expr))
      (:|withAssignment| ((node '|ENode|))
        "Experimental: for ENode#walkScopes"
        (extend-ss self node :set-names (e. node |getName|)))
      (:|withFinalPattern| ((node '|ENode|))
        "Experimental: for ENode#walkScopes"
        (extend-ss self node :def-names (e. (e. node |getNoun|) |getName|)))
      (:|withNounUse| ((node '|ENode|))
        "Experimental: for ENode#walkScopes"
        (extend-ss self node :read-names (e. node |getName|)))
      (:|withVarPattern| ((node '|ENode|))
        "Experimental: for ENode#walkScopes"
        (extend-ss self node :var-names (e. (e. node |getNoun|) |getName|)))
      (:|withSlotPattern| ((node '|ENode|))
        "Experimental: for ENode#walkScopes"
        (extend-ss self node :var-names (e. (e. node |getNoun|) |getName|)))
      (:|withMetaState| ()
        "Experimental: for ENode#walkScopes"
        (e. self |add| +has-meta-static-scope+))
      (:|withSubnode| (key node)
        "Experimental: for ENode#walkScopes"
        (declare (ignore key))
        (e. self |add| (e. node |staticScope|)))
      (:|withSubnodeIndex| (key index node)
        "Experimental: for ENode#walkScopes"
        (declare (ignore key index))
        (e. self |add| (e. node |staticScope|)))
      (:|withHide| (sub-function)
        "Experimental: for ENode#walkScopes"
        (e. self |add| (e. (efuncall sub-function +empty-static-scope+) |hide|)))
      (:|withScript| (sub-function)
        "Experimental: for ENode#walkScopes"
        (e. self |add| (e. (efuncall sub-function +empty-static-scope+) |script|)))
      
      ;; convenience
      (:|uses| (name)
        "Whether execution of the node for which this is the static scope might be affected by the presence of 'name'; that is, whether it contains an explicit usage of 'name' or a MetaStateExpr not inside an object expression."
        (as-e-boolean
          (or (e.tables::maps-no-sugar read-names name)
              (e.tables::maps-no-sugar set-names name)
              (e-is-true has-outer-meta-state-expr)))))))

(defglobal +empty-static-scope+ (make-static-scope))
(defglobal +has-meta-static-scope+ 
  (make-static-scope :has-outer-meta-state-expr +e-true+
                     :has-meta-state-expr +e-true+))

(flet ((make (node kind label)
        ;; muffle non-constant keyword argument warning
        (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
        (make-static-scope kind
          (e. +the-make-const-map+ |fromPairs|
            `#(#(,label ,node))))))
  (defobject +the-make-static-scope+ "org.erights.e.evm.makeStaticScope" 
      (:stamped +deep-frozen-stamp+)
    (:|run| (sn rn dn vn (hms 'e-boolean))
      "General StaticScope constructor. Currently provided only to make StaticScopes selfless."
      (let ((map-guard (e. (e-import "org.erights.e.elib.slot.Map")
                           |get|
                           (type-specifier-to-guard 'string)
                           (type-specifier-to-guard '|ENode|))))
        ;; XXX arrange to not be re-constructing the map guard on every
        ;; invocation (cache per vat)
        
        ;; this is rather loose, but it shouldn't matter, as a StaticScope
        ;; is only worth as much as what you got it from
        
        ;; XXX convert to guarded lambda list
        (e-coercef sn map-guard)
        (e-coercef rn map-guard)
        (e-coercef dn map-guard)
        (e-coercef vn map-guard))
      (make-static-scope :set-names sn
                         :read-names rn
                         :def-names dn
                         :var-names vn
                         :has-meta-state-expr hms))
    (:|scopeAssign| ((node '|ENode|))
      (make node :set-names (e. node |getName|)))
    (:|scopeDef| ((node '|ENode|))
      (make node :def-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeRead| ((node '|ENode|))
      (make node :read-names (e. node |getName|)))
    (:|scopeVar| ((node '|ENode|))
      (make node :var-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeSlot| ((node '|ENode|))
      (make node :var-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeMeta| ()     +has-meta-static-scope+)
    (:|getEmptyScope| () +empty-static-scope+)))

; --- static scope computation ---

(defmacro def-scope-rule (specializer scope-expr)
  "Forms in the scope rule language:
    nil    -- the empty scope
    :|foo| -- the static scope of the property foo of this node, 
              if foo is not nil
    (seq <forms>) -- sequencing, as in StaticScope#add/1
    (hide <form>) -- hiding, as in StaticScope#hide/0
    (script <form>) -- as in StaticScope#script/0
    (flatten :|foo|) -- the sequencing of the list property foo of this node
    (! <CL-form>) -- CL code to return a static scope
    
    Within a ! form, elang::node is bound to this node, elang::environment is bound to the preceding/enclosing environment, and (:get <keyword>) is bound to a property-fetching function."
  (labels ((transform (expr)
            (cond
              ((null expr)
                `environment)
              ((atom expr)
                `(let ((v (:get ,expr)))
                  (if v
                    (e. environment |withSubnode| ',(symbol-name expr) v)
                    environment)))
              ((eql (first expr) 'hide)
                (assert (= (length expr) 2))
                `(e. environment |withHide| (efun (environment) ,(transform (second expr)))))
              ((eql (first expr) 'script)
                (assert (= (length expr) 2))
                `(e. environment |withScript| (efun (environment) ,(transform (second expr)))))
              ((eql (first expr) 'seq)
                (reduce (lambda (env-form rule)
                          `(let ((environment ,env-form))
                             ,(transform rule)))
                        (rest expr)
                        :initial-value `environment))
              ((eql (first expr) '!)
                `(progn ,@(rest expr)))
              ((eql (first expr) 'flatten)
                (assert (= (length expr) 2))
                `(loop with env = environment
                       for node across (or (:get ',(second expr)) #())
                       for i from 0
                       do (setf env (e. env |withSubnodeIndex| 
                                      ',(symbol-name (second expr))
                                      i
                                      node))
                       finally (return env)))
              (t
                (error "Unknown scope-rule expression: ~S" expr)))))
    `(defmethod walk-node-scopes ((node ,specializer) environment)
       (labels ((:get (keyword) (funcall (opt-node-property-getter node keyword))))
         (declare (ignorable #':get))
         ,(transform scope-expr)))))

(defun sum-node-scopes (nodes getter empty)
  (let ((i -1))
    (reduce (lambda (a b) (e. a |add| b))
            nodes
            :key (lambda (node) (funcall getter (incf i) node))
            :initial-value empty)))

;; XXX lousy naming of this vs. the above
(defun sum-default-node-scopes (nodes)
  (sum-node-scopes nodes
                   (lambda (i a) (declare (ignore i)) (e. a |staticScope|))
                   +empty-static-scope+))

(def-scope-rule |AssignExpr|
  (seq (! (e. environment |withAssignment| (:get :|noun|)))
       :|rValue|))

(def-scope-rule |CallExpr|
  (seq :|recipient| (flatten :|args|)))

(def-scope-rule |CatchExpr|
  (seq (hide :|attempt|)
       (hide (seq :|pattern|
                  :|catcher|))))

(def-scope-rule |DefineExpr|
  (seq :|pattern|
       :|optEjectorExpr|
       :|rValue|))

(def-scope-rule |EscapeExpr|
  (seq (hide (seq :|ejectorPattern|  :|body|))
       (hide (seq :|optCatchPattern| :|optCatchBody|))))

(def-scope-rule |FinallyExpr|
  (seq (hide :|attempt|)
       (hide :|unwinder|)))
    
(def-scope-rule |HideExpr|
  (hide :|block|))

(def-scope-rule |IfExpr|
  (seq (hide (seq :|test| :|then|))
       (hide :|else|)))

(def-scope-rule |LiteralExpr|
  nil)

(def-scope-rule |MetaContextExpr|
  nil)

(def-scope-rule |MetaStateExpr|
  (! (e. environment |withMetaState|)))

(def-scope-rule |NounExpr|
  (! (e. environment |withNounUse| node)))

(def-scope-rule |ObjectExpr|
  (seq :|pattern|
       (hide (flatten :|auditorExprs|))
       (script :|script|)))

(def-scope-rule |SeqExpr|
  (flatten :|subs|))

(def-scope-rule |SlotExpr|
  (! (e. environment |withNounUse| (:get :|noun|))))


(def-scope-rule |FinalPattern|
  (seq :|optGuardExpr|
       (! (e. environment |withFinalPattern| node))))

(def-scope-rule |IgnorePattern|
  nil)

(def-scope-rule |ListPattern|
  (flatten :|subs|))

(def-scope-rule |SlotPattern|
  (seq :|optGuardExpr|
       (! (e. environment |withSlotPattern| node))))

(def-scope-rule |VarPattern|
  (seq :|optGuardExpr|
       (! (e. environment |withVarPattern| node))))

(def-scope-rule |ViaPattern|
  (seq :|function| :|pattern|))

(def-scope-rule |EMatcher|
  (hide (seq :|pattern|
             :|body|)))

(def-scope-rule |EMethod|
  (hide (seq (flatten :|patterns|)
             :|optResultGuard|
             :|body|)))

(def-scope-rule |EScript|
  (seq (flatten :|methods|)
       (flatten :|matchers|)))

;;; --- scope utilities ---

;; a keyword is a property name for the node
;; a list is a name and a function to be applied to the node
(defun deref-node-part (node part)
  (etypecase part
    (keyword (funcall (opt-node-property-getter node part)))
    (list (funcall (second part) node))))

(defun name-node-part (part)
  (with-standard-io-syntax
    (etypecase part
      (keyword (write-to-string part :readably nil :escape nil))
      (list (first part)))))

(define-condition misleading-usage-error (error)
  ;; XXX bad accessor names
  ((node :type |ENode|
         :initarg :node
         :reader node)
   (defining-property :type keyword
                      :initarg :defining-property
                      :reader defining-property)
   (using-property :type keyword
                   :initarg :using-property
                   :reader using-property))
  (:report (lambda (condition stream
               &aux (node (node condition))
                    (definer (defining-property condition))
                    (user (using-property condition))
                    (node-d (deref-node-part node definer))
                    (node-u (deref-node-part node user)))
             (format stream "a ~A's ~A (~A) may not ~:[~;appear to ~]use ~
                             definitions from its ~A (~A)"
                            (observable-type-of node)
                            (name-node-part user) (e-quote node-u)
                            (typep condition 'misleading-apparent-usage-error)
                            (name-node-part definer) (e-quote node-d)))))

(define-condition misleading-actual-usage-error (misleading-usage-error) ())
(define-condition misleading-apparent-usage-error (misleading-usage-error) ())

(defun reject-definition-usage (node ejector actual definer user)
  (let ((node-d (e-macroexpand-all (deref-node-part node definer)))
        (node-u (e-macroexpand-all (deref-node-part node user))))
  (when (and node-d node-u (usesp node-d node-u))
    (ejerror ejector (if actual
                       'misleading-actual-usage-error
                       'misleading-apparent-usage-error)
                     :node node
                     :defining-property definer
                     :using-property user))))


;;; --- Kernel-E checking ---

(defun unmagical-typep (value type)
  "awful hack: E-LIST type can't work if expanded into a compiled file due to using (satisfies #:uninterned-symbol) so we ensure that TYPEP isn't optimized by indirecting through a redefinable function"
  (typep value type))

(defmacro define-kernel-e-type-constraints (node-class &rest rules)
  `(define-kernel-e-check-method ,node-class
     (check-property-types ,@rules)))

(defun %check-property-types (node ejector rules)
  (loop for (property ptype) in rules collect
    (let ((value (funcall (opt-node-property-getter node property))))
      (unless (unmagical-typep (ref-shorten value) ptype)
        (ejerror ejector "~A must have ~A of type ~A, but had ~A" 
                         (e-quote node) (e-quote property) (e-print ptype) (e-print value))))))

(defmacro define-kernel-e-check-method (node-class &body body)
  `(defmethod require-kernel-e-recursive progn ((node ,node-class) ejector)
     (macrolet ((check-property-types (&rest rules)
                  `(%check-property-types node ejector ',rules)))
       (flet ((this-reject-usage (actual definer user)
                (reject-definition-usage node ejector actual definer user)))
         (declare (ignorable #'this-reject-usage))
         ,@body))))

(defun require-kernel-e (node ejector)
  "Verify that the node is a valid Kernel-E tree."
  (require-kernel-e-recursive node ejector)
  (walk-node-scopes node (make-scope-checker ejector))
  (values))

(defun make-scope-checker (ejector) 
  (labels ((make (finals)
             ;; XXX use hash tables?
             (e-lambda |kernelScopeChecker| ()
               ;; XXX report implements StaticWalkEnvironment
               (:|withHide| (subf) 
                 (efuncall subf |kernelScopeChecker|)
                 |kernelScopeChecker|)
               (:|withScript| (subf) 
                 (efuncall subf |kernelScopeChecker|)
                 |kernelScopeChecker|)
               (:|withAssignment| (node)
                 (check-type node |NounExpr|)
                 (let ((noun (e. node |getName|)))
                   (if (member noun finals :test #'string=)
                     ;; XXX message could use some improvement
                     (ejerror ejector "~A is not an assignable variable" noun))
                     |kernelScopeChecker|))
               (:|withFinalPattern| (node)
                 (check-type node |FinalPattern|)
                 (make (cons (e. (e. node |getNoun|) |getName|) finals)))
               (:|withNounUse| (node)
                 (declare (ignore node))
                 |kernelScopeChecker|)
               (:|withVarPattern| (node)
                 (check-type node |VarPattern|)
                 (make (remove (e. (e. node |getNoun|) |getName|) finals :test #'string=)))
               (:|withSlotPattern| (node)
                 (check-type node |SlotPattern|)
                 (make (remove (e. (e. node |getNoun|) |getName|) finals :test #'string=)))
               (:|withMetaState| () |kernelScopeChecker|)
               (:|withSubnode| (key node)
                 (declare (ignore key))
                 (walk-node-scopes node |kernelScopeChecker|))
               (:|withSubnodeIndex| (key index node)
                 (declare (ignore key index))
                 (walk-node-scopes node |kernelScopeChecker|)))))
    (make nil)))

(defgeneric require-kernel-e-recursive (node ejector)
  (:method-combination progn))

(defmethod require-kernel-e-recursive progn ((node null) ejector)
  nil)

(defmethod require-kernel-e-recursive progn ((node vector) ejector)
  (map nil (lambda (sub) (require-kernel-e-recursive sub ejector)) node))

(defmethod require-kernel-e-recursive progn ((node |ENode|) ejector)
  (loop with maker = (get (class-name (class-of node)) 'static-maker)
        for subnode-flag across (e. maker |getParameterSubnodeFlags|)
        for maybe-subnode in (node-visitor-arguments node)
        when (e-is-true subnode-flag)
          do (require-kernel-e-recursive maybe-subnode ejector)))


(define-kernel-e-check-method |DefineExpr|
  (this-reject-usage nil :|pattern|        :|rValue|)
  (this-reject-usage nil :|pattern|        :|optEjectorExpr|)
  (this-reject-usage t   :|rValue|         :|pattern|)
  (this-reject-usage t   :|optEjectorExpr| :|pattern|))

(define-kernel-e-check-method |ObjectExpr|
  (this-reject-usage nil :|auditorExprs| :|script|)
  (this-reject-usage t   :|pattern|      :|auditorExprs|)
  (check-property-types
    (:|script| (or |EScript| |EMatcher|))))

(define-kernel-e-type-constraints |AssignExpr|
  (:|noun| |NounExpr|))

(define-kernel-e-check-method |NounPattern|
  (check-property-types
    (:|noun| |NounExpr|))
  (let ((guard (e. node |getOptGuardExpr|)))
    (when (and guard (usesp node guard))
      (ejerror ejector "kernel ~A may not use its own noun (~A) in its guard (~A)"
                       (observable-type-of node)
                       (e-quote (e. node |getNoun|))
                       (e-quote guard)))))

(define-kernel-e-type-constraints |EScript|
  (:|methods| (e-list |EMethod|))
  (:|matchers| (e-list |EMatcher|)))
  
