; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

; --- base ---

(defgeneric node-elements (node))
(defgeneric compute-node-static-scope (node))
(defgeneric reduce-scopewise (node builder))
(defgeneric opt-node-property-getter (node field-keyword))
(defgeneric node-visitor-arguments (node))
(defgeneric node-class-arity (node-class))
(defgeneric node-class-element-types (node-class))
(defgeneric node-class-element-names (node-class))

;; XXX this macro used to be used on its own but is now only used by define-node-class - review and cleanup
(defmacro %def-node-maker (class-sym subnode-flags param-types rest-p
    &aux (span-sym (gensym "SPAN"))
         (jlayout-sym (gensym "SCOPE-JLAYOUT"))
         (param-syms (loop repeat (length param-types) collect (gensym))))
  `(setf 
    (get ',class-sym 'static-maker)
    (e-lambda 
        ,(concatenate 'string "org.erights.e.elang.evm.make"
                              (symbol-name class-sym))
        ()
      (:|asType| () 
        (type-specifier-to-guard ',class-sym))
      (:|getParameterSubnodeFlags| ()
        "Return a list of booleans which indicate whether the corresponding parameter (as used in makers and visitors) is a 'subnode' of the node (e.g. HideExpr's sole parameter) or not (e.g. LiteralExpr's sole parameter). XXX clarify"
        ',(map 'vector #'as-e-boolean subnode-flags))
      (,(locally
          (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
          (e-util:mangle-verb "run" (+ 2 (length param-types))))
        (,span-sym ,@param-syms ,jlayout-sym)
        (assert (null ,jlayout-sym))
        (e-coercef ,span-sym '(or null source-span))
        ,@(loop for param in param-syms
                for type in param-types
                ;; XXX half-baked fix for vector args
                collect `(progn 
                           (when (typep (setf ,param (ref-shorten ,param)) '(and vector (not string)))
                             (setf ,param (map 'vector #'ref-shorten ,param)))
                           (e-coercef ,param ',type)))
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
                 :type (or null function))
   (source-span :initform nil
                :type (or null source-span)
                :initarg :source-span)))

(define-node-class |EExpr|   (|ENode|)
  ())
(define-node-class |Pattern| (|ENode|)
  ())

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
   (:|rValue| t |EExpr|)
   (:|optEjectorExpr| t (or null |EExpr|))))
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
  ;; XXX loosen restriction to DeepPassByCopy? different set of literals? include other CL float types?
  ((:|value| nil (or string character rational float64))))
(define-node-class |MetaContextExpr| (|EExpr|)
  ())
(define-node-class |MetaStateExpr|   (|EExpr|)
  ())
(define-node-class |MatchBindExpr|   (|EExpr|)
  ((:|specimen| t |EExpr|) 
   (:|pattern| t |Pattern|)))
(define-node-class |NounExpr|        (|EExpr|)
  ((:|name| nil string)))
(define-node-class |ObjectExpr|      (|EExpr|)
  ((:|docComment| nil string)
   (:|qualifiedName| nil string) 
   (:|auditorExprs| t (e-list |EExpr|)) 
   (:|script| t |EScriptoid|)))
(define-node-class |SeqExpr|         (|EExpr|)
  ((:|subs| t (e-list |EExpr|)))
  :rest-slot t)
(define-node-class |SlotExpr|        (|EExpr|)
  ((:|noun| t |EExpr|)))

(defclass |EMethodoid| (|ENode|) ()) ;; to support "to"
(define-node-class |EMethod|         (|EMethodoid|)
  ((:|docComment| nil string)
   (:|verb| nil string)
   (:|patterns| t (e-list |Pattern|))
   (:|optResultGuard| t (or null |EExpr|))
   (:|body| t |EExpr|)))
(define-node-class |EMatcher|        (|ENode|)
  ((:|pattern| t |Pattern|) 
   (:|body| t |EExpr|)))

(defclass |EScriptoid| (|ENode|) ())
(define-node-class |EScript|         (|EScriptoid|)
  ((:|optMethods| t (or null (e-list |EMethodoid|)))
   (:|matchers| t (e-list |EMatcher|))))
                               
(define-node-class |CdrPattern|      (|Pattern|)
  ((:|listPatt| t |ListPattern|) 
   (:|restPatt| t |Pattern|)))
(define-node-class |IgnorePattern|   (|Pattern|)
  ())
(define-node-class |ListPattern|     (|Pattern|)
  ((:|subs| t (e-list |Pattern|)))
  :rest-slot t)
(define-node-class |SuchThatPattern| (|Pattern|)
  ((:|pattern| t |Pattern|) 
   (:|test| t |EExpr|)))

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

(define-node-class |QuasiNode| (|ENode|)
  ; XXX allow inheritance of slot layouts and thus make all quasi-nodes share one definition
  ())

(define-node-class |QuasiLiteralNode| (|QuasiNode|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiPatternNode| (|QuasiNode|)
  ((:|index| nil (integer 0))))

(define-node-class |QuasiExpr| (|EExpr| |QuasiNode|)
  ())
(define-node-class |QuasiPatt| (|Pattern| |QuasiNode|)
  ())

(define-node-class |QuasiLiteralExpr| (|QuasiLiteralNode| |QuasiExpr|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiPatternExpr| (|QuasiPatternNode| |QuasiExpr|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiLiteralPatt| (|QuasiLiteralNode| |QuasiPatt|)
  ((:|index| nil (integer 0))))
(define-node-class |QuasiPatternPatt| (|QuasiPatternNode| |QuasiPatt|)
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
                               '.undefined-elements.)))
    (with-standard-io-syntax
          (format stream
                  "#.~W"
                  `(make-instance ',(class-name (class-of node))
                                  ,(if (slot-boundp node 'elements)
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
               "~@<Attempted to create a ~S with ~S children, ~:_~S, ~:_instead of ~S-~S.~:> ~:_~@<This probably means you are using an incompatible version of E-on-Java, or haven't fixed the bug described at: ~:_<http://www.eros-os.org/pipermail/e-lang/2006-January/011049.html>~:>" 
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

(defun usesp (defining using)
  "Return whether 'using' uses nouns defined by 'defining'."
  (> (e. (e. (e. (e. defining |staticScope|) |outNames|)
             |and|
             (e. (e. using |staticScope|) |namesUsed|))
         |size|)
     0))

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

(defmethod shared-initialize :after ((node |EScript|) slot-names &key &allow-other-keys
    &aux (methods (funcall (opt-node-property-getter node :|optMethods|)))
         (matchers (funcall (opt-node-property-getter node :|matchers|))))
  (declare (ignore slot-names))
  (unless (or methods (> (length matchers) 0))
      (error "EScript must have methods or at least one matcher")))

;; XXX reject NounPatterns that use their noun in the guard

; --- E-level methods ---

(def-vtable |ENode|
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +selfless-stamp+))
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (let ((quote (e-is-true (e. tw |isQuoting|))))
      (when quote
        (e. tw |print| (e. this |quasiTypeTag|) "`"))
      (e. this |welcome|
        (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw))
      (when quote
        (e. tw |print| "`"))))
  (:|__optUncall| (this)
    ;; xxx if errors in Selfless uncalls should be vat-killing, this is one of them
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
    (with-slots (static-scope) this
      (or static-scope
          (setf static-scope (compute-node-static-scope this)))))
  (:|getOptSpan| (this)
    (slot-value this 'source-span))
  (:|substitute| (this args)
    "Quasiliteral ValueMaker interface.

NOTE: There is a non-transparent optimization, with the effect that if args == [] and there are quasi-nodes in this tree, they will be returned unreplaced."
    (e-coercef args 'vector)
    (kernelize
      (if (zerop (length args))
        this
        (e. this |welcome|
            (e. (e-import "org.erights.e.elang.visitors.makeQuasiSubstituteVisitor") 
                |run| args)))))
  (:|asKernelE/0| 'kernelize)
  (:|welcome| (this visitor)
    (e-call
      visitor
      ;; XXX need to decide on a policy about nonkernel nodes
      ; xxx should we use our alleged type method instead of observable-type-of?
      (concatenate 'string "visit" (symbol-name (observable-type-of this))) 
      (cons this (node-visitor-arguments this))))
  (:|lnPrintOn| (this tw precedence)
    "Java-E compatibility"
    (e. tw |println|)
    (e. this |subPrintOn| tw precedence))
  (:|subPrintOn| (this tw precedence)
    "Java-E compatibility"
    (e. this |welcome|
      (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw precedence))))

(defun mverb-get-to-property-name (mverb)
  "Return \"foo\" if mverb is :|getFoo/0|, otherwise nil."
  (when (keywordp mverb)
    (let* ((name (symbol-name mverb))
           (length (length name)))
      (when (and (>= length 5)
                 (string= name "get" :end1 3)
                 (string= name "/0" :start1 (- length 2)))
        (concatenate 'string
          (string (char-downcase (char name 3)))
          (subseq name 4 (- length 2)))))))
    
(defmethod e-call-match ((rec |ENode|) mverb &rest args)
  (declare (ignore args))
  (let* ((property (mverb-get-to-property-name mverb))
         (opt-getter (when property
                       (opt-node-property-getter rec 
                                                 (intern property :keyword)))))
    (if opt-getter
      (funcall opt-getter)
      (call-next-method))))

(def-vtable |EExpr|
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "e")
  (:|eval/1| 'eval-e)
  (:|evalToPair| (this scope)
    "Evaluate this expression in the given outer scope and return a tuple of the return value and a scope containing any new bindings."
    (multiple-value-call #'vector (eval-e this scope))))

(def-vtable |Pattern|
  (:|getOptPrincipalNoun/0| #'pattern-opt-noun)
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "epatt"))
     
; --- analysis ---

; XXX pattern-opt-noun and pattern-to-param-desc are looking similar - perhaps pattern-opt-noun should be defined as a wrapper

; XXX expose and test this
(defgeneric pattern-opt-noun (patt))

(defmethod pattern-opt-noun ((patt |Pattern|))
  nil)

(defmethod pattern-opt-noun ((patt |SuchThatPattern|))
  (pattern-opt-noun (first (node-elements patt))))

(defmethod pattern-opt-noun ((patt |NounPattern|))
  ; XXX use method of noun expr once we have it
  (first (node-elements (e. patt |getNoun|))))


(defgeneric pattern-to-param-desc (pattern))

(defmethod pattern-to-param-desc ((pattern |ENode|))
  (make-instance 'param-desc))

(defmethod pattern-to-param-desc ((pattern |NounPattern|))
  (make-instance 'param-desc
    :opt-name (e. (e. pattern |getNoun|) |getName|)
    :opt-guard (opt-guard-expr-to-safe-opt-guard (e. pattern |getOptGuardExpr|))))

(defmethod pattern-to-param-desc ((patt |SuchThatPattern|))
  (pattern-to-param-desc (first (node-elements patt))))

;; This is a class so that instances may be written to a compiled file.
(defclass false-guard ()
  ((text :initarg :text :type string))
  (:documentation "This is a false Guard created by Miranda __getAllegedType() representing the name of the guard expression in this object, but not the actual guard since it would be sometimes a security problem and/or impossible due to the guard expression not being constant over the life of the object."))

(defmethod make-load-form ((a false-guard) &optional environment)
  (make-load-form-saving-slots a :environment environment))
  
(def-vtable false-guard
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +deep-frozen-stamp+))
  (:|__printOn| (this tw) 
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |write| (slot-value this 'text))))

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

(defun make-static-scope (&key
    (has-meta-state-expr +e-false+)
    (def-names  (e. #() |asMap|))
    (var-names  (e. #() |asMap|))
    (read-names (e. #() |asMap|))
    (set-names  (e. #() |asMap|)))
  (with-result-promise (self)
    (e-lambda "org.erights.e.elang.evm.StaticScope" (:stamped +selfless-stamp+)
      (:|__printOn| (tw)
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |print| "<" (e. set-names  |getKeys|) " := "
                           (e. read-names |getKeys|) " =~ "
                           (e. def-names  |getKeys|) " + var "
                           (e. var-names  |getKeys|) 
                           (if (e-is-true has-meta-state-expr)
                             ", meta.getState()" 
                             "") 
                       ">"))
      (:|__optUncall| ()
        `#(,+the-make-static-scope+
           "run"
           #(,set-names ,read-names 
             ,def-names ,var-names 
             ,has-meta-state-expr)))
      (:|add| (right &aux (left self))
        (let ((left-out   (e. left |outNames|))
              (left-def   def-names)
              (left-var   var-names)
              (left-read  read-names)
              (left-set   set-names)
              (left-meta  has-meta-state-expr)
              (right-def  (e. right |defNames|))
              (right-var  (e. right |varNames|))
              (right-read (e. right |namesRead|))
              (right-set  (e. right |namesSet|))
              (right-meta (e. right |hasMetaStateExpr|)))
          (make-static-scope
            :def-names  (e. left-def  |or| right-def)
            :var-names  (e. left-var  |or| right-var)
            :read-names (e. left-read |or| (e. right-read |butNot| left-out))
            :set-names  (e. left-set  |or| (e. right-set  |butNot| left-out))
            :has-meta-state-expr (e. left-meta |or| right-meta))))
      (:|namesRead|        () read-names)
      (:|namesSet|         () set-names)
      (:|namesUsed|        () (e. read-names |or| set-names))
      (:|defNames|         () def-names)
      (:|varNames|         () var-names)
      (:|outNames|         () (e. def-names |or| var-names))
      (:|hasMetaStateExpr| () has-meta-state-expr)
      (:|hide|             () (make-static-scope 
                                :read-names read-names
                                :set-names set-names
                                :has-meta-state-expr has-meta-state-expr)))))

(defglobal +empty-static-scope+ (make-static-scope))
(defglobal +has-meta-static-scope+ (make-static-scope :has-meta-state-expr +e-true+))

(flet ((make (node kind label)
        ;; muffle non-constant keyword argument warning
        (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
        (make-static-scope kind
          (e. +the-make-const-map+ |fromPairs|
            `#(#(,label ,node))))))
  (defglobal +the-make-static-scope+ (e-lambda "org.erights.e.evm.makeStaticScope" ()
    (:|run| (sn rn dn vn hms)
      "General StaticScope constructor. Currently provided only to make StaticScopes selfless."
      (let ((map-guard (e. (e-import "org.erights.e.elib.slot.Map")
                           |get|
                           (type-specifier-to-guard 'string)
                           (type-specifier-to-guard '|ENode|))))
        ;; XXX arrange to not be re-constructing the map guard on every
        ;; invocation (cache per vat)
        
        ;; this is rather loose, but it shouldn't matter, as a StaticScope
        ;; is only worth as much as what you got it from
        (e-coercef sn map-guard)
        (e-coercef rn map-guard)
        (e-coercef dn map-guard)
        (e-coercef vn map-guard))
      (e-coercef sn 'boolean)
      (make-static-scope :set-names sn
                         :read-names rn
                         :def-names dn
                         :var-names vn
                         :has-meta-state-expr hms))
    (:|scopeAssign| (node)
      (e-coercef node '|ENode|)
      (make node :set-names (e. node |getName|)))
    (:|scopeDef| (node)
      (e-coercef node '|ENode|)
      (make node :def-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeRead| (node)
      (e-coercef node '|ENode|)
      (make node :read-names (e. node |getName|)))
    (:|scopeVar| (node)
      (e-coercef node '|ENode|)
      (make node :var-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeSlot| (node)
      (e-coercef node '|ENode|)
      (make node :var-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeMeta| ()     +has-meta-static-scope+)
    (:|getEmptyScope| () +empty-static-scope+))))

; --- static scope computation ---

(defmacro def-scope-rule (specializer scope-expr)
  "Forms in the scope rule language:
    nil    -- the empty scope
    :|foo| -- the static scope of the property foo of this node, 
              if foo is not nil
    (seq <forms>) -- sequencing, as in StaticScope#add/1
    (hide <form>) -- hiding, as in StaticScope#hide/0
    (flatten :|foo|) -- the sequencing of the list property foo of this node
    (! <CL-form>) -- CL code to return a static scope
    
    Within a ! form, elang::node is bound to this node, elang::builder is bound to the scope builder, and (:get <keyword>) is bound to a property-fetching function."
  (labels ((transform (expr)
            (cond
              ((null expr)
                `(e. builder |getEmptyScope|))
              ((atom expr)
                `(let ((.value. (:get ',expr)))
                   (if .value.
                     (get-scope .value.)
                     (e. builder |getEmptyScope|))))
              ((eql (first expr) 'hide)
                (assert (= (length expr) 2))
                `(e. ,(transform (second expr)) |hide|))
              ((eql (first expr) 'seq)
                (reduce #'(lambda (a b) `(e. ,a |add| ,b)) 
                        (rest expr)
                        :key #'transform
                        :initial-value `(e. builder |getEmptyScope|)))
              ((eql (first expr) '!)
                `(progn ,@(rest expr)))
              ((eql (first expr) 'flatten)
                (assert (= (length expr) 2))
                `(sum-node-scopes (or (:get ',(second expr)) #()) #'get-scope (e. builder |getEmptyScope|)))
              (t
                (error "Unknown scope-rule expression: ~S" expr)))))
    `(progn
      ;; XXX remove this duplication.
      ;; Note that compute-node-static-scope currently can't be expressed by reduce-scopewise due to the caching layer
      (defmethod compute-node-static-scope ((node ,specializer))
        (flet ((:get (keyword) (funcall (opt-node-property-getter node keyword)))
               (get-scope (node) (e. node |staticScope|)))
          (declare (ignorable (function :get) (function get-scope)))
          (let ((builder +the-make-static-scope+))
          ,(transform scope-expr))))
      (defmethod reduce-scopewise ((node ,specializer) builder)
        (flet ((:get (keyword) (funcall (opt-node-property-getter node keyword)))
               (get-scope (node) (reduce-scopewise node builder)))
          (declare (ignorable (function :get) (function get-scope)))
          ,(transform scope-expr))))))

(defun sum-node-scopes (nodes getter empty)
  (reduce #'(lambda (a b) (e. a |add| b))
          (map 'list getter nodes)
          :initial-value empty))

(def-scope-rule |AssignExpr|
  (seq (! (e. builder |scopeAssign| (:get :|noun|)))
       :|rValue|))

(def-scope-rule |CallExpr|
  (seq :|recipient| (flatten :|args|)))

(def-scope-rule |CatchExpr|
  (hide (seq (hide :|attempt|)
             :|pattern|
             :|catcher|)))

(def-scope-rule |DefineExpr|
  (seq :|pattern|
       :|rValue| 
       :|optEjectorExpr|))

(def-scope-rule |EscapeExpr|
  (hide (seq (hide (seq :|ejectorPattern| :|body|))
             :|optCatchPattern|
             :|optCatchBody|)))

(def-scope-rule |FinallyExpr|
  (hide (seq (hide :|attempt|)
             :|unwinder|)))
      
(def-scope-rule |HideExpr|
  (hide :|block|))

(def-scope-rule |IfExpr|
  (seq (hide (seq :|test| :|then|)) (hide :|else|)))

(def-scope-rule |LiteralExpr|
  nil)

(def-scope-rule |MatchBindExpr|
  (seq :|specimen| :|pattern|))

(def-scope-rule |MetaContextExpr|
  nil)

(def-scope-rule |MetaStateExpr|
  (! (e. builder |scopeMeta|)))

(def-scope-rule |NounExpr|
  (! (e. builder |scopeRead| node)))

(def-scope-rule |ObjectExpr|
  (hide (seq (flatten :|auditorExprs|) :|script|)))

(def-scope-rule |SeqExpr|
  (flatten :|subs|))

(def-scope-rule |SlotExpr|
  (! (e. builder |scopeRead| (:get :|noun|))))


(def-scope-rule |CdrPattern|
  (seq :|listPatt| :|restPatt|))

(def-scope-rule |FinalPattern|
  (seq :|optGuardExpr|
       (! (e. builder |scopeDef| node))))

(def-scope-rule |IgnorePattern|
  nil)

(def-scope-rule |ListPattern|
  (flatten :|subs|))

(def-scope-rule |SlotPattern|
  (seq :|optGuardExpr|
       (! (e. builder |scopeSlot| node))))

(def-scope-rule |SuchThatPattern|
  (seq :|pattern| :|test|))

(def-scope-rule |VarPattern|
  (seq :|optGuardExpr|
       (! (e. builder |scopeVar| node))))

(def-scope-rule |EMatcher|
  (hide (seq :|pattern|
             :|body|)))

(def-scope-rule |EMethod|
  (hide (seq (flatten :|patterns|)
             :|optResultGuard|
             :|body|)))

(def-scope-rule |EScript|
  (seq (flatten :|optMethods|)
       (flatten :|matchers|)))

;;; --- Kernel-E checking ---

;; utility, to be moved
(defun to-condition (defaulted-type datum &rest arguments)
  "Implements CLHS 9.1.2.1."
  (etypecase datum
    (symbol
      (apply #'make-condition datum arguments))
    ((or string function)
      (make-condition defaulted-type :format-control datum :format-arguments arguments))
    (condition
      datum)))
(defun ejerror (ejector &rest args)
  (eject-or-ethrow ejector (apply #'to-condition 'simple-error args)))

(defun unmagical-typep (value type)
  "awful hack: E-LIST type can't work if expanded into a compiled file due to using (satisfies #:uninterned-symbol) so we ensure that TYPEP isn't optimized by indirecting through a redefinable function"
  (typep value type))

(defmacro define-kernel-e-type-constraints (node-class &rest rules)
  `(defmethod require-kernel-e-recursive progn ((node ,node-class) ejector)
    ,@(loop for (property ptype) in rules collect
      `(let ((value (funcall (opt-node-property-getter node ',property))))
         (unless (unmagical-typep value ',ptype)
           (ejerror ejector "~S ~S must have ~S of type ~S, but had ~S" 
                            ',node-class node ',property ',ptype value))))))

(defun require-kernel-e (node ejector)
  "Verify that the node is a valid Kernel-E tree."
  (reduce-scopewise node (make-scope-checker ejector))
  (require-kernel-e-recursive node ejector)
  (values))

(defun make-scope-checker (ejector) 
  (labels ((make (finals assigns)
             ;; XXX use hash tables?
             (e-lambda |kernelScopeCheckerValue| ()
               (:|_getFinals| () finals)
               (:|_getAssigns| () assigns)
               (:|hide| () (make nil nil))
               (:|add| (other &aux (bad (intersection finals (e. other |_getAssigns|) :test #'string=)))
                 (if bad
                   ;; XXX message could use some improvement
                   (ejerror ejector "~A is not an assignable variable" (first bad))
                   (make (append finals (e. other |_getFinals|))
                         (append assigns (e. other |_getAssigns|))))))))
    (e-lambda "$kernelScopeChecker" ()
      (:|scopeAssign| (node)
        (make nil (list (e. node |getName|))))
      (:|scopeDef| (node)
        (make (list (e. (e. node |getNoun|) |getName|)) nil))
      (:|scopeRead| (node)
        (declare (ignore node))
        (make nil nil))
      (:|scopeVar| (node)
        (declare (ignore node))
        (make nil nil))
      (:|scopeSlot| (node)
        (declare (ignore node))
        (make nil nil))
      (:|scopeMeta| () (make nil nil))
      (:|getEmptyScope| () (make nil nil)))))

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

(defmethod require-kernel-e-recursive progn ((node |DefineExpr|) ejector
    &aux (pattern (e. node |getPattern|))
         (r-value (e. node |getRValue|))
         (opt-ejector-expr (e. node |getOptEjectorExpr|)))
  (labels ((reject (name-d node-d name-u node-u)
             (when (and node-d node-u (usesp node-d node-u))
               (ejerror ejector "kernel define expr may not use definitions ~
                                 from ~A (~A) in ~A (~A)" 
                                 name-d (e-quote node-d) 
                                 name-u (e-quote node-u)))))
    (reject "pattern" pattern               "r-value expr" r-value)
    (reject "pattern" pattern               "ejector expr" opt-ejector-expr)
    (reject "r-value expr" r-value          "pattern" pattern)
    (reject "ejector expr" opt-ejector-expr "pattern" pattern)))

(defmethod require-kernel-e-recursive progn ((node |DefineExpr|) ejector
    &aux (pattern (funcall (opt-node-property-getter node :|pattern|)))
         (r-value (funcall (opt-node-property-getter node :|rValue|)))
         (opt-ejector-expr (funcall (opt-node-property-getter node :|optEjectorExpr|))))
  (when (usesp r-value pattern)
    (ejerror ejector "kernel define expr may not use definitions from r-value expr (~A) in pattern (~A)" (e-quote r-value) (e-quote pattern)))
  (when (and opt-ejector-expr
             (usesp opt-ejector-expr pattern))
    (ejerror ejector "kernel define expr may not use definitions from ejector expr (~A) in pattern (~A)" (e-quote opt-ejector-expr) (e-quote pattern))))

(define-kernel-e-type-constraints |AssignExpr|
  (:|noun| |NounExpr|))

(define-kernel-e-type-constraints |NounPattern|
  (:|noun| |NounExpr|))

(define-kernel-e-type-constraints |ObjectExpr|
  (:|script| |EScript|))

(define-kernel-e-type-constraints |EScript|
  (:|optMethods| (or null (e-list |EMethod|)))
  (:|matchers| (e-list |EMatcher|)))
