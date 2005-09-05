; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

; --- base ---

;; XXX this should go elsewhere
(deftype e-list (element-type &aux (sym (gensym (princ-to-string element-type))))
  (setf (symbol-function sym) 
        (lambda (specimen)
          (every (lambda (element) (typep element element-type)) specimen)))
  `(and vector (satisfies ,sym)))

(defgeneric node-elements (node))
(defgeneric compute-node-static-scope (node))
(defgeneric opt-node-property-getter (node field-keyword))
(defgeneric node-visitor-arguments (node))

;; XXX this macro used to be used on its own but is now only used by define-node-class - review and cleanup
(defmacro %def-node-maker (class-sym subnode-flags param-types rest-p
    &aux (span-sym (gensym "SPAN"))
         (jlayout-sym (gensym "SCOPE-JLAYOUT"))
         (param-syms (loop repeat (length param-types) collect (gensym))))
  `(setf 
    (get ',class-sym 'static-maker)
    ; XXX don't ignore span-sym
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
        (declare (ignore ,span-sym))
        (assert (null ,jlayout-sym))
        ,@(loop for param in param-syms
                for type in param-types
                collect `(e-coercef ,param ',type))
        (make-instance ',class-sym :elements
          ,(if rest-p
            `(list* ,@(butlast param-syms) (coerce ,(car (last param-syms)) 'list))
            `(list ,@param-syms)))))))

(defun %setup-node-class (node-type)
  (let ((fqn (concatenate 'string "org.erights.e.elang.evm." (symbol-name node-type))))
    (defmethod elib:cl-type-fq-name ((type (eql node-type)))
      fqn)))

(defmacro define-node-class (class-name (&rest superclasses) (&rest property-defs) &key rest-slot)
  (let ((normal-prop-count (- (length property-defs) (if rest-slot 1 0)))
        subnode-flags 
        dnm-types)
    (loop for pd in property-defs do
      (destructuring-bind (property-name subnode-flag type &key) pd
        (declare (ignore property-name))
        (push subnode-flag subnode-flags)
        (push type dnm-types)))
    (nreverse-here subnode-flags)
    (nreverse-here dnm-types)
    `(progn
      (defclass ,class-name ,superclasses ())
      
      (%setup-node-class ',class-name)
      
      (%def-node-maker ,class-name
        ,subnode-flags
        ,dnm-types
        ,rest-slot)
      
      ,@(loop for (property-name) in property-defs
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
  ((elements :initarg :elements :accessor node-elements)
   (static-scope :initform nil :type function)))

(define-node-class |EExpr|   (|ENode|)
  ())
(define-node-class |Pattern| (|ENode|)
  ())

(define-node-class |AssignExpr|      (|EExpr|)
  ((:|noun|   t (or |NounExpr| |QuasiExpr|))
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
   (:|script| t |EScript|)))
(define-node-class |SeqExpr|         (|EExpr|)
  ((:|subs| t (e-list |EExpr|)))
  :rest-slot t)
(define-node-class |SlotExpr|        (|EExpr|)
  ((:|noun| t (or |NounExpr| |QuasiExpr|))))

(define-node-class |EMethod|         (|ENode|)
  ((:|docComment| nil string)
   (:|verb| nil string)
   (:|patterns| t (e-list |Pattern|))
   (:|optResultGuard| t (or null |EExpr|))
   (:|body| t |EExpr|)))
(define-node-class |EMatcher|        (|ENode|)
  ((:|pattern| t |Pattern|) 
   (:|body| t |EExpr|)))
(define-node-class |EScript|         (|ENode|)
  ((:|optMethods| t (or null (e-list |EMethod|)))
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
  ((:|noun| t (or |QuasiExpr| |NounExpr|)) 
   (:|optGuardExpr| t (or null |EExpr|))))
(define-node-class |SlotPattern|     (|NounPattern|)
  ((:|noun| t (or |QuasiExpr| |NounExpr|)) 
   (:|optGuardExpr| t (or null |EExpr|))))
(define-node-class |VarPattern|      (|NounPattern|)
  ((:|noun| t (or |QuasiExpr| |NounExpr|)) 
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
  (print-unreadable-object (node stream :type nil :identity nil)
    (format stream "~A ~W" (type-of node) (node-elements node))))

; --- constraints on nodes ---

(defun usesp (defining using)
  "Return whether 'using' uses nouns defined by 'defining'."
  (> (e. (e. (e. (e. defining |staticScope|) |outNames|)
             |and|
             (e. (e. using |staticScope|) |namesUsed|))
         |size|)
     0))

(defmethod change-class ((node |ENode|) new-class &key &allow-other-keys)
  (error "change-class is not a good idea for ENodes"))

;; (defmethod shared-initialize :before ((node |QuasiNode|) slot-names &key &allow-other-keys)
;;   (unless (typep node '(or |QuasiLiteralExpr| |QuasiLiteralPatt|
;;                            |QuasiPatternExpr| |QuasiPatternPatt|))
;;     (error "Attempt to construct a quasi-node of unspecific class ~S." (class-of node))))

(defmethod shared-initialize :after ((node |SeqExpr|) slot-names &key &allow-other-keys)
  (unless (> (length (funcall (opt-node-property-getter node :|subs|))) 0)
    (error "SeqExpr must have at least one subexpression")))

(defmethod shared-initialize :after ((node |EScript|) slot-names &key &allow-other-keys
    &aux (methods (funcall (opt-node-property-getter node :|optMethods|)))
         (matchers (funcall (opt-node-property-getter node :|matchers|))))
  (unless (or methods (> (length matchers) 0))
      (error "EScript must have methods or at least one matcher")))
    
(defmethod shared-initialize :after ((node |DefineExpr|) slot-names &key &allow-other-keys
    &aux (pattern (funcall (opt-node-property-getter node :|pattern|)))
         (r-value (funcall (opt-node-property-getter node :|pattern|)))
         (opt-ejector-expr (funcall (opt-node-property-getter node :|optEjectorExpr|))))
  (when (usesp r-value pattern)
    (error "define expr may not use definitions from r-value expr (~A) in pattern (~A)" (e-quote r-value) (e-quote pattern)))
  (when (and opt-ejector-expr
             (usesp opt-ejector-expr pattern))
    (error "define expr may not use definitions from ejector expr (~A) in pattern (~A)" (e-quote opt-ejector-expr) (e-quote pattern))))

; --- E-level methods ---

(defmethod eeq-is-transparent-selfless ((a |ENode|))
  (declare (ignore a))
  t)

(def-vtable |ENode|
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
  (:|substitute| (this args)
    "Quasiliteral ValueMaker interface.

NOTE: There is a non-transparent optimization, with the effect that if args == [] and there are quasi-nodes in this tree, they will be returned unreplaced."
    (e-coercef args 'vector)
    (if (= (length args) 0)
      this
      (e. this |welcome|
          (e. (e-import "org.erights.e.elang.visitors.makeQuasiSubstituteVisitor") 
              |run| args))))
  (:|welcome| (this visitor)
    (e-call
      visitor
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

(defun opt-guard-expr-to-safe-opt-guard (opt-guard-expr
    &aux (opt-guard-string
           (if opt-guard-expr 
             (if (typep (ref-shorten opt-guard-expr) '|NounExpr|)
               ;; optimization
               (e. opt-guard-expr |getName|)
               (e-print opt-guard-expr)))))
  (if opt-guard-string
    (e-lambda "org.erights.e.elang.FalseGuard"
        (:stamped +deep-frozen-stamp+
         :doc "This is a false Guard created by Miranda __getAllegedType() representing the name of the guard expression in this object, but not the actual guard since it would be sometimes a security problem and/or impossible due to the guard expression not being constant over the life of the object.")
      (:|__printOn| (tw) 
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |write| opt-guard-string)))))

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
      (warn "using inefficient makeStaticScope#run")
      (let ((map-guard (e-import "org.erights.e.elib.slot.Map"))
            (string-guard (type-specifier-to-guard 'string)))
        (e-coercef sn (e. map-guard |get| string-guard (type-specifier-to-guard '|NounExpr|)))
        (e-coercef rn (e. map-guard |get| string-guard (type-specifier-to-guard '(or |NounExpr| |SlotExpr|))))
        (e-coercef dn (e. map-guard |get| string-guard (type-specifier-to-guard '|FinalPattern|)))
        (e-coercef vn (e. map-guard |get| string-guard (type-specifier-to-guard '(or |VarPattern| |SlotPattern|)))))
      (e-coercef sn 'boolean)
      (make-static-scope :set-names sn
                         :read-names rn
                         :def-names dn
                         :var-names vn
                         :has-meta-state-expr hms))
    (:|scopeAssign| (node)
      (e-coercef node '|NounExpr|)
      (make node :set-names (e. node |getName|)))
    (:|scopeDef| (node)
      (e-coercef node '|FinalPattern|)
      (make node :def-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeRead| (node)
      (e-coercef node '(or |NounExpr| |SlotExpr|))
      (make node :read-names (e. node |getName|)))
    (:|scopeVar| (node)
      (e-coercef node '|VarPattern|)
      (make node :var-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeSlot| (node)
      (e-coercef node '|SlotPattern|)
      (make node :var-names (e. (e. node |getNoun|) |getName|)))
    (:|scopeMeta| ()     +has-meta-static-scope+)
    (:|getEmptyScope| () +empty-static-scope+))))

; --- static scope computation ---

(defmacro def-scope-rule (specializer scope-expr)
  "Forms in the scope rule language:
    nil    -- the empty scope
    :|foo| -- the static scope of the property foo of this node
    (seq <forms>) -- sequencing, as in StaticScope#add/1
    (hide <form>) -- hiding, as in StaticScope#hide/0
    (flatten :|foo|) -- the sequencing of the list property foo of this node
    (! <CL-form>) -- CL code to return a static scope
    
    Within a ! form, elang::node is bound to this node and (:get <keyword>) is bound to a property-fetching function."
  (labels ((transform (expr)
            (cond
              ((null expr)
                `(e. +the-make-static-scope+ |getEmptyScope|))
              ((atom expr)
                `(e. (:get ',expr) |staticScope|))
              ((eql (first expr) 'hide)
                (assert (= (length expr) 2))
                `(e. ,(transform (second expr)) |hide|))
              ((eql (first expr) 'seq)
                (reduce #'(lambda (a b) `(e. ,a |add| ,b)) 
                        (rest expr)
                        :key #'transform
                        :initial-value `(e. +the-make-static-scope+ |getEmptyScope|)))
              ((eql (first expr) '!)
                `(progn ,@(rest expr)))
              ((eql (first expr) 'flatten)
                (assert (= (length expr) 2))
                `(sum-node-scopes (:get ',(second expr))))
              (t
                (error "Unknown scope-rule expression: ~S" expr)))))
    `(defmethod compute-node-static-scope ((node ,specializer))
      (flet ((:get (keyword) (funcall (opt-node-property-getter node keyword))))
        (declare (ignorable (function :get)))
        ,(transform scope-expr)))))

(defun sum-node-scopes (nodes)
  (reduce #'(lambda (a b) (e. a |add| b))
          (map 'list #'(lambda (a) (e. a |staticScope|))
                     nodes)
          :initial-value (e. +the-make-static-scope+ |getEmptyScope|)))

(def-scope-rule |AssignExpr|
  (seq (! (e. +the-make-static-scope+ |scopeAssign| (:get :|noun|)))
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
       (! (let ((ex (:get :|optEjectorExpr|)))
            (if ex
              (e. ex |staticScope|)
              (e. +the-make-static-scope+ |getEmptyScope|))))))

(def-scope-rule |EscapeExpr|
  (hide (seq (hide (seq :|ejectorPattern| :|body|))
             (! (if (:get :|optCatchPattern|)
                  (e. (e. (:get :|optCatchPattern|) |staticScope|) 
                      |add|
                      (e. (:get :|optCatchBody|) |staticScope|))
                  (e. +the-make-static-scope+ |getEmptyScope|))))))

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
  (! (e. +the-make-static-scope+ |scopeMeta|)))

(def-scope-rule |NounExpr|
  (! (e. +the-make-static-scope+ |scopeRead| node)))

(def-scope-rule |ObjectExpr|
  (seq (flatten :|auditorExprs|) :|script|))

(def-scope-rule |SeqExpr|
  (flatten :|subs|))

(def-scope-rule |SlotExpr|
  (! (e. +the-make-static-scope+ |scopeRead| (:get :|noun|))))


(def-scope-rule |CdrPattern|
  (seq :|listPatt| :|restPatt|))

(def-scope-rule |FinalPattern|
  (! (e. +the-make-static-scope+ |scopeDef| node)))

(def-scope-rule |IgnorePattern|
  nil)

(def-scope-rule |ListPattern|
  (flatten :|subs|))

(def-scope-rule |SlotPattern|
  (! (e. +the-make-static-scope+ |scopeSlot| node)))

(def-scope-rule |SuchThatPattern|
  (seq :|pattern| :|test|))

(def-scope-rule |VarPattern|
  (! (e. +the-make-static-scope+ |scopeVar| node)))

(def-scope-rule |EMatcher|
  (hide (seq :|pattern|
             :|body|)))

(def-scope-rule |EMethod|
  (hide (seq (flatten :|patterns|)
             (! (if (:get :|optResultGuard|)
                  (e. (:get :|optResultGuard|) |staticScope|)
                  (e. +the-make-static-scope+ |getEmptyScope|)))
             :|body|)))

(def-scope-rule |EScript|
  (seq (! (if (:get :|optMethods|)
            (sum-node-scopes (:get :|optMethods|))
            (e. +the-make-static-scope+ |getEmptyScope|)))
       (! (sum-node-scopes (:get :|matchers|)))))
