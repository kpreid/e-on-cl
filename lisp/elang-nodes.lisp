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

;; XXX this should go elsewhere
(define-modify-macro nreverse-here ()
  nreverse)

(defgeneric node-elements (node))
(defgeneric node-static-scope (node))

(defmacro def-node-maker (class-sym subnode-flags args &optional elements-form
    &aux (span-sym (gensym "SPAN"))
         (jlayout-sym (gensym "SCOPE-JLAYOUT"))
         (bare-args (mapcar #'car args)))
  `(setf 
    (get ',class-sym 'static-maker)
    ; XXX don't ignore span-sym
    (e-lambda 
        ,(concatenate 'string "org.erights.e.elang.evm.make"
                              (symbol-name class-sym))
        ()
      (:|asType| () 
        (make-instance 'cl-type-guard :type-specifier ',class-sym))
      (:|getParameterSubnodeFlags| ()
        "Return a list of booleans which indicate whether the corresponding parameter (as used in makers and visitors) is a 'subnode' of the node (e.g. HideExpr's sole parameter) or not (e.g. LiteralExpr's sole parameter). XXX clarify"
        ; XXX make this constant - note that e-booleans currently aren't compilable
        (map 'vector #'as-e-boolean ',subnode-flags))
      (,(locally
          (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
          (e-util:mangle-verb "run" (+ 2 (length args))))
        (,span-sym ,@bare-args ,jlayout-sym)
        (declare (ignore ,span-sym))
        (assert (null ,jlayout-sym))
        ,@(loop for (arg type) in args
                collect `(e-coercef ,arg ',type))
        (make-instance ',class-sym :elements ,(or elements-form `(list ,@bare-args)))))))

(defun %setup-node-class (node-type)
  (let ((fqn (concatenate 'string "org.erights.e.elang.evm." (symbol-name node-type))))
    (defmethod elib:cl-type-fq-name ((type (eql node-type)))
      fqn)))

(defmacro define-node-class (class-name (&rest superclasses) (&rest property-defs) &key no-dnm)
  (let (subnode-flags dnm-args)
    (loop for pd in property-defs do
      (destructuring-bind (property-name subnode-flag type &key) pd
        (push subnode-flag subnode-flags)
        (push `(,(intern (string property-name) :e.elang) ,type) dnm-args)))
    (nreverse-here subnode-flags)
    (nreverse-here dnm-args)
    `(progn
      (defclass ,class-name ,superclasses ())
      (%setup-node-class ',class-name)
      ,(unless no-dnm
        `(def-node-maker ,class-name
          ,subnode-flags
          ,dnm-args)))))

;;; --- Node definitions ---

#+sbcl (sb-ext:unlock-package :e.elang.vm-node)

(defclass |ENode| () ((elements :initarg :elements :accessor node-elements)))

(define-node-class |EExpr|   (|ENode|)
  ()
  :no-dnm t)
(define-node-class |Pattern| (|ENode|)
  ()
  :no-dnm t)

(define-node-class |AssignExpr|      (|EExpr|)
  ((:|noun|   t (or |NounExpr| |QuasiExpr|))
   (:|rValue| t |EExpr|)))
(define-node-class |CallExpr|        (|EExpr|)
  ((:|target| t |EExpr|)
   (:|verb| nil string) 
   (:|args| t (e-list |EExpr|)))
  ;; :rest-slot t
  :no-dnm t)
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
  ((:|body| t |EExpr|)))
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
  ;; :rest-slot t
  :no-dnm t)
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
  :no-dnm t)
(define-node-class |SuchThatPattern| (|Pattern|)
  ((:|pattern| t |Pattern|) 
   (:|test| t |EExpr|)))

(define-node-class |NounPattern|     (|Pattern|)
  ()
  :no-dnm t)

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
  ()
  :no-dnm t)
(define-node-class |QuasiPatt| (|Pattern| |QuasiNode|)
  ()
  :no-dnm t)

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

; --- makers ---

;;; XXX to be moved into define-node-class and shared-initialize :after

(def-node-maker |CallExpr|
  (t nil t)
  ; XXX parameterized list guard is not available at this layer: consider fixing.
  ;     -- and if it was, we'd need a way to incorporate the reference to it in the type specifier def-node-maker provides
  ;     perhaps we should use (vector EExpr)? cl-type-guard would need to be smart enough to coerce elements, and we'd want it to resemble E-level List[EExpr]
  ((recipient |EExpr|) (verb string) (args vector))
  (list* recipient verb (loop for arg across args collect (e-coerce arg '|EExpr|))))

(def-node-maker |SeqExpr|
  (t)
  ((subs t))
  (progn
    (unless (> (length subs) 0)
      (error "SeqExpr must have at least one subexpression"))
    (loop for sub across subs collect (e-coerce sub '|EExpr|))))

(def-node-maker |EScript|
  (t t)
  ((methods (or null vector)) (matchers vector))
  (progn
    (unless (or methods (> (length matchers) 0))
      (error "EScript must have methods or at least one matcher"))
    (list (and methods (map 'vector (lambda (sub) (e-coerce sub '|EMethod|)) methods))
          (map 'vector (lambda (sub) (e-coerce sub '|EMatcher|)) matchers))))

(def-node-maker |ListPattern|
  (t)
  ((subs t))
  (progn
    (loop for sub across subs collect (e-coerce sub '|Pattern|))))

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

(defmethod shared-initialize :after ((node |DefineExpr|) slot-names &key &allow-other-keys
    &aux (pattern (funcall (opt-node-property-getter node :|pattern|)))
         (r-value (funcall (opt-node-property-getter node :|pattern|)))
         (opt-ejector-expr (funcall (opt-node-property-getter node :|optEjectorExpr|))))
  (when (usesp r-value pattern)
    (error "define expr may not use definitions from r-value expr (~A) in pattern (~A)" (e-quote r-value) (e-quote pattern)))
  (when (and opt-ejector-expr
             (usesp opt-ejector-expr pattern))
    (error "define expr may not use definitions from ejector expr (~A) in pattern (~A)" (e-quote opt-ejector-expr) (e-quote pattern))))

; --- E-level methods, and printing ---

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
    `#(,(let ((sm (get (observable-type-of this) 'static-maker)))
          (assert sm () "Missing maker for E-node class ~A" (observable-type-of this))
          sm)
       "run"
       #(nil ,@(node-visitor-arguments this) nil)))
  (:|asText| (this)
    (e-print this))
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "e??")
  (:|staticScope| (this)
    "Return a static scope analysis of this subtree that doesn't depend on the enclosing context."
    ; xxx in Java-E this caches the result (doing so will require StaticScope to be Selfless)
    (node-static-scope this))
  (:|substitute| (this args)
    "Quasiliteral ValueMaker interface"
    (e-coercef args 'vector)
    ; XXX the test is a non-transparent optimization: if a tree has quasi-fields anyway, we'll return them instead of failing now as index-out-of-range
    ; XXX vat-locally cache makeQuasiSubstituteVisitor until we have it properly deep-frozen so <import> can do so
    (if (/= (length args) 0)
      (e. this |welcome|
          (e. (e. (e. (vat-safe-scope *vat*) |get|
                      "import__uriGetter") 
                  |get|
                  "org.erights.e.elang.visitors.makeQuasiSubstituteVisitor") 
              |run| args))
      this))
  (:|welcome| (this visitor)
    (e-call
      visitor
      ; xxx should we use our alleged type method instead of observable-type-of?
      (concatenate 'string "visit" (symbol-name (observable-type-of this))) 
      (cons this (node-visitor-arguments this))))
  (:|lnPrintOn| (this tw priority)
    "Java-E compatibility"
    (declare (ignore priority)) ; XXX
    (e. tw |println|)
    (e. this |welcome|
      (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw)))
  (:|subPrintOn| (this tw priority)
    "Java-E compatibility"
    (declare (ignore priority)) ; XXX
    (e. this |welcome|
      (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw))))

(defgeneric opt-node-property-getter (node field-keyword))  ;XXX this should be elsewhere

(defmacro def-indexed-node-properties (class-name names)
  (labels ((method (name body)
            `(defmethod opt-node-property-getter 
                ((node ,class-name) 
                 (field-keyword (eql ',(intern (string name) "KEYWORD"))))
              (declare (ignore field-keyword))
              (lambda () ,body)))
           (generate (i list)
            (cond
              ((null list) 
                nil)
              ((eql (first list) '&rest)
                (assert (= (length list) 2))
                (list (method (second list) `(coerce (nthcdr ',i (node-elements node)) 'vector))))
              (t
                (destructuring-bind (name &rest tail) list
                  (cons
                    (method name `(nth ',i (node-elements node)))
                    (generate (1+ i) tail)))))))
  `(progn
    ,@(generate 0 names)
    (defmethod opt-node-property-getter 
        ((node ,class-name) 
         (field-keyword t))
      (declare (ignore field-keyword))
      nil))))

(defmethod e-call-match ((rec |ENode|) mverb &rest args
    &aux (mverb-string (symbol-name mverb)) opt-getter)
  (declare (ignore args))
  (if (and (keywordp mverb)
           (>= (length mverb-string) 5)
           (string= mverb-string "get" :end1 3)
           (string= mverb-string "/0" :start1 (- (length mverb-string) 2))
           (setf opt-getter (opt-node-property-getter 
                              rec
                              (intern (concatenate 'string
                                        (string (char-downcase (char mverb-string 3)))
                                        (subseq mverb-string 4 (- (length mverb-string) 2)))
                                      "KEYWORD"))))
    (funcall opt-getter)
    (call-next-method)))

; XXX put these somewhere else
(def-indexed-node-properties |AssignExpr| ("noun" "rValue"))
(def-indexed-node-properties |CallExpr| ("recipient" "verb" &rest "args"))
(def-indexed-node-properties |CatchExpr| ("attempt" "pattern" "catcher"))
(def-indexed-node-properties |DefineExpr| ("pattern" "rValue" "optEjectorExpr"))
(def-indexed-node-properties |EscapeExpr| ("exitPattern" "body" "optArgPattern" "optCatcher"))
(def-indexed-node-properties |FinallyExpr| ("attempt" "unwinder"))
(def-indexed-node-properties |HideExpr| ("block"))
(def-indexed-node-properties |IfExpr| ("test" "then" "else"))
(def-indexed-node-properties |MatchBindExpr| ("specimen" "pattern"))
(def-indexed-node-properties |NounExpr| ("name"))
(def-indexed-node-properties |ObjectExpr| ("docComment" "qualifiedName" "auditorExprs" "script"))
(def-indexed-node-properties |SeqExpr| (&rest "subs"))
(def-indexed-node-properties |SlotExpr| ("noun"))

(def-indexed-node-properties |EScript| ("optMethods" "matchers"))
(def-indexed-node-properties |EMatcher| ("pattern" "body"))
(def-indexed-node-properties |EMethod| ("docComment" "verb" "patterns" "optResultGuard" "body"))

(def-indexed-node-properties |NounPattern| ("noun" "optGuardExpr"))
(def-indexed-node-properties |CdrPattern| ("listPatt" "restPatt"))
(def-indexed-node-properties |ListPattern| (&rest "subs"))
(def-indexed-node-properties |SuchThatPattern| ("pattern" "test"))

(def-vtable |EExpr|
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "e")
  (:|eval/1| 'eval-e)
  (:|evalToPair| (this scope)
    "Evaluate this expression in the given outer scope and return a tuple of the return value and a scope containing any new bindings."
    (multiple-value-call #'vector (eval-e this scope))))

(def-vtable |AssignExpr|)

(def-vtable |CallExpr|)

(def-vtable |DefineExpr|)

(def-vtable |EscapeExpr|)

(def-vtable |LiteralExpr|)

(def-vtable |NounExpr|
  (:|name| (this)
    (nth 0 (node-elements this))))

(def-vtable |ObjectExpr|)

(def-vtable |SeqExpr|)

(def-vtable |SlotExpr|)

(def-vtable |EMethod|)

(def-vtable |EMatcher|)
    
(def-vtable |EScript|
  (:|getDocComment| (this)
    (nth 0 (node-elements this))))
    

(def-vtable |Pattern|
  (:|quasiTypeTag| (this)
    (declare (ignore this))
    "epatt"))

(def-vtable |FinalPattern|)

(def-vtable |VarPattern|)

(def-vtable |IgnorePattern|)

(def-vtable |SlotPattern|)

(def-vtable |SuchThatPattern|)
      
; --- analysis ---

; XXX merge this with staticScope? not user-visible?
; XXX this isn't actually used
(defgeneric needs-reified-slot (node name))

(defmethod needs-reified-slot ((node |ENode|) name)
  (some #'(lambda (node) (needs-reified-slot node name)) (node-elements node)))

(defmethod needs-reified-slot ((node |SlotExpr|) name)
  (string= name (first (node-elements node))))

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
    ; XXX make this a formal NotAGuard
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
    (e-lambda "org.erights.e.elang.evm.StaticScope" ()
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

(flet ((make (node kind label)
        ;; muffle non-constant keyword argument warning
        (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
        (make-static-scope kind
          (e. +the-make-const-map+ |fromPairs|
            `#(#(,label ,node))))))
  (defglobal +the-make-static-scope+ (e-lambda "org.erights.e.evm.makeStaticScope" ()
    (:|scopeAssign| (node)
      (e-coercef node '|NounExpr|)
      (make node :set-names (e. node |name|)))
    (:|scopeDef| (node)
      (e-coercef node '|FinalPattern|)
      (make node :def-names (e. (e. node |getNoun|) |name|)))
    (:|scopeRead| (node)
      (e-coercef node '(or |NounExpr| |SlotExpr|))
      (make node :read-names (e. node |name|)))
    (:|scopeVar| (node)
      (e-coercef node '|VarPattern|)
      (make node :var-names (e. (e. node |getNoun|) |name|)))
    (:|scopeSlot| (node)
      (e-coercef node '|SlotPattern|)
      (make node :var-names (e. (e. node |getNoun|) |name|)))
    (:|scopeMeta| ()
      (make-static-scope :has-meta-state-expr +e-true+))
    (:|getEmptyScope| ()
      (make-static-scope)))))

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
    `(defmethod node-static-scope ((node ,specializer))
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
  (hide (seq (hide (seq :|exitPattern| :|body|))
             (! (if (:get :|optArgPattern|)
                  (e. (e. (:get :|optArgPattern|) |staticScope|) 
                      |add|
                      (e. (:get :|optCatcher|) |staticScope|))
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

; XXX 'node' not explicitly bound
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

; XXX hidden binding
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

; --- visitors ---

(defgeneric node-visitor-arguments (node))

(defmethod node-visitor-arguments ((node |ENode|))
  (node-elements node))

(defmethod node-visitor-arguments ((node |CallExpr|))
  (destructuring-bind (rec verb &rest args) (node-elements node)
    `(,rec ,verb ,(coerce args 'vector))))

(defmethod node-visitor-arguments ((node |SeqExpr|))
  `(,(coerce (node-elements node) 'vector)))

(defmethod node-visitor-arguments ((node |ListPattern|))
  `(,(coerce (node-elements node) 'vector)))

