; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

; --- nodes ---

(defgeneric node-elements (node))
(defgeneric node-static-scope (node))

#+sbcl (sb-ext:unlock-package :e.elang.vm-node)

(defclass |ENode| () ((elements :initarg :elements :accessor node-elements)))

(defclass |EExpr|   (|ENode|) ())
(defclass |Pattern| (|ENode|) ())

(defclass |AssignExpr|      (|EExpr|) ())
(defclass |CallExpr|        (|EExpr|) ())
(defclass |CatchExpr|       (|EExpr|) ())
(defclass |DefineExpr|      (|EExpr|) ())
(defclass |EscapeExpr|      (|EExpr|) ())
(defclass |FinallyExpr|     (|EExpr|) ())
(defclass |HideExpr|        (|EExpr|) ())
(defclass |IfExpr|          (|EExpr|) ())
(defclass |LiteralExpr|     (|EExpr|) ())
(defclass |MetaContextExpr| (|EExpr|) ())
(defclass |MetaStateExpr|   (|EExpr|) ())
(defclass |MatchBindExpr|   (|EExpr|) ())
(defclass |NounExpr|        (|EExpr|) ())
(defclass |ObjectExpr|      (|EExpr|) ())
(defclass |SeqExpr|         (|EExpr|) ())
(defclass |SlotExpr|        (|EExpr|) ())

(defclass |EMethod|         (|ENode|) ())
(defclass |EMatcher|        (|ENode|) ())
(defclass |EScript|         (|ENode|) ())
                               
(defclass |CdrPattern|      (|Pattern|) ())
(defclass |IgnorePattern|   (|Pattern|) ())
(defclass |ListPattern|     (|Pattern|) ())
(defclass |SuchThatPattern| (|Pattern|) ())
(defclass |NounPattern|     (|Pattern|) ())

(defclass |FinalPattern|    (|NounPattern|) ())
(defclass |SlotPattern|     (|NounPattern|) ())
(defclass |VarPattern|      (|NounPattern|) ())

(defclass |QuasiNode| (|ENode|) ())

(defclass |QuasiLiteralNode| (|QuasiNode|) ())
(defclass |QuasiPatternNode| (|QuasiNode|) ())

(defclass |QuasiLiteralExpr| (|QuasiLiteralNode| |EExpr|) ())
(defclass |QuasiPatternExpr| (|QuasiPatternNode| |EExpr|) ())
(defclass |QuasiLiteralPatt| (|QuasiLiteralNode| |Pattern|) ())
(defclass |QuasiPatternPatt| (|QuasiPatternNode| |Pattern|) ())

#+sbcl (sb-ext:lock-package :e.elang.vm-node)

(do-symbols (node-type (find-package :e.elang.vm-node))
  (let ((fqn (concatenate 'string "org.erights.e.elang.evm." (symbol-name node-type))))
    (defmethod elib:cl-type-fq-name ((type (eql node-type)))
      fqn)))

(defmethod print-object ((node |ENode|) stream)
  (print-unreadable-object (node stream :type nil :identity nil)
    (format stream "~A ~W" (type-of node) (node-elements node))))

; --- makers ---

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

(def-node-maker |AssignExpr|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun |EExpr|) (rvalue |EExpr|)))

(def-node-maker |CallExpr|
  (t nil t)
  ; XXX parameterized list guard is not available at this layer: consider fixing.
  ;     -- and if it was, we'd need a way to incorporate the reference to it in the type specifier def-node-maker provides
  ;     perhaps we should use (vector EExpr)? cl-type-guard would need to be smart enough to coerce elements, and we'd want it to resemble E-level List[EExpr]
  ((recipient |EExpr|) (verb string) (args vector))
  (list* recipient verb (loop for arg across args collect (e-coerce arg '|EExpr|))))

(def-node-maker |CatchExpr|
  (t t t)
  ((attempt |EExpr|) (catch-pattern |Pattern|) (catcher |EExpr|)))

(def-node-maker |DefineExpr|
  (t t)
  ((pattern |Pattern|) (rvalue |EExpr|)))

(def-node-maker |FinalPattern|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun |EExpr|) (opt-guard (or null |EExpr|))))

(def-node-maker |FinallyExpr|
  (t t)
  ((attempt |EExpr|) (unwinder |EExpr|)))
  
(def-node-maker |HideExpr|
  (t)
  ((body |EExpr|)))
  
(def-node-maker |IfExpr|
  (t t t)
  ((condition |EExpr|) (true-block |EExpr|) (false-block |EExpr|)))

(def-node-maker |ListPattern|
  (t)
  ((patterns t))
  (loop for pattern in patterns collect (e-coerce pattern '|Pattern|)))

(def-node-maker |LiteralExpr|
  (nil)
  ; XXX require value be DeepPassByCopy? be string/int/char/float64?
  ((value t)))

(def-node-maker |MatchBindExpr|
  (t t)
  ((specimen |EExpr|) (pattern |Pattern|)))

(def-node-maker |MetaContextExpr|
  ()
  ())

(def-node-maker |MetaStateExpr|
  ()
  ())

(def-node-maker |NounExpr|
  (nil)
  ((name string)))

(def-node-maker |SeqExpr|
  (t)
  ((subs t))
  (progn
    (unless (> (length subs) 0)
      (error "SeqExpr must have at least one subexpression"))
    (loop for sub across subs collect (e-coerce sub '|EExpr|))))

(def-node-maker |SlotExpr|
  (nil)
  ((noun |NounExpr|)))

(def-node-maker |EScript|
  (t t)
  ((methods (or null vector)) (matcher (or null |EMatcher|)))
  (progn
    (unless (or methods matcher)
      (error "EScript must have methods or matcher"))
    (list (and methods (map 'vector (lambda (sub) (e-coerce sub '|EMethod|)) methods))
          matcher)))
        
(def-node-maker |ObjectExpr|
  (nil nil t t)
  ((doc-comment string) (qualified-name string) (auditor-exprs vector) (script |EScript|))
  (list doc-comment qualified-name (map 'vector (lambda (sub) (e-coerce sub '|EExpr|)) auditor-exprs) script))

(def-node-maker |EMethod|
  (nil nil t t t)
  ; XXX constrain patterns to vector of Pattern (write tests)
  ((doc-comment string) (verb string) (patterns vector) (opt-result-guard (or null |EExpr|)) (body |EExpr|))
  (list doc-comment verb patterns opt-result-guard body))

(def-node-maker |EMatcher|
  (t t)
  ((pattern |Pattern|) (body |EExpr|))
  (list pattern body))

(def-node-maker |EscapeExpr|
  (t t t t)
  ((ejector-pattern |Pattern|) (body |EExpr|) (opt-catch-pattern (or null |Pattern|)) (opt-catcher (or null |EExpr|))))

(def-node-maker |CdrPattern|
  (t t)
  ((list-patt |ListPattern|) (rest-patt |Pattern|)))

(def-node-maker |ListPattern|
  (t)
  ((subs t))
  (progn
    (loop for sub across subs collect (e-coerce sub '|Pattern|))))

(def-node-maker |VarPattern|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun |EExpr|) (opt-guard (or null |EExpr|))))

(def-node-maker |SlotPattern|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun |EExpr|) (opt-guard (or null |EExpr|))))

(def-node-maker |SuchThatPattern|
  (t t)
  ((subpattern |Pattern|) (condition |EExpr|)))
  
(def-node-maker |IgnorePattern| () ())

(def-node-maker |QuasiLiteralNode| (nil) ((index integer)))
(def-node-maker |QuasiPatternNode| (nil) ((index integer)))
(def-node-maker |QuasiLiteralExpr| (nil) ((index integer)))
(def-node-maker |QuasiPatternExpr| (nil) ((index integer)))
(def-node-maker |QuasiLiteralPatt| (nil) ((index integer)))
(def-node-maker |QuasiPatternPatt| (nil) ((index integer)))

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
(def-indexed-node-properties |DefineExpr| ("pattern" "rValue"))
(def-indexed-node-properties |EscapeExpr| ("exitPattern" "body" "optArgPattern" "optCatcher"))
(def-indexed-node-properties |FinallyExpr| ("attempt" "unwinder"))
(def-indexed-node-properties |HideExpr| ("block"))
(def-indexed-node-properties |IfExpr| ("test" "then" "else"))
(def-indexed-node-properties |MatchBindExpr| ("specimen" "pattern"))
(def-indexed-node-properties |NounExpr| ("name"))
(def-indexed-node-properties |ObjectExpr| ("docComment" "qualifiedName" "auditorExprs" "script"))
(def-indexed-node-properties |SeqExpr| (&rest "subs"))
(def-indexed-node-properties |SlotExpr| ("noun"))

(def-indexed-node-properties |EScript| ("optMethods" "optMatcher"))
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
  (defvar +the-make-static-scope+ (e-lambda "org.erights.e.evm.makeStaticScope" ()
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
  (seq :|pattern| :|rValue|))

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
       (! (if (:get :|optMatcher|)
            (e. (:get :|optMatcher|) |staticScope|)
            (e. +the-make-static-scope+ |getEmptyScope|)))))

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
