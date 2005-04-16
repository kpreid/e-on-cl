; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

; --- nodes ---

(defgeneric node-elements (node))
(defgeneric node-static-scope (node))

#+sbcl (sb-ext:unlock-package :e.elang.vm-node)

(defclass evm-node::|ENode| () ((elements :initarg :elements :accessor node-elements)))

(defclass evm-node::|EExpr|   (evm-node::|ENode|) ())
(defclass evm-node::|Pattern| (evm-node::|ENode|) ())

; XXX ABCL workaround for bug where IN-PACKAGE gets inappropriately written unqualified to .abcl when :cl isn't USEd by the current package
#+abcl (shadow '(:null :error) :evm-node)
#+abcl (use-package :cl :evm-node)

(in-package :evm-node)          
  (cl:defclass |AssignExpr|      (|EExpr|) ())
  (cl:defclass |CallExpr|        (|EExpr|) ())
  (cl:defclass |CatchExpr|       (|EExpr|) ())
  (cl:defclass |DefineExpr|      (|EExpr|) ())
  (cl:defclass |EscapeExpr|      (|EExpr|) ())
  (cl:defclass |FinallyExpr|     (|EExpr|) ())
  (cl:defclass |HideExpr|        (|EExpr|) ())
  (cl:defclass |IfExpr|          (|EExpr|) ())
  (cl:defclass |LiteralExpr|     (|EExpr|) ())
  (cl:defclass |MetaContextExpr| (|EExpr|) ())
  (cl:defclass |MetaStateExpr|   (|EExpr|) ())
  (cl:defclass |MatchBindExpr|   (|EExpr|) ())
  (cl:defclass |NounExpr|        (|EExpr|) ())
  (cl:defclass |ObjectExpr|      (|EExpr|) ())
  (cl:defclass |SeqExpr|         (|EExpr|) ())
  (cl:defclass |SlotExpr|        (|EExpr|) ())
  
  (cl:defclass |EMethod|         (|ENode|) ())
  (cl:defclass |EMatcher|        (|ENode|) ())
  (cl:defclass |EScript|         (|ENode|) ())
                                 
  (cl:defclass |CdrPattern|      (|Pattern|) ())
  (cl:defclass |IgnorePattern|   (|Pattern|) ())
  (cl:defclass |ListPattern|     (|Pattern|) ())
  (cl:defclass |SuchThatPattern| (|Pattern|) ())
  (cl:defclass |NounPattern|     (|Pattern|) ())

  (cl:defclass |FinalPattern|    (|NounPattern|) ())
  (cl:defclass |SlotPattern|     (|NounPattern|) ())
  (cl:defclass |VarPattern|      (|NounPattern|) ())
  
  (cl:defclass |QuasiNode| (|ENode|) ())
  
  (cl:defclass |QuasiLiteralNode| (|QuasiNode|) ())
  (cl:defclass |QuasiPatternNode| (|QuasiNode|) ())
  
  (cl:defclass |QuasiLiteralExpr| (|QuasiLiteralNode| |EExpr|) ())
  (cl:defclass |QuasiPatternExpr| (|QuasiPatternNode| |EExpr|) ())
  (cl:defclass |QuasiLiteralPatt| (|QuasiLiteralNode| |Pattern|) ())
  (cl:defclass |QuasiPatternPatt| (|QuasiPatternNode| |Pattern|) ())
(cl:in-package :elang)

; XXX ABCL workaround
#+abcl (unuse-package :cl :evm-node)

#+sbcl (sb-ext:lock-package :e.elang.vm-node)

(do-symbols (node-type (find-package :evm-node))
  (let ((fqn (concatenate 'string "org.erights.e.elang.evm." (symbol-name node-type))))
    (defmethod elib:cl-type-fq-name ((type (eql node-type)))
      fqn)))

(defmethod print-object ((node evm-node::|ENode|) stream)
  (print-unreadable-object (node stream :type nil :identity nil)
    (format stream "~A ~W" (type-of node) (node-elements node))))

; --- makers ---

(defmacro def-node-maker (class-sym subnode-flags args &optional elements-form
    &aux (span-sym (gensym "SPAN"))
         (layout-sym (gensym "SCOPE-LAYOUT"))
         (bare-args (mapcar #'car args)))
  `(setf 
    (get ',class-sym 'static-maker)
    ; XXX don't ignore span-sym
    (e-named-lambda ,(concatenate 'string "org.erights.e.elang.evm.make" (symbol-name class-sym))
      (:|asType/0| () 
        (make-instance 'cl-type-guard :type-specifier ',class-sym))
      (:|getParameterSubnodeFlags/0| ()
        "Return a list of booleans which indicate whether the corresponding parameter (as used in makers and visitors) is a 'subnode' of the node (e.g. HideExpr's sole parameter) or not (e.g. LiteralExpr's sole parameter). XXX clarify"
        ; XXX make this constant - note that e-booleans currently aren't compilable
        (map 'vector #'as-e-boolean ',subnode-flags))
      (,(e-util:mangle-verb "run" (+ 2 (length args))) (,span-sym ,@bare-args ,layout-sym)
        (declare (ignore ,span-sym))
        (assert (null ,layout-sym))
        ,@(loop for (arg type) in args
                collect `(e-coercef ,arg ',type))
        (make-instance ',class-sym :elements ,(or elements-form `(list ,@bare-args)))))))

(def-node-maker evm-node::|AssignExpr|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun evm-node::|EExpr|) (rvalue evm-node::|EExpr|)))

(def-node-maker evm-node::|CallExpr|
  (t nil t)
  ; XXX parameterized list guard is not available at this layer: consider fixing.
  ;     -- and if it was, we'd need a way to incorporate the reference to it in the type specifier def-node-maker provides
  ;     perhaps we should use (vector EExpr)? cl-type-guard would need to be smart enough to coerce elements, and we'd want it to resemble E-level List[EExpr]
  ((recipient evm-node::|EExpr|) (verb string) (args vector))
  (list* recipient verb (loop for arg across args collect (e-coerce arg 'evm-node::|EExpr|))))

(def-node-maker evm-node:|CatchExpr|
  (t t t)
  ((attempt evm-node::|EExpr|) (catch-pattern evm-node::|Pattern|) (catcher evm-node::|EExpr|)))

(def-node-maker evm-node::|DefineExpr|
  (t t)
  ((pattern evm-node::|Pattern|) (rvalue evm-node::|EExpr|)))

(def-node-maker evm-node::|FinalPattern|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun evm-node::|EExpr|) (opt-guard (or null evm-node::|EExpr|))))

(def-node-maker evm-node:|FinallyExpr|
  (t t)
  ((attempt evm-node::|EExpr|) (unwinder evm-node::|EExpr|)))
  
(def-node-maker evm-node::|HideExpr|
  (t)
  ((body evm-node::|EExpr|)))
  
(def-node-maker evm-node:|IfExpr|
  (t t t)
  ((condition evm-node::|EExpr|) (true-block evm-node::|EExpr|) (false-block evm-node::|EExpr|)))

(def-node-maker evm-node::|ListPattern|
  (t)
  ((patterns t))
  (loop for pattern in patterns collect (e-coerce pattern 'evm-node::|Pattern|)))

(def-node-maker evm-node::|LiteralExpr|
  (nil)
  ; XXX require value be DeepPassByCopy? be string/int/char/float64?
  ((value t)))

(def-node-maker evm-node::|MatchBindExpr|
  (t t)
  ((specimen evm-node::|EExpr|) (pattern evm-node::|Pattern|)))

(def-node-maker evm-node:|MetaContextExpr|
  ()
  ())

(def-node-maker evm-node:|MetaStateExpr|
  ()
  ())

(def-node-maker evm-node::|NounExpr|
  (nil)
  ((name string)))

(def-node-maker evm-node::|SeqExpr|
  (t)
  ((subs t))
  (progn
    (unless (> (length subs) 0)
      (error "SeqExpr must have at least one subexpression"))
    (loop for sub across subs collect (e-coerce sub 'evm-node::|EExpr|))))

(def-node-maker evm-node::|SlotExpr|
  (nil)
  ((noun evm-node::|NounExpr|)))

(def-node-maker evm-node::|EScript|
  (t t)
  ((methods (or null vector)) (matcher (or null evm-node::|EMatcher|)))
  (progn
    (unless (or methods matcher)
      (error "EScript must have methods or matcher"))
    (list (and methods (map 'vector (lambda (sub) (e-coerce sub 'evm-node::|EMethod|)) methods))
          matcher)))
        
(def-node-maker evm-node::|ObjectExpr|
  (nil nil t t)
  ((doc-comment string) (qualified-name string) (auditor-exprs vector) (script evm-node::|EScript|))
  (list doc-comment qualified-name (map 'vector (lambda (sub) (e-coerce sub 'evm-node::|EExpr|)) auditor-exprs) script))

(def-node-maker evm-node::|EMethod|
  (nil nil t t t)
  ; XXX constrain patterns to vector of Pattern (write tests)
  ((doc-comment string) (verb string) (patterns vector) (opt-result-guard (or null evm-node::|EExpr|)) (body evm-node::|EExpr|))
  (list doc-comment verb patterns opt-result-guard body))

(def-node-maker evm-node::|EMatcher|
  (t t)
  ((pattern evm-node::|Pattern|) (body evm-node::|EExpr|))
  (list pattern body))

(def-node-maker evm-node::|EscapeExpr|
  (t t t t)
  ((ejector-pattern evm-node::|Pattern|) (body evm-node::|EExpr|) (opt-catch-pattern (or null evm-node::|Pattern|)) (opt-catcher (or null evm-node::|EExpr|))))

(def-node-maker evm-node::|CdrPattern|
  (t t)
  ((list-patt evm-node::|ListPattern|) (rest-patt evm-node::|Pattern|)))

(def-node-maker evm-node::|ListPattern|
  (t)
  ((subs t))
  (progn
    (loop for sub across subs collect (e-coerce sub 'evm-node::|Pattern|))))

(def-node-maker evm-node::|VarPattern|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun evm-node::|EExpr|) (opt-guard (or null evm-node::|EExpr|))))

(def-node-maker evm-node::|SlotPattern|
  (t t)
  ; XXX tighten noun to NounExpr or quasi-hole
  ((noun evm-node::|EExpr|) (opt-guard (or null evm-node::|EExpr|))))

(def-node-maker evm-node::|SuchThatPattern|
  (t t)
  ((subpattern evm-node::|Pattern|) (condition evm-node::|EExpr|)))
  
(def-node-maker evm-node::|IgnorePattern| () ())

(def-node-maker evm-node::|QuasiLiteralNode| (nil) ((index integer)))
(def-node-maker evm-node::|QuasiPatternNode| (nil) ((index integer)))
(def-node-maker evm-node::|QuasiLiteralExpr| (nil) ((index integer)))
(def-node-maker evm-node::|QuasiPatternExpr| (nil) ((index integer)))
(def-node-maker evm-node::|QuasiLiteralPatt| (nil) ((index integer)))
(def-node-maker evm-node::|QuasiPatternPatt| (nil) ((index integer)))

; --- E-level methods, and printing ---

(defmethod eeq-is-transparent-selfless ((a evm-node::|ENode|))
  (declare (ignore a))
  t)

(def-vtable evm-node::|ENode|
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (let ((quote (e-is-true (e. tw |isQuoting|))))
      (when quote
        (e. tw |print| (e. this |quasiTypeTag|) "`"))
      (e. this |welcome|
        (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw))
      (when quote
        (e. tw |print| "`"))))
  (:|__optUncall/0| (this)
    `#(,(let ((sm (get (observable-type-of this) 'static-maker)))
          (assert sm () "Missing maker for E-node class ~A" (observable-type-of this))
          sm)
       "run"
       #(nil ,@(node-visitor-arguments this) nil)))
  (:|asText/0| (this)
    (e-print this))
  (:|quasiTypeTag/0| (this)
    (declare (ignore this))
    "e??")
  (:|staticScope/0| (this)
    "Return a static scope analysis of this subtree that doesn't depend on the enclosing context."
    ; xxx in Java-E this caches the result (doing so will require StaticScope to be Selfless)
    (node-static-scope this))
  (:|substitute/1| (this args)
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
  (:|welcome/1| (this visitor)
    (e-call
      visitor
      ; xxx should we use our alleged type method instead of observable-type-of?
      (concatenate 'string "visit" (symbol-name (observable-type-of this))) 
      (cons this (node-visitor-arguments this))))
  (:|lnPrintOn/2| (this tw priority)
    "Java-E compatibility"
    (declare (ignore priority)) ; XXX
    (e. tw |println|)
    (e. this |welcome|
      (e. e.syntax:+e-printer+ |makePrintENodeVisitor| tw)))
  (:|subPrintOn/2| (this tw priority)
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

(defmethod e-call-match ((rec evm-node::|ENode|) mverb &rest args
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
(def-indexed-node-properties evm-node::|AssignExpr| ("noun" "rValue"))
(def-indexed-node-properties evm-node::|CallExpr| ("recipient" "verb" &rest "args"))
(def-indexed-node-properties evm-node::|CatchExpr| ("attempt" "pattern" "catcher"))
(def-indexed-node-properties evm-node::|DefineExpr| ("pattern" "rValue"))
(def-indexed-node-properties evm-node::|EscapeExpr| ("exitPattern" "body" "optArgPattern" "optCatcher"))
(def-indexed-node-properties evm-node::|FinallyExpr| ("attempt" "unwinder"))
(def-indexed-node-properties evm-node::|HideExpr| ("block"))
(def-indexed-node-properties evm-node::|IfExpr| ("test" "then" "else"))
(def-indexed-node-properties evm-node::|MatchBindExpr| ("specimen" "pattern"))
(def-indexed-node-properties evm-node::|NounExpr| ("name"))
(def-indexed-node-properties evm-node::|ObjectExpr| ("docComment" "qualifiedName" "auditorExprs" "script"))
(def-indexed-node-properties evm-node::|SeqExpr| (&rest "subs"))
(def-indexed-node-properties evm-node::|SlotExpr| ("noun"))

(def-indexed-node-properties evm-node::|EScript| ("optMethods" "optMatcher"))
(def-indexed-node-properties evm-node::|EMatcher| ("pattern" "body"))
(def-indexed-node-properties evm-node::|EMethod| ("docComment" "verb" "patterns" "optResultGuard" "body"))

(def-indexed-node-properties evm-node::|NounPattern| ("noun" "optGuardExpr"))
(def-indexed-node-properties evm-node::|CdrPattern| ("listPatt" "restPatt"))
(def-indexed-node-properties evm-node::|ListPattern| (&rest "subs"))
(def-indexed-node-properties evm-node::|SuchThatPattern| ("pattern" "test"))

(def-vtable evm-node::|EExpr|
  (:|quasiTypeTag/0| (this)
    (declare (ignore this))
    "e")
  (:|eval/1| #'eval-e)
  (:|evalToPair/1| (this scope)
    "Evaluate this expression in the given outer scope and return a tuple of the return value and a scope containing any new bindings."
    (multiple-value-call #'vector (eval-e this scope))))

(def-vtable evm-node::|AssignExpr|)

(def-vtable evm-node::|CallExpr|)

(def-vtable evm-node::|DefineExpr|)

(def-vtable evm-node::|EscapeExpr|)

(def-vtable evm-node::|LiteralExpr|)

(def-vtable evm-node::|NounExpr|
  (:|name/0| (this)
    (nth 0 (node-elements this))))

(def-vtable evm-node::|ObjectExpr|)

(def-vtable evm-node::|SeqExpr|)

(def-vtable evm-node::|SlotExpr|)

(def-vtable evm-node::|EMethod|)

(def-vtable evm-node::|EMatcher|)
    
(def-vtable evm-node::|EScript|
  (:|getDocComment/0| (this)
    (nth 0 (node-elements this))))
    

(def-vtable evm-node::|Pattern|
  (:|quasiTypeTag/0| (this)
    (declare (ignore this))
    "epatt"))

(def-vtable evm-node::|FinalPattern|)

(def-vtable evm-node::|VarPattern|)

(def-vtable evm-node::|IgnorePattern|)

(def-vtable evm-node::|SlotPattern|)

(def-vtable evm-node::|SuchThatPattern|)
      
; --- analysis ---

; XXX merge this with staticScope? not user-visible?
; XXX this isn't actually used
(defgeneric needs-reified-slot (node name))

(defmethod needs-reified-slot ((node evm-node::|ENode|) name)
  (some #'(lambda (node) (needs-reified-slot node name)) (node-elements node)))

(defmethod needs-reified-slot ((node evm-node::|SlotExpr|) name)
  (string= name (first (node-elements node))))

; XXX pattern-opt-noun and pattern-to-param-desc are looking similar - perhaps pattern-opt-noun should be defined as a wrapper

; XXX expose and test this
(defgeneric pattern-opt-noun (patt))

(defmethod pattern-opt-noun ((patt evm-node::|Pattern|))
  nil)

(defmethod pattern-opt-noun ((patt evm-node::|SuchThatPattern|))
  (pattern-opt-noun (first (node-elements patt))))

(defmethod pattern-opt-noun ((patt evm-node::|NounPattern|))
  ; XXX use method of noun expr once we have it
  (first (node-elements (e. patt |getNoun|))))


(defgeneric pattern-to-param-desc (pattern))

(defmethod pattern-to-param-desc ((pattern evm-node::|ENode|))
  (make-instance 'param-desc))

(defmethod pattern-to-param-desc ((pattern evm-node::|NounPattern|))
  (make-instance 'param-desc
    :opt-name (e-print (e. pattern |getNoun|))
    ; XXX make this a formal NotAGuard
    :opt-guard (opt-guard-expr-to-safe-opt-guard (e. pattern |getOptGuardExpr|))))

(defmethod pattern-to-param-desc ((patt evm-node::|SuchThatPattern|))
  (pattern-to-param-desc (first (node-elements patt))))

(defun opt-guard-expr-to-safe-opt-guard (opt-guard-expr
    &aux (opt-guard-string (if opt-guard-expr (e-print opt-guard-expr))))
  (if opt-guard-string
    (e-named-lambda "org.erights.e.elang.FalseGuard"
      :stamped +deep-frozen-stamp+
      "This is a false Guard created by Miranda __getAllegedType() representing the name of the guard expression in this object, but not the actual guard since it would be sometimes a security problem and/or impossible due to the guard expression not being constant over the life of the object."
      (:|__printOn/1| (tw) 
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |print| opt-guard-string)))))

; --- static scopes ---

(defun make-static-scope (&key
    has-meta-state-expr
    (def-names  (e. #() |asMap|))
    (var-names  (e. #() |asMap|))
    (read-names (e. #() |asMap|))
    (set-names  (e. #() |asMap|)))
  (with-result-promise (self)
    (e-named-lambda "org.erights.e.elang.evm.StaticScope"
      (:|__printOn| (tw)
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |print| "<" (e. set-names  |getKeys|) " := "
                           (e. read-names |getKeys|) " =~ "
                           (e. def-names  |getKeys|) " + var "
                           (e. var-names  |getKeys|) 
                           (if has-meta-state-expr 
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
            :read-names (e. left-read |or| (e. right-read |butNot| left-out))
            :set-names  (e. left-set  |or| (e. right-set  |butNot| left-out))
            :has-meta-state-expr (or left-meta right-meta))))
      (:|namesRead|        () read-names)
      (:|namesSet|         () set-names)
      (:|namesUsed|        () (e. read-names |or| set-names))
      (:|defNames|         () def-names)
      (:|varNames|         () var-names)
      (:|outNames|         () (e. def-names |or| var-names))
      (:|hasMetaStateExpr| () (as-e-boolean has-meta-state-expr))
      (:|hide|             () (make-static-scope 
                                :read-names read-names
                                :set-names set-names
                                :has-meta-state-expr has-meta-state-expr)))))

(flet ((make (node kind label)
        (make-static-scope kind
          (e. +the-make-const-map+ |fromPairs|
            `#(#(,label ,node))))))
  (defvar +the-make-static-scope+ (e-named-lambda "org.erights.e.evm.makeStaticScope"
    (:|scopeAssign| (node)
      (e-coercef node 'evm-node::|NounExpr|)
      (make node :set-names (e. node |name|)))
    (:|scopeDef| (node)
      (e-coercef node 'evm-node::|FinalPattern|)
      (make node :def-names (e. (e. node |getNoun|) |name|))))
    (:|scopeRead| (node)
      (e-coercef node '(or evm-node::|NounExpr| evm-node::|SlotExpr|))
      (make node :read-names (e. node |name|)))
    (:|scopeVar| (node)
      (e-coercef node 'evm-node::|VarPattern|)
      (make node :var-names (e. (e. node |getNoun|) |name|)))
    (:|scopeSlot| (node)
      (e-coercef node 'evm-node::|SlotPattern|)
      (make node :var-names (e. (e. node |getNoun|) |name|)))
    (:|scopeMeta| ()
      (make-static-scope :has-meta-state-expr t))
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

(def-scope-rule evm-node::|AssignExpr|
  (seq (! (e. +the-make-static-scope+ |scopeAssign| (:get :|noun|)))
       :|rValue|))

(def-scope-rule evm-node::|CallExpr|
  (seq :|recipient| (flatten :|args|)))

(def-scope-rule evm-node::|CatchExpr|
  (hide (seq (hide :|attempt|)
             :|pattern|
             :|catcher|)))

(def-scope-rule evm-node::|DefineExpr|
  (seq :|pattern| :|rValue|))

(def-scope-rule evm-node::|EscapeExpr|
  (hide (seq (hide (seq :|exitPattern| :|body|))
             (! (if (:get :|optArgPattern|)
                  (e. (e. (:get :|optArgPattern|) |staticScope|) 
                      |add|
                      (e. (:get :|optCatcher|) |staticScope|))
                  (e. +the-make-static-scope+ |getEmptyScope|))))))

(def-scope-rule evm-node::|FinallyExpr|
  (hide (seq (hide :|attempt|)
             :|unwinder|)))
      
(def-scope-rule evm-node::|HideExpr|
  (hide :|block|))

(def-scope-rule evm-node::|IfExpr|
  (seq (hide (seq :|test| :|then|)) (hide :|else|)))

(def-scope-rule evm-node::|LiteralExpr|
  nil)

(def-scope-rule evm-node::|MatchBindExpr|
  (seq :|specimen| :|pattern|))

(def-scope-rule evm-node::|MetaContextExpr|
  nil)

(def-scope-rule evm-node::|MetaStateExpr|
  (! (e. +the-make-static-scope+ |scopeMeta|)))

; XXX 'node' not explicitly bound
(def-scope-rule evm-node::|NounExpr|
  (! (e. +the-make-static-scope+ |scopeRead| node)))

(def-scope-rule evm-node::|ObjectExpr|
  (seq (flatten :|auditorExprs|) :|script|))

(def-scope-rule evm-node::|SeqExpr|
  (flatten :|subs|))

(def-scope-rule evm-node::|SlotExpr|
  (! (e. +the-make-static-scope+ |scopeRead| (:get :|noun|))))


(def-scope-rule evm-node::|CdrPattern|
  (seq :|listPatt| :|restPatt|))

; XXX hidden binding
(def-scope-rule evm-node::|FinalPattern|
  (! (e. +the-make-static-scope+ |scopeDef| node)))

(def-scope-rule evm-node::|IgnorePattern|
  nil)

(def-scope-rule evm-node::|ListPattern|
  (flatten :|subs|))

(def-scope-rule evm-node::|SlotPattern|
  (! (e. +the-make-static-scope+ |scopeSlot| node)))

(def-scope-rule evm-node::|SuchThatPattern|
  (seq :|pattern| :|test|))

(def-scope-rule evm-node::|VarPattern|
  (! (e. +the-make-static-scope+ |scopeVar| node)))

(def-scope-rule evm-node::|EMatcher|
  (hide (seq :|pattern|
             :|body|)))

(def-scope-rule evm-node::|EMethod|
  (hide (seq (flatten :|patterns|)
             (! (if (:get :|optResultGuard|)
                  (e. (:get :|optResultGuard|) |staticScope|)
                  (e. +the-make-static-scope+ |getEmptyScope|)))
             :|body|)))

(def-scope-rule evm-node::|EScript|
  (seq (! (if (:get :|optMethods|)
            (sum-node-scopes (:get :|optMethods|))
            (e. +the-make-static-scope+ |getEmptyScope|)))
       (! (if (:get :|optMatcher|)
            (e. (:get :|optMatcher|) |staticScope|)
            (e. +the-make-static-scope+ |getEmptyScope|)))))

; --- visitors ---

(defgeneric node-visitor-arguments (node))

(defmethod node-visitor-arguments ((node evm-node::|ENode|))
  (node-elements node))

(defmethod node-visitor-arguments ((node evm-node::|CallExpr|))
  (destructuring-bind (rec verb &rest args) (node-elements node)
    `(,rec ,verb ,(coerce args 'vector))))

(defmethod node-visitor-arguments ((node evm-node::|SeqExpr|))
  `(,(coerce (node-elements node) 'vector)))

(defmethod node-visitor-arguments ((node evm-node::|ListPattern|))
  `(,(coerce (node-elements node) 'vector)))
