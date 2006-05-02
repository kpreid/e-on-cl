; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.nonkernel
  (:use)
  (:export
    :|AccumExpr|
    :|AccumPlaceholderExpr|
    :|BinaryExpr|
    :|CoerceExpr|
    :|CompareExpr|
    :|ConditionalExpr|
    :|CurryExpr|
    :|DefrecExpr|
    :|ExitExpr|
    :|ForExpr|
    :|ForwardExpr|
    :|FunctionExpr|
    :|FunCallExpr|
    :|FunSendExpr|
    :|GetExpr|
    :|If1Expr|
    :|InterfaceExpr|
    :|ListExpr|
    :|MapExpr|
    :|MessageDescExpr|
    :|MismatchExpr|
    :|ModPowExpr|
    :|NKAssignExpr|
    :|NKObjectExpr|
    :|NullExpr|
    :|ObjectHeadExpr|
    :|ParamDescExpr|
    :|PrefixExpr|
    :|PropertyExpr|
    :|PropertySlotExpr|
    :|QuasiExpr|
    :|RangeExpr|
    :|SameExpr|
    :|SendExpr|
    :|SwitchExpr|
    :|ThunkExpr|
    :|UpdateExpr|
    :|URIExpr|
    :|URISchemeExpr|
    :|WhenExpr|
    :|WhenFnExpr|
    :|WhileExpr|
    :|BindPattern|
    :|MapPattern|
    :|QuasiParserExpr|
    :|QuasiPattern|
    :|SamePattern|
    :|TailPattern|
    :|FunctionObject|
    :|MethodObject|
    :|PlumbingObject|
    :|ETo|
    :|MapPatternAssoc|
    :|MapPatternImport|
    :|MapPatternRequired|
    :|MapPatternOptional|
    :|QuasiText|
    :|QuasiExprHole|
    :|QuasiPatternHole|
    :|QuasiEscape|))

(cl:defpackage :e.nonkernel.impl
  (:use :cl :e.util :e.elib :e.elang :e.elang.node-impl :e.elang.vm-node :e.nonkernel))

(cl:in-package :e.nonkernel.impl)

;;; --- Temporary name support ---

(define-node-class |TemporaryExpr| (|EExpr|)
  ((:|name| nil symbol)))
 
;; XXX make this less internal      
(def-scope-rule |TemporaryExpr|
  (e.elang::! (e. +the-make-static-scope+ |scopeRead| e.elang::node)))


(defun gennoun (label)
  "Return a temporary name expr which will be substituted to a concrete NounExpr with an unused name after expansion."
  (mn '|TemporaryExpr| (make-symbol label)))

(defun find-names (node)
  "Return a list, possibly containing duplicates, of the 'name' of every NounExpr in the tree."
  (etypecase node
    (null nil)
    (vector (loop for sub across node append (find-names sub)))
    (|NounExpr| (node-elements node))
    (|ENode| 
      (loop for subnode-flag across (e. (get (class-name (class-of node)) 'static-maker) |getParameterSubnodeFlags|)
            for sub in (elang::node-visitor-arguments node)
            append (if (e-is-true subnode-flag)
                     (find-names sub)
                     '())))))
    
(defun reify-temporaries (node 
    &aux (names (e. (coerce (find-names node) 'vector) |asMap|))
         (seen (make-hash-table))
         (index 0))
  "Replace all TemporaryExprs in the node tree with NounExprs."
  (labels ((substitution (node)
    (etypecase node
      (null nil)
      (vector (map 'vector #'substitution node))
      (|TemporaryExpr| 
        (let ((symbol (e. node |getName|)))
          (or 
            (gethash symbol seen)
            (setf (gethash symbol seen)
              (progn
                (incf index)
                (mn '|NounExpr| 
                  (loop with label = (symbol-name symbol)
                        for name = (format nil "~A__~A" label index)
                        while (e-is-true (e. names |maps| name))
                        do (incf index)
                        finally (return name))))))))
      (|ENode| 
        ;; XXX duplicated code
        (let ((maker (get (class-name (class-of node)) 'static-maker)))
          (e-call maker "run"
            ((lambda (args) `(nil ,@args nil))
              (loop for subnode-flag across (e. maker |getParameterSubnodeFlags|)
                    for sub in (elang::node-visitor-arguments node)
                    collect (if (e-is-true subnode-flag)
                              (substitution sub)
                              sub)))))))))
    (substitution node)))

(defun kernelize (node)
  "Convert a general E node tree to Kernel-E."
  (reify-temporaries (e-macroexpand-all node)))

;;; --- ---

(defgeneric e-macroexpand-1 (node stop)
  (:method ((node |ENode|) stop) (funcall stop))
  (:method ((node vector ) stop) (funcall stop))
  (:method ((node null   ) stop) (funcall stop)))

(defun e-macroexpand (node &aux (catch-tag (gensym)))
  ;; using throw instead of return because of ABCL compiler bug
  (catch catch-tag
    (loop (setf node (e-macroexpand-1 node (lambda () (throw catch-tag node)))))))

(defgeneric e-macroexpand-all (node))

(defmethod e-macroexpand-all ((node null))
  nil)

(defmethod e-macroexpand-all ((node vector))
  (map 'vector #'e-macroexpand-all node))
(defmethod e-macroexpand-all ((node string))
  (error "shouldn't happen: string encountered in node macroexpansion: ~S" node))

(defmethod e-macroexpand-all ((node |ENode|))
  (let* ((node (e-macroexpand node)))
    ;; XXX poor structure. should we have a separate "e-macroexpand-children"?
    (if (not (typep node '|ENode|))
      (e-macroexpand-all node)
      (let* ((maker (get (class-name (class-of node)) 'elang::static-maker))
             (new-node-args 
              (progn 
                (assert maker () "maker not found for ~S" node)
                (loop 
                  for subnode-flag across (e. maker |getParameterSubnodeFlags|)
                  for sub in (elang::node-visitor-arguments node)
                  collect (if (e-is-true subnode-flag)
                            (e-macroexpand-all sub)
                            sub)))))
        (e-call maker "run" `(nil ,@new-node-args nil))))))

;;; --- conveniences ---

(defun mn (name &rest elements)
  (make-instance name :elements elements))

(defun node-quote (value)
  (if value
    (mn '|LiteralExpr| value) 
    (mn '|NullExpr|)))

(defmacro defemacro (node-class-name (&rest superclasses) (&rest properties) (&rest options &key ((&whole node) (gensym "NODE")) &allow-other-keys) &body body
    &aux (stop (gensym "STOP")))
  `(progn
     (define-node-class ,node-class-name ,superclasses
       ,(loop for (name subnode type) in properties
              collect (list (intern (symbol-name name) :keyword) subnode type))
       ,@(let ((options (copy-seq options)))
           (remf options '&whole)
           options))
     (defmethod e-macroexpand-1 ((,node ,node-class-name) ,stop)
       (declare (ignore ,stop))
       ;; XXX can this be simplified with node-visitor-arguments?
       (destructuring-bind ,(let ((vars (mapcar #'first properties)))
                              (if (getf options :rest-slot)
                                `(,@(butlast vars) &rest ,@(last vars))
                                vars))
                           (node-elements ,node)
         ,@body))))

(defun noun-to-resolver-noun (noun)
  (make-instance '|NounExpr| :elements (list (concatenate 'string (e. noun |getName|) "__Resolver"))))

;;; --- ---

(defgeneric expand-accum-body (body accum-var))
(def-shorten-methods expand-accum-body 2)
;; Additional methods defined later in this file

(defemacro |AccumExpr| (|EExpr|) ((|initialValue| t |EExpr|)
                                  (|loop| t |EExpr|))
                                 ()
  (let ((accum-var (gennoun "accum")))
    (mn '|SeqExpr|
      (mn '|DefineExpr| (mn '|VarPattern| accum-var nil) |initialValue| nil)
      (expand-accum-body |loop| accum-var)
      accum-var)))

;; no expansion; will be substituted by AccumExpr expander
(define-node-class |AccumPlaceholderExpr| (|EExpr|) ())

(defmethod expand-accum-body ((body |CallExpr|) accum-var)
  (destructuring-bind (recipient verb args) (node-visitor-arguments body)
    (check-type (ref-shorten recipient) |AccumPlaceholderExpr|)
    ;; XXX this mess is evidence we need a handy make-node-according-to-"visitor"-arguments function (visitor arguments == maker arguments)
    (mn '|UpdateExpr| (apply #'mn '|CallExpr| accum-var verb (coerce args 'list)))))

(defemacro |BinaryExpr| (|EExpr|) ((|op| nil string)
                                   (|first| t |EExpr|)
                                   (|rest| t (e-list |EExpr|)))
                                  ()
  (if (member |op| '(".." "..!") :test #'string=)
    (mn '|RangeExpr| |first| 
                     (progn
                       (assert (= 1 (length |rest|)))
                       (elt |rest| 0))
                     (ecase (find-symbol |op| :keyword)
                       (:.. +e-true+)
                       (:..! +e-false+)))
    (apply #'mn '|CallExpr|
      |first|
      (case (find-symbol |op| :keyword)
        ;; XXX ensure that this list is complete
        (:+  "add")
        (:-  "subtract")
        (:*  "multiply")
        (:** "pow")
        (:/  "approxDivide")
        (:// "floorDivide")
        (:%  "remainder")
        (:%% "mod")
        (:&  "and")
        (:\| "or")
        (:&! "butNot")
        (:<< "shiftLeft")
        (:>> "shiftRight")
        (:^  "xor")
        (otherwise |op|))
      (coerce |rest| 'list))))

(defmethod expand-accum-body ((body |BinaryExpr|) accum-var)
  (destructuring-bind (op first rest) (node-visitor-arguments body)
    (check-type (ref-shorten first) |AccumPlaceholderExpr|)
    (mn '|UpdateExpr| (mn '|BinaryExpr| op accum-var rest))))

(defemacro |CoerceExpr| (|EExpr|) ((value t |EExpr|)
                                   (guard t |EExpr|))
                                  ()
  (mn '|CallExpr|
    (mn '|CallExpr|
      (mn '|NounExpr| "ValueGuard")
      "coerce"
      guard
      (mn '|NullExpr|))
    "coerce"
    value
    (mn '|NullExpr|)))

(defemacro |CompareExpr| (|EExpr|) ((|op| nil string)
                                    (|left| t |EExpr|)
                                    (|right| t |EExpr|))
                                   ()
  (mn '|CallExpr|
      (mn '|NounExpr| "__comparer")
      (case (find-symbol |op| :keyword)
        (:<=> "asBigAs")
        (:< "lessThan")
        (:<= "leq")
        (:> "greaterThan")
        (:>= "geq")
        (otherwise |op|))
      |left|
      |right|))

(defun conditional-op-p (x) (member x '("&&" "||") :test #'equal))

(defun keys-to-nouns (map)
  (map 'list (lambda (name) 
               ;; XXX the need for this test demonstrates that /something/ is broken; most likely, that StaticScopes unwrap their Noun-or-TemporaryExprs
               (etypecase name
                 (string (mn '|NounExpr| name))
                 (symbol (mn '|TemporaryExpr| name)))) 
             (ref-shorten (e. map |getKeys|))))

(defemacro |ConditionalExpr| (|EExpr|) ((|op| nil (satisfies conditional-op-p))
                                        (|left| t |EExpr|)
                                        (|right| t |EExpr|))
                                       ()
  (let* ((kernel-left  (e-macroexpand-all |left|))
         (kernel-right (e-macroexpand-all |right|))
         (left-map  (e. (e. kernel-left |staticScope|) |outNames|))
         (right-map (e. (e. kernel-right |staticScope|) |outNames|))
         (both-nouns (keys-to-nouns (e. left-map |or| right-map)))
         (slot-list (apply #'mn '|ListExpr| 
                      (map 'list (lambda (noun) (mn '|SlotExpr| noun))
                                 both-nouns))))
    (labels ((slot-patterns (nouns)
               (apply #'mn '|ListPattern|
                 (map 'list (lambda (noun) (mn '|SlotPattern| noun nil))
                            nouns)))
             (bind-failure (nouns) 
               (mn '|MatchBindExpr| (mn '|NullExpr|)
                                    (slot-patterns nouns))))
      (mn '|MatchBindExpr|
        (ecase (find-symbol |op| :keyword)
          (:\|\|
           (let* ((left-only  (keys-to-nouns (e. left-map |butNot| right-map)))
                  (right-only (keys-to-nouns (e. right-map |butNot| left-map))))
             (mn '|IfExpr| kernel-left 
                           (mn '|SeqExpr|
                             (bind-failure right-only)
                             slot-list)
                           (mn '|IfExpr| kernel-right
                             (mn '|SeqExpr|
                               (bind-failure left-only)
                               slot-list)
                             (mn '|NullExpr|)))))
          (:&&   
           (mn '|IfExpr| kernel-left
                         (mn '|IfExpr| kernel-right
                                       slot-list
                                       (mn '|NullExpr|))
                         (mn '|NullExpr|))))
        (slot-patterns both-nouns)))))

(defemacro |CurryExpr| (|EExpr|) ((|expr| t (or |CallExpr| |SendExpr|)))
                                 ()
  ;; xxx may want to unrestrict the type eventually?
  (mn '|CallExpr|
    (mn '|NounExpr| "__makeVerbFacet")
    (etypecase |expr|
      (|CallExpr| "curryCall")
      (|SendExpr| "currySend"))
    (e. |expr| |getRecipient|)
    (mn '|LiteralExpr| (e. |expr| |getVerb|))))

(defemacro |PromiseVarExpr| (|EExpr|) ((|promiseNoun| t |EExpr|)
                                       (|resolverNoun| t |EExpr|))
                                      ()
  (mn '|DefrecExpr|
    (mn '|ListPattern|
      (mn '|FinalPattern| |promiseNoun| nil)
      (mn '|FinalPattern| |resolverNoun| nil))
    (mn '|CallExpr| (mn '|NounExpr| "Ref") "promise")
    nil))


(defun make-noun-substitute-visitor (map)
  "Replaces NounExprs according to the map. Requires optOriginals."
  (e-lambda noun-substitute-visitor ()
    (:|run| (what)
      (setf what (ref-shorten what))
      (etypecase what
        (null nil)
        (vector (map 'vector (lambda (x) (e. noun-substitute-visitor |run| x)) what))
        (|ENode| (e. what |welcome| noun-substitute-visitor))))
    (:|visitNounExpr| (opt-original name)
      (declare (ignore name))
      (e. map |fetch| opt-original (efun () opt-original)))
    (otherwise (mverb opt-original &rest args)
      (declare (ignore mverb))
      ;; XXX need to update the visitor protocol to consider nonkernel nodes -
      ;; this isn't even really a visitor because of that
      (let ((maker (get (class-name (class-of opt-original)) 'static-maker)))
        (e-call maker "run"
          ((lambda (args) `(nil ,@args nil))
            (loop for subnode-flag across (e. maker |getParameterSubnodeFlags|)
                  for sub in args
                  collect (if (e-is-true subnode-flag)
                            (e. noun-substitute-visitor |run| sub)
                            sub))))))))

(defemacro |DefrecExpr| (|EExpr|) ((|pattern| t |Pattern|)
                                   (|rValue| t |EExpr|)
                                   (|optEjectorExpr| t (or null |EExpr|)))
                                  ()
  (let* ((kernel-pattern (e-macroexpand-all |pattern|))
         (kernel-r-value (e-macroexpand-all |rValue|))
         (kernel-ejector (e-macroexpand-all |optEjectorExpr|))
         (left-scope (e. kernel-pattern |staticScope|))
         (right-scope (e. (e. kernel-r-value |staticScope|)
                          |add|
                          (if kernel-ejector
                            (e. kernel-ejector |staticScope|)
                            elang::+empty-static-scope+)))
         (common-names
           (e. (e. (e. left-scope |outNames|) |and| (e. right-scope |namesUsed|)) |getKeys|)))
    (if (plusp (length common-names))
      (let* ((result-noun (gennoun "value"))
             (common-nouns (map 'vector (lambda (n) (mn '|NounExpr| n)) common-names))
             resolver-nouns
             replacement-nouns
             forward-exprs)
        (loop for name across common-names 
              for resolver-noun = (gennoun (format nil "~AR" name))
              for replacement-noun = (gennoun name)
              do
          (push resolver-noun resolver-nouns)
          (push replacement-noun replacement-nouns)
          (push (mn '|PromiseVarExpr| replacement-noun resolver-noun) forward-exprs))
        (nreverse-here resolver-nouns)
        (nreverse-here replacement-nouns)
        (flet ((substitute-recursions (expr)
                 (e. (make-noun-substitute-visitor
                       (e. +the-make-const-map+ |fromColumns|
                         common-nouns
                         (coerce replacement-nouns 'vector)))
                     |run| expr)))
          (apply #'mn '|SeqExpr|
            `(,@forward-exprs
              ,(mn '|DefineExpr| (mn '|FinalPattern| result-noun nil)
                 (mn '|DefineExpr| kernel-pattern 
                                   (substitute-recursions
                                     kernel-r-value) 
                                   (substitute-recursions
                                     kernel-ejector))
                 nil)
              ,@(loop for vn across common-nouns 
                      for rn in resolver-nouns
                      collect (mn '|CallExpr| rn "resolve" vn))
              ,result-noun))))
      (mn '|DefineExpr| kernel-pattern kernel-r-value kernel-ejector))))

(defemacro |ForExpr| (|EExpr|) ((|optKeyPattern| t (or |Pattern| null))
                                (|valuePattern| t |Pattern|)
                                (|collection| t |EExpr|)
                                (|body| t |EExpr|))
                               (&whole node)
  (let ((valid-flag-var (gennoun "valid"))
        (key-var (gennoun "key"))
        (value-var (gennoun "value"))
        (pattern-escape-var (gennoun "skip"))
        (key-pattern (or |optKeyPattern| (mn '|IgnorePattern|))))
    ;; XXX this shows there should be an ejector for expansion
    (reject-definition-usage node nil nil :|optKeyPattern| :|collection|)
    (reject-definition-usage node nil t   :|collection| :|optKeyPattern|)
    (reject-definition-usage node nil nil :|valuePattern| :|collection|)
    (reject-definition-usage node nil t   :|collection| :|valuePattern|)
    (mn '|EscapeExpr| (mn '|FinalPattern| (mn '|NounExpr| "__break") nil)
      (mn '|SeqExpr|
        (mn '|DefineExpr| (mn '|VarPattern| valid-flag-var nil)
                          (mn '|NounExpr| "true") nil)
        (mn '|FinallyExpr|
          (mn '|CallExpr| |collection| "iterate"
            (mn '|ObjectExpr|
              ""
              "_"
              #()
              (mn '|EScript| 
                (vector (mn '|EMethod| 
                  "" "run" (vector (mn '|FinalPattern| key-var nil)
                                   (mn '|FinalPattern| value-var nil)) 
                  nil
                  (mn '|SeqExpr|
                    (mn '|CallExpr| (mn '|NounExpr| "require") 
                                    "run"
                                    valid-flag-var
                                    (mn '|LiteralExpr| "For-loop body isn't valid after for-loop exits."))
                    (mn '|EscapeExpr| (mn '|FinalPattern|
                                        (mn '|NounExpr| "__continue") nil) 
                                          
                      (mn '|SeqExpr|
                        (mn '|EscapeExpr| (mn '|FinalPattern|
                                            pattern-escape-var nil)
                          (mn '|SeqExpr|
                            (mn '|DefineExpr| key-pattern 
                                              key-var pattern-escape-var)
                            (mn '|DefineExpr| |valuePattern| 
                                              value-var pattern-escape-var)
                            |body|)
                          nil nil)
                        ;; this null-expr prevents the pattern failures or the loop body's final expression from being returned from the iterate callback; only __continue can return non-null
                        (mn '|NullExpr|))
                      nil nil)))) 
                #())))
          (mn '|AssignExpr| valid-flag-var (mn '|NounExpr| "false")))
        ;; this null-expr prevents the return value of 'iterate' from being returned from the for expr; only __break can return non-null
        (mn '|NullExpr|))
      nil nil)))

(defmethod expand-accum-body ((node |ForExpr|) accum-var)
  (destructuring-bind (k v c body) (node-visitor-arguments node)
    (mn '|ForExpr| k v c (expand-accum-body body accum-var))))

(defemacro |ForwardExpr| (|EExpr|) ((|noun| nil |EExpr|)) ()
  (let* ((real-noun (e-macroexpand-all |noun|))
         (resolver-noun (noun-to-resolver-noun real-noun)))
      (make-instance '|SeqExpr| :elements (list
        (mn '|PromiseVarExpr| real-noun resolver-noun)
        resolver-noun))))

(defemacro |FunctionExpr| (|EExpr|) ((|patterns| t (e-list |Pattern|))
                                  (|body| t |EExpr|))
                                 ()
  (mn '|ObjectExpr|
      ""
      "_"
      #()
      (mn '|EScript| (vector (mn '|EMethod| "" "run" |patterns| nil |body|)) 
                             #())))

(defemacro |FunCallExpr| (|EExpr|) ((|recipient| t |EExpr|) 
                                    (|args| t (e-list |EExpr|)))
                               (:rest-slot t)
  (apply #'mn '|CallExpr| |recipient| "run" |args|))

(defemacro |FunSendExpr| (|EExpr|) ((|recipient| t |EExpr|) 
                                    (|args| t (e-list |EExpr|)))
                               (:rest-slot t)
  (apply #'mn '|SendExpr| |recipient| "run" |args|))

(defemacro |GetExpr| (|EExpr|) ((|recipient| t |EExpr|) 
                                (|args| t (e-list |EExpr|)))
                               (:rest-slot t)
  (apply #'mn '|CallExpr| |recipient| "get" |args|))

(defemacro |If1Expr| (|EExpr|) ((|test| t |EExpr|) 
                                (|then| t |EExpr|))
                               ()
  (mn '|IfExpr| |test| |then| (mn '|NullExpr|)))

(defmethod expand-accum-body ((body |If1Expr|) accum-var)
  (destructuring-bind (test then) (node-visitor-arguments body)
    (mn '|If1Expr| test (expand-accum-body then accum-var))))

;; using LiteralExpr for the name is a bit clunky but necessary: when it's a pattern the slot must be marked as a subnode. XXX come up with a better solution, and apply it to NKObjectExpr too
(defemacro |InterfaceExpr| (|EExpr|) ((|docComment| nil string)
                                      (|name| t (or null |Pattern| |LiteralExpr|))
                                      (|optStamp| t (or null |Pattern|))
                                      (|parents| t (e-list |EExpr|))
                                      (|auditors| t (e-list |EExpr|)) 
                                      (|messages| t (e-list |MessageDescExpr|)))
                                     ()
  (labels ((whatever-to-qn (whatever)
             (etypecase whatever
               (string whatever)
               (|LiteralExpr| (e. whatever |getValue|))
               (null "$_")
               (|Pattern|
                 (let ((noun-string (pattern-opt-noun whatever)))
                         (if noun-string
                             ;; XXX remove the __T once we're out of the compatibility phase
                             (format nil "$~A__T" noun-string)
                             "_")))))
           (nodify-seq (seq)
             (apply #'mn '|ListExpr| (coerce seq 'list)))
           (make-expr (qn verb)
             (mn '|HideExpr|
               (mn '|CallExpr|
                 (mn '|NounExpr| "__makeProtocolDesc")
                 verb
                 (node-quote |docComment|)
                 (let ((pqn (without-prefix qn "$")))
                   (if pqn
                     (mn '|CallExpr| (mn '|CallExpr| (mn '|MetaContextExpr|) "getFQNPrefix") "add" (node-quote pqn))
                     (node-quote qn)))
                 (nodify-seq |parents|)
                 (nodify-seq |auditors|)
                 (nodify-seq |messages|)))))
  (cond
    (|optStamp|
      (mn '|CallExpr|
        (mn '|DefrecExpr|
          (mn '|ListPattern|
            (if (typep |name| '|Pattern|)
              |name|
              (mn '|IgnorePattern|))
            |optStamp|)
          (make-expr (whatever-to-qn |name|) "makePair")
          nil)
        "get" (node-quote 0)))
    ((typep |name| '|Pattern|)
      (mn '|DefrecExpr|
        |name| 
        (make-expr (whatever-to-qn |name|) "run")
        nil))
    ((typep |name| '(or null |LiteralExpr|))
      (make-expr (whatever-to-qn |name|) "run")))))
  
(defemacro |ListExpr| (|EExpr|) ((|subs| t (e-list |EExpr|))) (:rest-slot t)
  (apply #'mn '|CallExpr|
      (mn '|NounExpr| "__makeList")
      "run"
      (coerce |subs| 'list)))

(defemacro |MapExpr| (|EExpr|) ((|pairs| t (e-list |EExpr|)))
                               (:rest-slot t)
  (mn '|CallExpr| (mn '|NounExpr| "__makeMap") 
                  "fromPairs" 
                  (apply #'mn '|ListExpr| (coerce |pairs| 'list))))

(defemacro |MessageDescExpr| (|EExpr|)
    ((|docComment| nil string)
     (|verb| nil string)
     (|params| t (e-list |ParamDescExpr|))
     (|optResultType| t (or null |EExpr|)))
    ()
  ;; XXX justify the HideExpr
  (mn '|HideExpr|
    (mn '|CallExpr| (mn '|NounExpr| "__makeMessageDesc")
                    "run"
                    (mn '|LiteralExpr| |docComment|)
                    (mn '|LiteralExpr| |verb|)
                    (apply #'mn '|ListExpr| (coerce |params| 'list))
                    (or |optResultType| (mn '|NullExpr|)))))

(defemacro |MismatchExpr| (|EExpr|) ((|specimen| t |EExpr|)
                                     (|pattern| t |Pattern|))
                                    ()
  (mn '|CallExpr| (mn '|MatchBindExpr| |specimen| |pattern|) "not"))

(defemacro |ModPowExpr| (|EExpr|) ((|base| t |EExpr|)
                                   (|exponent| t |EExpr|)
                                   (|modulus| t |EExpr|))
                                  ()
  (mn '|CallExpr| |base| "modPow" |exponent| |modulus|))

(defemacro |NKAssignExpr| (|EExpr|) ((|place| t |EExpr|)
                                     (|value| t |EExpr|))
                                    ()
  (setf |place| (e-macroexpand-all |place|)) ;; expand function calls, etc.
  (typecase |place|
    (|CallExpr|
     (let* ((verb (e. |place| |getVerb|))
            (value-noun (gennoun "ares"))
            (definer (mn '|DefineExpr| (mn '|FinalPattern| value-noun nil) |value| nil)))
       (flet ((make-call (out-verb)
                (apply #'mn '|CallExpr| (e. |place| |getRecipient|) 
                                        out-verb
                                        `(,@(coerce (e. |place| |getArgs|) 
                                                    'list)
                                          ,definer))))
         (mn '|SeqExpr|
           (cond
             ((string= verb "get")
              (make-call "put"))
             ((string= verb "run")
              (make-call "setRun"))
             ((let ((s (without-prefix verb "get")))
                (when s
                  (make-call (format nil "set~A" s)))))
             (t
              (error "assignment can only be done to nouns and collection elements (not ~A call)" (e-quote verb) #| XXX ejector? |#)))
           value-noun))))
    ((or |NounExpr| |TemporaryExpr|)
     (mn '|AssignExpr| |place| |value|))
    (t
     (error "Assignment can only be done to nouns and collection elements (not ~A)" (e-quote |place|) #| XXX ejector? |#))))

(defemacro |NKObjectExpr| (|EExpr|) ((|docComment| nil string)
                                     (|name| nil (or null |Pattern| string))
                                     (|parent| t (or null |EExpr|))
                                     (|auditors| t (e-list |EExpr|))
                                     (|script| t e.elang::|EScriptoid|))
                                    ()
  (etypecase |name|
    (|Pattern|
      (mn '|DefrecExpr| 
        |name| 
        (mn '|NKObjectExpr| 
          |docComment|
          (let ((noun-string (pattern-opt-noun |name|)))
            (if noun-string
              ;; XXX remove the __C once we're out of the compatibility phase
              (format nil "$~A__C" noun-string)
              "_"))
          |parent|
          |auditors|
          |script|) 
        nil))
    ((or null string)
      (if |parent|
        (mn '|HideExpr|
          (mn '|SeqExpr|
            (mn '|DefrecExpr| (mn '|FinalPattern| (mn '|NounExpr| "super") nil)
                              |parent|
                              nil)
            (mn '|NKObjectExpr| |docComment| |name| nil |auditors|
                                (mn '|EScript|
                                  (e. |script| |getOptMethods|)
                                  (e. (e. |script| |getMatchers|) |with|
                                    (let ((msg-noun (gennoun "message")))
                                      (mn '|EMatcher|
                                        (mn '|FinalPattern| msg-noun nil)
                                        (mn '|CallExpr| (mn '|NounExpr| "E")
                                                        "callWithPair"
                                                        (mn '|NounExpr| "super")
                                                        msg-noun))))))))
        ;; XXX is introducing _ here the right thing? or should kernel ObjectExpr accept nil for the qualifiedName?
        (mn '|ObjectExpr| |docComment| (or |name| "_") |auditors| |script|)))))

(defemacro |NullExpr| (|EExpr|) () ()
  (mn '|NounExpr| "null"))

(defemacro |ObjectHeadExpr| (|EExpr|) ((|docComment| nil string)
                                       (|name| nil (or null |Pattern| string))
                                       (|tail| t |ObjectTail|))
                                      ()
  ;; XXX possibly merge NKObjectExpr into this
  (multiple-value-bind (doc tail)
      (if (typep |tail| '|FunctionObject|)
        (values "" (e. |tail| |withFunctionDocumentation| |docComment|))
        (values |docComment| |tail|))
    (destructuring-bind (parent auditors script) 
        (coerce (e-macroexpand #| NOT -all |# tail) 'list)
      (mn '|NKObjectExpr| doc |name| parent auditors script))))

(define-node-class |ObjectTail| (|ENode|) ())

(defemacro |ParamDescExpr| (|EExpr|)
    ((|optName| nil (or null string))
     (|optGuard| t (or null |EExpr|)))
    ()
  (mn '|CallExpr| (mn '|NounExpr| "__makeParamDesc")
                  "run"
                  (node-quote |optName|)
                  (or |optGuard| (mn '|NullExpr|))))

(defemacro |PrefixExpr| (|EExpr|) ((|op| nil string)
                                   (|argument| t |EExpr|))
                                  ()
  (mn '|CallExpr|
      |argument|
      (case (find-symbol |op| :keyword)
        (:! "not")
        (:~ "complement")
        (:- "negate")
        (otherwise |op|))))

(defemacro |PropertyExpr| (|EExpr|) ((|recipient| t |EExpr|) 
                                     (|key| nil string))
                                    ()
  (mn '|CallExpr| (mn '|PropertySlotExpr| |recipient| |key|) "getValue"))

(defemacro |PropertySlotExpr| (|EExpr|) ((|recipient| t |EExpr|) 
                                         (|key| nil string))
                                    ()
  (mn '|CallExpr| |recipient| "__getPropertySlot" (node-quote |key|)))

(defemacro |QuasiExpr| (|EExpr|) 
    ((|optParser| t (or null |EExpr|))
     (|parts| t (e-list (and |QuasiPart| 
                             (not |QuasiPatternHole|)))))
    (:rest-slot t)
  (mn '|CallExpr|
    (mn '|CallExpr| (quasi-deopt-parser |optParser|)
                    "valueMaker"
                    (quasi-description |parts|))
    "substitute"
    (quasi-value-list |parts|)))

(defemacro |QuasiParserExpr| (|EExpr|) ((|name| nil string)) ()
  (mn '|NounExpr| (format nil "~A__quasiParser" |name|)))

(defemacro |RangeExpr| (|EExpr|) ((|start| t |EExpr|)
                                  (|end| t |EExpr|)
                                  (|thru| nil e-boolean))
                                 ()
  (mn '|CallExpr|
      (mn '|NounExpr| "__makeOrderedSpace")
      (if (e-is-true |thru|) "op__thru" "op__till")
      |start|
      |end|))

(defemacro |ExitExpr| (|EExpr|) ((|label| nil string)
                                 (|value| t (or null |EExpr|))) ()
  (apply #'mn '|CallExpr| (mn '|NounExpr| (format nil "__~A" |label|))
                          "run" 
                          (when |value| (list |value|))))

(defemacro |SameExpr| (|EExpr|) ((|left| t |EExpr|)
                                 (|right| t |EExpr|)
                                 (|invert| nil e-boolean))
                                ()
  (if (e-is-true |invert|)
    (mn '|CallExpr| (mn '|SameExpr| |left| |right| +e-false+) "not")
    (mn '|CallExpr| (mn '|NounExpr| "__equalizer") "sameEver" |left| |right|)))

(defemacro |SendExpr| (|EExpr|) ((|recipient| t |EExpr|)
                                 (|verb| nil string) 
                                 (|args| t (e-list |EExpr|)))
                                (:rest-slot t)
  (mn '|CallExpr|
    (mn '|NounExpr| "E")
    "send"
    |recipient| 
    (mn '|LiteralExpr| |verb|)
    (apply #'mn '|CallExpr| (mn '|NounExpr| "__makeList") "run" (coerce |args| 'list))))

(defemacro |SwitchExpr| (|EExpr|) ((|specimen| t |EExpr|)
                                   (|matchers| nil (e-list |EMatcher|)))
                                  (:rest-slot t)
  (let ((specimen-var (gennoun "specimen")))
    ;; NOTE: I briefly got confused. Even if |specimen| turns out to be a
    ;; NounExpr, we can't skip employing a temporary var, because the noun
    ;; might be for a special slot.
    (mn '|HideExpr|
      (mn '|SeqExpr|
        (mn '|DefineExpr| (mn '|FinalPattern| specimen-var nil) |specimen| nil)
        (labels ((match-chain (list) 
                  (if list
                    (let ((matcher (first list)))
                      (mn '|IfExpr|
                        (mn '|MatchBindExpr| specimen-var
                                             (e. matcher |getPattern|))
                        (e. matcher |getBody|)
                        (match-chain (rest list))))
                    (mn '|CallExpr| (mn '|NounExpr| "throw") "run"
                      (mn '|CallExpr| (mn '|LiteralExpr| "no match: ") "add"
                        specimen-var)))))
          (match-chain (coerce |matchers| 'list)))))))

(defemacro |ThunkExpr| (|EExpr|) ((|docComment| nil string)
                                  (|body| t |EExpr|))
                                 ()
  (mn '|ObjectExpr|
      ""
      "_"
      #()
      (mn '|EScript| (vector (mn '|EMethod| |docComment| "run" #() nil |body|)) 
                             #())))

(defemacro |UpdateExpr| (|EExpr|) ((|call| t (or |CallExpr| |BinaryExpr|)))
                                  ()
  (let ((exp (e-macroexpand-all |call|)))
    (check-type exp |CallExpr|)
    (let ((target (e-macroexpand-all (ref-shorten (e. exp |getRecipient|)))))
      (etypecase target
        ((or |NounExpr| |TemporaryExpr|)
          ;; update of noun
          (mn '|NKAssignExpr| target exp))
        (|CallExpr|
          ;; update of complex access (a[b] += c)
          (let ((defs '()))
            (flet ((nsub (node label)
                     (let ((temp (gennoun label)))
                       (push (mn '|DefineExpr| (mn '|FinalPattern| temp nil) node nil) defs)
                       temp)))
              (let* ((onceized (apply #'mn '|CallExpr| 
                                 (nsub (e. target |getRecipient|) "recip") 
                                 (e. target |getVerb|)
                                 (map 'list (lambda (x) (nsub x "arg")) 
                                            (e. target |getArgs|)))))
                (apply #'mn '|SeqExpr|
                  `(,@(nreverse defs)
                    ,(mn '|NKAssignExpr|
                       onceized
                       (apply #'mn '|CallExpr| onceized (e. exp |getVerb|) (coerce (e. exp |getArgs|) 'list)))))))))))))

(defemacro |URIExpr| (|EExpr|) ((|uri| nil string))
                               ()
  (let ((sep (position #\: |uri|)))
    (mn '|CallExpr|
        (mn '|URISchemeExpr| (subseq |uri| 0 sep))
        "get"
        (mn '|LiteralExpr| (subseq |uri| (1+ sep))))))

(defemacro |URISchemeExpr| (|EExpr|) ((|scheme| nil string))
                                     ()
  (mn '|NounExpr| (format nil "~A__uriGetter" (string-downcase |scheme|))))

(defemacro |WhenExpr| (|EExpr|) ((|args| t (e-list |EExpr|))
                                 (|reactor| t |WhenFnExpr|)) ;; loosen?
                                ()
  (assert (= (length |args|) (length (ref-shorten (e. |reactor| |getParams|)))) ()
          "must have same number of expressions and patterns")
  (mn '|HideExpr|
    (mn '|CallExpr|
      (mn '|NounExpr| "Ref")
      "whenResolved"
      (if (= 1 (length |args|))
        (elt |args| 0)
        (mn '|CallExpr| (mn '|NounExpr| "promiseAllFulfilled") "run"
                        (apply #'mn '|ListExpr| (coerce |args| 'list))))
      |reactor|)))

(defemacro |WhenFnExpr| (|EExpr|) ((|name| t |Pattern|)
                                   (|params| t (e-list |Pattern|))
                                   (|optResultGuard| t (or null |EExpr|))
                                   (|body| t |EExpr|)
                                   (|catchers| t (e-list |EMatcher|))
                                   (|optFinally| t (or null |EExpr|))
                                   (|isEasyReturn| nil e-boolean))
                                  ()
  (let ((resolution (gennoun "resolution")))
    (mn '|ObjectHeadExpr|
      "" |name|
      (mn '|FunctionObject| (vector (mn '|FinalPattern| resolution nil)) |optResultGuard| #()
        ;; XXX replace finally/catch handling with expansion to TryExpr, once that's nonkernel instead of syntax layer
        (labels ((do-finally (e)
                   (if |optFinally|
                     (mn '|FinallyExpr| e |optFinally|)
                     e))
                 (do-catchers (c e)
                   (if c
                     (let ((catcher (first c)))
                       (do-catchers (rest c) (mn '|CatchExpr| e (e. catcher |getPattern|) (e. catcher |getBody|))))
                     e)))
        (do-finally 
          (do-catchers (coerce |catchers| 'list) 
            (mn '|SeqExpr|
              (mn '|DefineExpr|
                (if (= 1 (length |params|))
                  (elt |params| 0)
                  (apply #'mn '|ListPattern| (coerce |params| 'list)))
                (mn '|CallExpr| (mn '|NounExpr| "Ref") "fulfillment" resolution)
                nil)
              |body|))))
        |isEasyReturn|))))

(defemacro |WhileExpr| (|EExpr|) ((|test| t |EExpr|)
                                  (|body| t |EExpr|))
                                 ()
  (mn '|EscapeExpr| (mn '|FinalPattern| (mn '|NounExpr| "__break") nil)
    (mn '|CallExpr| (mn '|NounExpr| "__loop") "run"
      (mn '|ObjectExpr|
        ""
        "_"
        #()
        (mn '|EScript| 
          (vector (mn '|EMethod| 
            "" "run" #() (mn '|NounExpr| "boolean")
            (mn '|IfExpr|
              |test|
              (mn '|SeqExpr|
                (mn '|EscapeExpr| (mn '|FinalPattern|
                                    (mn '|NounExpr| "__continue") nil) 
                                  |body| nil nil)
                (mn '|NounExpr| "true"))
              (mn '|NounExpr| "false")))) 
          #())))
    nil nil))

(defmethod expand-accum-body ((node |WhileExpr|) accum-var)
  (destructuring-bind (test body) (node-visitor-arguments node)
    (mn '|WhileExpr| test (expand-accum-body body accum-var))))
    
(defemacro |BindPattern| (|Pattern|) ((|noun| t |EExpr|)
                                      (|optGuard| t (or null |EExpr|)))
                                     ()
  (let* ((real-noun (e-macroexpand-all |noun|))
         (temp (gennoun (e. real-noun |getName|))))
    (make-instance '|SuchThatPattern| :elements (list
      (make-instance '|FinalPattern| :elements (list
        temp
        |optGuard|))
      (make-instance '|SeqExpr| :elements (list
        (make-instance '|CallExpr| :elements (list
          (noun-to-resolver-noun real-noun)
          "resolve"
          temp))
        (make-instance '|NounExpr| :elements '("true"))))))))

(defmethod pattern-opt-noun ((patt |BindPattern|))
  ;; XXX this is identical code to pattern-opt-noun of NounPattern
  (let ((kernel-noun (e-macroexpand-all (e. patt |getNoun|))))
    (when (typep kernel-noun '|NounExpr|)
      (e. kernel-noun |getName|))))

;; XXX lousy name
(define-node-class |MapPatternKeyer| (|ENode|) ())

;; XXX lousy names
(defgeneric map-pattern-key (keyer))
(defgeneric map-pattern-value (keyer))

;; XXX mediocre name
(define-node-class |MapPatternAssoc| (|MapPatternKeyer|)
  ((:|key| t |EExpr|)
   (:|value| t |Pattern|)))

(defmethod map-pattern-key ((keyer |MapPatternAssoc|))
  (e. keyer |getKey|))
(defmethod map-pattern-value ((keyer |MapPatternAssoc|))
  (e. keyer |getValue|))

(define-node-class |MapPatternImport| (|MapPatternKeyer|)
  ((:|keyValue| t |Pattern|)))

(defmethod map-pattern-key ((keyer |MapPatternImport|))
  (let ((key-value (e-macroexpand-all (e. keyer |getKeyValue|))))
    (mn '|LiteralExpr|
      (etypecase key-value
        ((or |FinalPattern| |VarPattern|) (e. (e. key-value |getNoun|) |getName|))
        (|SlotPattern| (format nil "&~A" (e. (e. key-value |getNoun|) |getName|)))))))
(defmethod map-pattern-value ((keyer |MapPatternImport|))
  (e. keyer |getKeyValue|))

(define-node-class |MapPatternPart| (|ENode|) ())

(defgeneric build-incremental-map-pattern (first rest))

(define-node-class |MapPatternRequired| (|MapPatternPart|)
  ((:|keyer| t |MapPatternKeyer|)))
   
(defmethod build-incremental-map-pattern ((first |MapPatternRequired|) rest)
  (let ((map-var (gennoun "map"))
        (assoc (e. first |getKeyer|)))
    (mn '|SuchThatPattern|
      (mn '|FinalPattern| map-var nil)
      (mn '|MatchBindExpr|
        (mn '|CallExpr| map-var "optExtract" (map-pattern-key assoc))
        (mn '|ListPattern|
          (map-pattern-value assoc)
          (build-incremental-map-pattern (first rest) (rest rest)))))))

(define-node-class |MapPatternOptional| (|MapPatternPart|)
  ((:|keyer| t |MapPatternKeyer|)
   (:|default| t |EExpr|)))

(defmethod build-incremental-map-pattern ((first |MapPatternOptional|) rest)
  (let ((map-var (gennoun "map"))
        (assoc (e. first |getKeyer|)))
    (mn '|SuchThatPattern|
      (mn '|FinalPattern| map-var nil)
      (mn '|MatchBindExpr|
        (mn '|CallExpr| map-var "extract" (map-pattern-key assoc)
                                          (e. first |getDefault|))
        (mn '|ListPattern|
          (map-pattern-value assoc)
          (build-incremental-map-pattern (first rest) (rest rest)))))))

(define-node-class |MapPatternRest| (|MapPatternPart|)
  ((:|pattern| t |Pattern|)))

(defmethod build-incremental-map-pattern ((first |MapPatternRest|) rest)
  (check-type rest null)
  (e. first |getPattern|))

(defmethod build-incremental-map-pattern ((first null) rest)
  (check-type rest null)
  (let ((map-var (gennoun "map")))
    (mn '|SuchThatPattern|
      (mn '|FinalPattern| map-var nil)
      ;; NOTE: I once had this using CompareExpr instead of SameExpr, which seems more sensible, but it turned out to be a cyclic dependency: __comparer uses DeepFrozen uses map-patterns uses __comparer.
      (mn '|SameExpr| (mn '|CallExpr| map-var "size") 
                      (mn '|LiteralExpr| 0)
                      +e-false+))))

(defun expand-map-pattern (pairs)
  (build-incremental-map-pattern (first pairs) (rest pairs)))

(defemacro |MapPattern| (|Pattern|) ((|pairs| t (e-list |MapPatternPart|)))
                                    (:rest-slot t)
  (expand-map-pattern (coerce |pairs| 'list)))



(defemacro |QuasiPattern| (|Pattern|)
    ((|optParser| t (or null |EExpr|))
     (|parts| t (e-list (and |QuasiPart|))))
    (:rest-slot t)
  ;; XXX todo: reject usage between (all $-holes) and (all @-holes)
  (let ((context-noun (gennoun "co"))
        (specimen-noun (gennoun "sp"))
        (ejector-noun (gennoun "ej")))
    ;; NOTE: once we have the generalized guard change we can put the pair pattern directly in the SuchThatPattern
    (mn '|SuchThatPattern|
      (mn '|FinalPattern| context-noun (mn '|NounExpr| "__MatchContext"))
      (mn '|SeqExpr|
        (mn '|DefineExpr| (mn '|ListPattern| (mn '|FinalPattern| specimen-noun nil)
                                             (mn '|FinalPattern| ejector-noun nil)) 
                          context-noun nil)
        (mn '|MatchBindExpr|
          (mn '|CallExpr|
            (mn '|CallExpr| (quasi-deopt-parser |optParser|)
                            "matchMaker"
                            (quasi-description |parts|))
            "matchBind"
            (quasi-value-list |parts|)
            specimen-noun
            ejector-noun)
          (quasi-subpattern-list |parts|))))))

(defemacro |SamePattern| (|Pattern|) ((|value| t |EExpr|)
                                      (|invert| nil e-boolean))
                                     ()
  (let ((temp (gennoun "")))
    (mn '|SuchThatPattern| (mn '|FinalPattern| temp nil) 
                           (mn '|SameExpr| temp |value| |invert|))))

(defemacro |TailPattern| (|Pattern|) ((|fixedPatt| t |Pattern|)
                                      (|tailPatt| t |Pattern|))
                                     ()
  ;; XXX should probably expand the fixedPatt until it becomes something we handle
  (etypecase |fixedPatt|
    (|ListPattern|
     (mn '|CdrPattern| |fixedPatt| |tailPatt|))
    (|MapPattern|
     (expand-map-pattern (nconc (coerce (e. |fixedPatt| |getPairs|) 'list)
                                (list (mn '|MapPatternRest| |tailPatt|)))))))

(defemacro |MethodObject| (|ObjectTail|) ((|parent| t (or null |EExpr|))
                                          (|auditors| t (e-list |EExpr|))
                                          (|methods| t (e-list |EMethodoid|))
                                          (|matchers| t (e-list |EMatcher|)))
                                         ()
  (vector |parent| |auditors| (mn '|EScript| |methods| |matchers|)))

(defemacro |FunctionObject| (|ObjectTail|) 
    ((|patterns| t (e-list |Pattern|))
     (|optResultGuard| t (or null |EExpr|))
     (|auditors| t (e-list |EExpr|))
     (|body| t |EExpr|)
     (|isEasyReturn| nil e-boolean))
    (&whole node)
  (declare (ignore |patterns| |optResultGuard| |auditors| |body| |isEasyReturn|))
  (expand-function-object node ""))

(def-vtable |FunctionObject|
  (:|withFunctionDocumentation| (this doc)
    (e-coercef doc '(or null string))
    (expand-function-object this doc)))

(defun expand-function-object (node doc)
  (destructuring-bind
      (|patterns| |optResultGuard| |auditors| |body| |isEasyReturn|)
      (node-elements node)
    (let ((kernel-patterns (e-macroexpand-all |patterns|))
          (kernel-guard (e-macroexpand-all |optResultGuard|))
          (kernel-auditors (e-macroexpand-all |auditors|)))
      (reject-definition-usage node nil nil :|patterns| :|auditors|)
      (reject-definition-usage node nil t   :|auditors| :|patterns|)
      (reject-definition-usage node nil nil :|optResultGuard| :|auditors|)
      (reject-definition-usage node nil t   :|auditors| :|optResultGuard|)
      (mn '|MethodObject|
        nil
        kernel-auditors
        (vector (mn '|ETo| doc "run" kernel-patterns kernel-guard |body| |isEasyReturn|))
        #()))))

(defemacro |PlumbingObject| (|ObjectTail|) ((|auditors| t (e-list |EExpr|))
                                            (|matcher| t |EMatcher|))
                                           ()
  (vector nil |auditors| (mn '|EScript| nil (vector |matcher|))))

(defemacro |ETo| (|EMethodoid|) 
    ((|docComment| nil string)
     (|verb| nil string)
     (|patterns| t (e-list |Pattern|))
     (|optResultGuard| t (or null |EExpr|))
     (|body| t |EExpr|)
     (|isEasyReturn| nil e-boolean))
    ()
  (mn '|EMethod| |docComment| |verb| |patterns| 
                 (or |optResultGuard|
                   (if (e-is-true |isEasyReturn|)
                     (mn '|NounExpr| "any")
                     (mn '|NounExpr| "void")))
                 (mn '|EscapeExpr| 
                   (mn '|FinalPattern| (mn '|NounExpr| "__return") nil) 
                   (if (e-is-true |isEasyReturn|)
                     (mn '|SeqExpr| |body| (mn '|NullExpr|))
                     |body|)
                   nil nil)))

;;; --- Quasi-* nodes and support ---

(define-node-class |QuasiPart| (|ENode|) ())

(define-node-class |QuasiText| (|QuasiPart|)
  ((:|text| nil string)))

(define-node-class |QuasiExprHole| (|QuasiPart|)
  ((:|expr| t |EExpr|)))

(define-node-class |QuasiPatternHole| (|QuasiPart|)
  ((:|pattern| t |Pattern|)))

(define-node-class |QuasiEscape| (|QuasiPart|)
  ((:|char| nil character)))


(defun quasi-deopt-parser (opt-parser)
 (or opt-parser (mn '|QuasiParserExpr| "simple")))

(defun quasi-description (parts)
  (mn '|LiteralExpr|
    (with-output-to-string (s)
      (let ((value-index 0) (pattern-index 0))
        (map nil (lambda (part &aux text)
                   (setf (values text value-index pattern-index)
                     (quasi-part-description part value-index pattern-index))
                   (write-string text s))
             parts)))))

(defgeneric quasi-part-description (part value-index pattern-index))

(defmethod quasi-part-description ((part |QuasiText|) value-index pattern-index)
  ;; xxx icky/inefficient
  (values (e. (e. (e. part |getText|) |replaceAll| "$" "$$") |replaceAll| "@" "@@") 
          value-index
          pattern-index))

(defmethod quasi-part-description ((part |QuasiEscape|) value-index pattern-index)
  (values (let ((char (e. part |getChar|)))
            (case char
              ((#\@ #\$) (make-array 2 :element-type (type-of char) :initial-element char))
              (t (string char))))
          pattern-index))

(defmethod quasi-part-description ((part |QuasiExprHole|) value-index pattern-index)
  (values (format nil "${~A}" value-index)
          (1+ value-index)
          pattern-index))

(defmethod quasi-part-description ((part |QuasiPatternHole|) value-index pattern-index)
  (values (format nil "@{~A}" pattern-index)
          value-index
          (1+ pattern-index)))

(defun quasi-value-list (parts)
  (apply #'mn '|ListExpr| (loop for part in (coerce parts 'list)
                                when (typep part '|QuasiExprHole|)
                                  collect (e. part |getExpr|))))

(defun quasi-subpattern-list (parts)
  (apply #'mn '|ListPattern| (loop for part in (coerce parts 'list)
                                   when (typep part '|QuasiPatternHole|)
                                     collect (e. part |getPattern|))))


;; XXX general AssignExpr (assigning to runs and gets and unary-stars)
