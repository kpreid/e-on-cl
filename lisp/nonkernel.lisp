; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.nonkernel
  (:use)
  (:export
    :|BinaryExpr|
    :|CoerceExpr|
    :|CompareExpr|
    :|ConditionalExpr|
    :|DefrecExpr|
    :|ForwardExpr|
    :|If1Expr|
    :|ListExpr|
    :|MapExpr|
    :|MismatchExpr|
    :|NKObjectExpr|
    :|NullExpr|
    :|PrefixExpr|
    :|RangeExpr|
    :|ReturnExpr|
    :|SameExpr|
    :|SendExpr|
    :|SwitchExpr|
    :|ThunkExpr|
    :|UpdateExpr|
    :|URIExpr|
    :|URISchemeExpr|
    :|BindPattern|
    :|SamePattern|
    :|TailPattern|
    :|FunctionScript|
    :|ETo|))

(cl:defpackage :e.nonkernel.impl
  (:use :cl :e.util :e.elib :e.elang :e.elang.vm-node :e.nonkernel)
  (:export :kernelize))

(cl:in-package :e.nonkernel.impl)

;;; --- Temporary name support ---

(define-node-class |TemporaryExpr| (|EExpr|)
  ((:|name| nil symbol)))
 
;; XXX make this less internal      
(e.elang::def-scope-rule |TemporaryExpr|
  (e.elang::! (e. e.elang:+the-make-static-scope+ |scopeRead| e.elang::node)))


(defun gennoun (label)
  "Return a temporary name expr which will be substituted to a concrete NounExpr with an unused name after expansion."
  (mn '|TemporaryExpr| (make-symbol label)))

(defun find-names (node)
  "Return a list, possibly containing duplicates, of the 'name' of every NounExpr in the tree."
  (etypecase node
    (null nil)
    (vector (loop for sub across node append (find-names sub)))
    (|NounExpr| (elang::node-elements node))
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

(defun e-macroexpand (node)
  (loop (setf node (e-macroexpand-1 node (lambda () (return node))))))

(defgeneric e-macroexpand-all (node))

(defmethod e-macroexpand-all ((node null))
  nil)

(defmethod e-macroexpand-all ((node vector))
  (map 'vector #'e-macroexpand-all node))
(defmethod e-macroexpand-all ((node string))
  (error "shouldn't happen: string encountered in node macroexpansion: ~S" node))

(defmethod e-macroexpand-all ((node |ENode|))
  (let* ((node (e-macroexpand node))
         (maker (get (class-name (class-of node)) 'elang::static-maker))
         (new-node-args (loop for subnode-flag across (e. maker |getParameterSubnodeFlags|)
                         for sub in (elang::node-visitor-arguments node)
                         collect (if (e-is-true subnode-flag)
                                   (e-macroexpand-all sub)
                                   sub))))
    (e-call maker "run" `(nil ,@new-node-args nil))))

;;; --- conveniences ---

(defun mn (name &rest elements)
  (make-instance name :elements elements))

(defmacro defemacro (node-class-name (&rest superclasses) (&rest properties) (&rest options) &body body
    &aux (node (gensym "NODE")) (stop (gensym "STOP")))
  `(progn
     (e.elang::define-node-class ,node-class-name ,superclasses
       ,(loop for (name subnode type) in properties
              collect (list (intern (symbol-name name) :keyword) subnode type))
       ,@options)
     (defmethod e-macroexpand-1 ((,node ,node-class-name) ,stop)
       (declare (ignore ,stop))
       (destructuring-bind ,(let ((vars (mapcar #'first properties)))
                              (if (getf options :rest-slot)
                                `(,@(butlast vars) &rest ,@(last vars))
                                vars))
                           (e.elang::node-elements ,node)
         ,@body))))

(defun noun-to-resolver-noun (noun)
  (make-instance '|NounExpr| :elements (list (concatenate 'string (e. noun |getName|) "__Resolver"))))

;;; --- ---

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
        (:+ "add")
        (:- "subtract")
        (:* "multiply")
        (:** "pow")
        (:/ "approxDivide")
        (:// "floorDivide")
        (:% "mod")
        (:%% "remainder")
        (:& "and")
        (:\| "or")
        (otherwise |op|))
      (coerce |rest| 'list))))

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
        (:<= "ltEq")
        (:> "greaterThan")
        (:>= "gtEq")
        (otherwise |op|))
      |left|
      |right|))

(defun conditional-op-p (x) (member x '("&&" "||") :test #'equal))

(defemacro |ConditionalExpr| (|EExpr|) ((|op| nil (satisfies conditional-op-p))
                                        (|left| t |EExpr|)
                                        (|right| t |EExpr|))
                                       ()
  ;; XXX handle bindings
  (ecase (find-symbol |op| :keyword)
    (:&&   (mn '|IfExpr| |left| |right| (mn '|NounExpr| "false")))
    (:\|\| (mn '|IfExpr| |left| (mn '|NounExpr| "true") |right|))))

(defemacro |PromiseVarExpr| (|EExpr|) ((|promiseNoun| t |EExpr|)
                                       (|resolverNoun| t |EExpr|))
                                      ()
  (mn '|DefrecExpr|
    (mn '|ListPattern|
      (mn '|FinalPattern| |promiseNoun| nil)
      (mn '|FinalPattern| |resolverNoun| nil))
    (load-time-value (mn '|CallExpr| (mn '|NounExpr| "Ref") "promise"))
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

(defemacro |ForwardExpr| (|EExpr|) ((|noun| nil |NounExpr|)) ()
  (let* ((resolver-noun (noun-to-resolver-noun |noun|)))
      (make-instance '|SeqExpr| :elements (list
        (mn '|PromiseVarExpr| |noun| resolver-noun)
        resolver-noun))))

(defemacro |If1Expr| (|EExpr|) ((|test| t |EExpr|) 
                                (|then| t |EExpr|))
                               ()
  (mn '|IfExpr| |test| |then| (mn '|NullExpr|)))
            
(defemacro |ListExpr| (|EExpr|) ((|subs| t (e-list |EExpr|))) (:rest-slot t)
  (apply #'mn '|CallExpr|
      (load-time-value (mn '|NounExpr| "__makeList"))
      "run"
      (coerce |subs| 'list)))

(defemacro |MapExpr| (|EExpr|) ((|pairs| t (e-list |EExpr|)))
                               (:rest-slot t)
  (mn '|CallExpr| (mn '|NounExpr| "__makeMap") 
                  "fromPairs" 
                  (apply #'mn '|ListExpr| (coerce |pairs| 'list))))

(defemacro |MismatchExpr| (|EExpr|) ((|specimen| t |EExpr|)
                                     (|pattern| t |Pattern|))
                                    ()
  (mn '|CallExpr| (mn '|MatchBindExpr| |specimen| |pattern|) "not"))

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
          ;; XXX remove the __C once we're out of the compatibility phase
          (format nil "$~A__C" (or (e.elang::pattern-opt-noun |name|) "_"))
          |parent|
          |auditors|
          |script|) 
        nil))
    ((or nil string)
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
  (load-time-value (mn '|NounExpr| "null")))

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

(defemacro |RangeExpr| (|EExpr|) ((|start| t |EExpr|)
                                  (|end| t |EExpr|)
                                  (|thru| nil e-boolean))
                                 ()
  (mn '|CallExpr|
      (mn '|NounExpr| "__makeOrderedSpace")
      (if (e-is-true |thru|) "op__thru" "op__till")
      |start|
      |end|))

(defemacro |ReturnExpr| (|EExpr|) ((|value| t (or null |EExpr|))) ()
  (apply #'mn '|CallExpr| (mn '|NounExpr| "__return") 
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
  (let ((exp (e-macroexpand |call|)))
    (check-type exp |CallExpr|)
    (mn '|AssignExpr| (e. exp |getRecipient|) exp)))

(defemacro |URIExpr| (|EExpr|) ((|uri| nil string))
                               ()
  (let ((sep (position #\: |uri|)))
    (mn '|CallExpr|
        (mn '|URISchemeExpr| (subseq |uri| 0 sep))
        "get"
        (mn '|LiteralExpr| (subseq |uri| (1+ sep))))))

(defemacro |URISchemeExpr| (|EExpr|) ((|scheme| nil string))
                                     ()
  (mn '|NounExpr| (format nil "~A__uriGetter" |scheme|)))

(defemacro |BindPattern| (|Pattern|) ((|noun| t |NounExpr|)
                                      (|optGuard| t (or null |EExpr|)))
                                     ()
  (let ((temp (gennoun (e. |noun| |getName|))))
    (make-instance '|SuchThatPattern| :elements (list
      (make-instance '|FinalPattern| :elements (list
        temp
        |optGuard|))
      (make-instance '|SeqExpr| :elements (list
        (make-instance '|CallExpr| :elements (list
          (noun-to-resolver-noun |noun|)
          "resolve"
          temp))
        (make-instance '|NounExpr| :elements '("true"))))))))

(defemacro |SamePattern| (|Pattern|) ((|value| t |EExpr|)
                                      (|invert| nil e-boolean))
                                     ()
  (let ((temp (gennoun "")))
    (mn '|SuchThatPattern| (mn '|FinalPattern| temp nil) 
                           (mn '|SameExpr| temp |value| |invert|))))

(defemacro |TailPattern| (|Pattern|) ((|fixedPatt| t |Pattern|)
                                      (|tailPatt| t |Pattern|))
                                     ()
  ;; will be less trivial once we support map patterns
  (mn '|CdrPattern| |fixedPatt| |tailPatt|))

(defemacro |FunctionScript| (|EScriptoid|) 
    ((|patterns| t (e-list |Pattern|))
     (|optResultGuard| t (or null |EExpr|))
     (|body| t |EExpr|))
    ()
  (mn '|EScript| 
    (vector (mn '|ETo| "" "run" |patterns| |optResultGuard| |body|))
    #()))

(defemacro |ETo| (|EMethodoid|) 
    ((|docComment| nil string)
     (|verb| nil string)
     (|patterns| t (e-list |Pattern|))
     (|optResultGuard| t (or null |EExpr|))
     (|body| t |EExpr|))
    ()
  (mn '|EMethod| |docComment| |verb| |patterns| |optResultGuard| 
                 (mn '|EscapeExpr| 
                   (mn '|FinalPattern| (mn '|NounExpr| "__return") nil) 
                   |body| 
                   nil nil)))
  
;; XXX general AssignExpr (assigning to runs and gets and unary-stars)
