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
    :|ListExpr|
    :|MismatchExpr|
    :|NKObjectExpr|
    :|NullExpr|
    :|PrefixExpr|
    :|RangeExpr|
    :|ReturnExpr|
    :|SameExpr|
    :|SendExpr|
    :|ThunkExpr|
    :|UpdateExpr|
    :|URIExpr|
    :|URISchemeExpr|
    :|TailPattern|
    :|BindPattern|
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
    
(defun reify-temporaries (node &optional (names (e. (coerce (find-names node) 'vector) |asMap|))
                                         (index-cell (cons 1 nil)))
  "Replace all TemporaryExprs in the node tree with NounExprs."
  (etypecase node
    (null nil)
    (vector (map 'vector (lambda (x) (reify-temporaries x names index-cell)) node))
    (|TemporaryExpr| 
      (mn '|NounExpr| 
        (loop with label = (symbol-name (e. node |getName|))
              for name = (format nil "~A__~A" label (car index-cell))
              while (e-is-true (e. names |maps| name))
              do (incf (car index-cell))
              finally (return name))))
    (|ENode| 
      ;; XXX duplicated code
      (let ((maker (get (class-name (class-of node)) 'static-maker)))
        (e-call maker "run"
          ((lambda (args) `(nil ,@args nil))
            (loop for subnode-flag across (e. maker |getParameterSubnodeFlags|)
                  for sub in (elang::node-visitor-arguments node)
                  collect (if (e-is-true subnode-flag)
                            (reify-temporaries sub names index-cell)
                            sub))))))))

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
  (make-instance '|DefrecExpr| :elements (list 
    (make-instance '|ListPattern| :elements (list 
      (make-instance '|FinalPattern| :elements (list |promiseNoun| nil))
      (make-instance '|FinalPattern| :elements (list |resolverNoun| nil))))
    (load-time-value (make-instance '|CallExpr| :elements (list (make-instance '|NounExpr| :elements '("Ref")) "promise")))
    (make-instance '|NounExpr| :elements '("null")))))


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
      (let* ((result-noun (gennoun "result"))
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
        
(defemacro |ListExpr| (|EExpr|) ((|subs| t (e-list |EExpr|))) (:rest-slot t)
  (apply #'mn '|CallExpr|
      (load-time-value (mn '|NounExpr| "__makeList"))
      "run"
      (coerce |subs| 'list)))

(defemacro |MismatchExpr| (|EExpr|) ((|specimen| t |EExpr|)
                                     (|pattern| t |Pattern|))
                                    ()
  (mn '|CallExpr| (mn '|MatchBindExpr| |specimen| |pattern|) "not"))

(defemacro |NKObjectExpr| (|EExpr|) ((|docComment| nil string)
                                     (|name| nil (or |Pattern| string))
                                     (|parent| t (or null |EExpr|))
                                     (|auditors| t (e-list |EExpr|))
                                     (|script| t |EScript|))
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
    (string
      (if |parent|
        (mn '|SeqExpr|
          (mn '|DefrecExpr| (mn '|FinalPattern| (mn '|NounExpr| "super") nil)
                            |parent|
                            nil)
          (mn '|NKObjectExpr| |docComment| |name| nil |auditors|
                              (mn '|EScript|
                                (e. |script| |getOptMethods|)
                                (e. (e. |script| |getMatchers|) |with|
                                  (mn '|EMatcher|
                                    (mn '|FinalPattern| (mn '|NounExpr| "msg") nil)
                                    (mn '|CallExpr| (mn '|NounExpr| "E")
                                                    "callWithPair"
                                                    (mn '|NounExpr| "super")
                                                    (mn '|NounExpr| "msg")))))))
        (mn '|ObjectExpr| |docComment| |name| |auditors| |script|)))))

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

(defemacro |ReturnExpr| (|EExpr|) ((|value| t |EExpr|)) ()
  (mn '|CallExpr| (mn '|NounExpr| "__return") "run" |value|))

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
                                ()
  (mn '|CallExpr|
    (mn '|NounExpr| "E")
    "send"
    (make-instance '|CallExpr| :elements (list* (mn '|NounExpr| "__makeList") "run" |recipient| (mn '|LiteralExpr| |verb|) (coerce |args| 'list)))))

(defemacro |ThunkExpr| (|EExpr|) ((|docComment| nil string)
                                  (|body| t |EExpr|))
                                 ()
  (mn '|ObjectExpr|
      |docComment|
      "_"
      #()
      (mn '|EScript| (vector (mn '|EMethod| "" "run" #() nil |body|)) 
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

(defemacro |TailPattern| (|Pattern|) ((|fixedPatt| t |Pattern|)
                                      (|tailPatt| t |Pattern|))
                                     ()
  (mn '|CdrPattern| |fixedPatt| |tailPatt|))

(defemacro |ETo| (elang::|EMethodoid|) 
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
