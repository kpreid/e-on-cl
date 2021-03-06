; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :e.nonkernel)) ;; XXX remove once nonkernel package is defined before syntax package

; --- ---

(defun is-identifier (text)
  #-e.syntax::local-parser
  (query-to-java "isIdentifier" text)
  #+e.syntax::local-parser
  ;; XXX inadequate stub originally written to get the local-parser mode working
  (or (string= text "")
      (not (digit-char-p (aref text 0)))))

; --- Printing ---

(defun print-escaped (tw chars specials
    &aux (buf (make-array 1024 :fill-pointer 0 :element-type 'character)))
  (labels ((out (new)
             (when (> (length new) (- (array-dimension buf 0) (fill-pointer buf)))
               (e. tw |write| (copy-seq buf))
               (setf (fill-pointer buf) 0))
             (incf (fill-pointer buf) (length new))
             (replace buf new :start1 (- (fill-pointer buf) (length new)))))
    (loop for char across chars do
      (case char
        ((#\Tab) (out "\\t"))
        ((#\Return) (out "\\r"))
        ((#\Linefeed) (out (list char)))
        ((#\\) (out "\\\\"))
        (otherwise
          (cond
            ((find char specials)
              (out (list #\\ char)))
            ((< (char-code char) (char-code #\Space))
              ; XXX should include some Unicode specials in the near-FFFF range, too
              (out (format nil "\\u~4,'0D" (char-code char)))) ; xxx Unicode/ASCII assumption
            (t
              (out (list char)))))))
    (e. tw |write| (copy-seq buf))))

(defconstant +precedence-atom+ 0)
(defconstant +precedence-in-guard+ +precedence-atom+)
(defconstant +precedence-in-suchthat+ +precedence-atom+)

(defconstant +precedence-call-rec+ 10)

(defconstant +precedence-in-slot-expr+ 29)
(defconstant +precedence-slot-expr+ 30) ; &foo.bar() parses as &(foo.bar())

(defconstant +precedence-matchbind-left+ 49) ; XXX or should this be 50/51?
(defconstant +precedence-matchbind+ 50)

(defconstant +precedence-in-assign-right+ 60)
(defconstant +precedence-assign+ 60)

(defconstant +precedence-define-right+ 60)
(defconstant +precedence-define+ 60)

(defconstant +precedence-into-left+ 70)
(defconstant +precedence-into+ 70)

(defconstant +precedence-in-seq+ 80)
(defconstant +precedence-seq+ 80)

(defconstant +precedence-outer+ 100)

(defconstant +precedence-in-assign-left+ nil)


; xxx stomping on erights.org namespace
(defobject +e-printer+ "org.erights.e.syntax.ePrinter"
    (:doc "Centralized object for generating E source and E-like text."
     :stamped +deep-frozen-stamp+)
  
  (:|__printOn| ((tw +the-text-writer-guard+))
    (e. tw |print| "<E-syntax printer>")
    nil)
  
  (:|printDocComment| ((tw +the-text-writer-guard+) (text 'doc-comment))
    ; XXX have a strictness (about */) argument
    "Print a \"/** */\" documentation comment with a trailing line break on 'tw', if 'text' is not an empty string."
    (when text
      (when (search "*/" text)
        ; xxx is there an escaping mechanism we can use?
        ; using print-not-readable is slightly wrong as this isn't a *lisp* printing problem
        (error "doc comment containing \"*/\" cannot be printed: ~A" (e-quote text)))
      (e. tw |print| "/** " text " */")
      (e. tw |println|))
    nil)
  
  (:|printVerb| ((tw +the-text-writer-guard+) (verb 'string))
    "Print a verb with appropriate quoting, as in CallExpr or EMethod."
    (if (is-identifier verb)
      (e. tw |print| verb)
      (e. tw |quote| verb))
    nil)
   
  (:|printNoun| ((tw +the-text-writer-guard+) (noun 'string))
    "Print a noun with appropriate quoting, as in NounExpr."
    (if (is-identifier noun)
      (e. tw |print| noun)
      (progn
        (e. tw |print| "::")
        (e. tw |quote| noun)))
    nil)
  
  (:|printPropertySlot| ((tw +the-text-writer-guard+) (prop-name 'string))
    "The print representation of a property-slot object. Not parsable."
    (e. tw |print| "_::&")
    (if (is-identifier prop-name)
      (e. tw |write| prop-name)
      (e. tw |quote| prop-name)))
  
  (:|printString| ((tw +the-text-writer-guard+) (this 'string))
    (e. tw |write| "\"")
    (print-escaped tw this "\"")
    (e. tw |write| "\""))
  
  (:|printCons| ((tw +the-text-writer-guard+) (this 'cons))
    "The print representation of a Lisp cons. Not parsable."
    (e. tw |write| "<(")
    (e. tw |quote| (car this))
    (loop for x = (ref-shorten (cdr this)) then (ref-shorten (cdr x))
          while (consp x)
          do (e. tw |write| ", ")
             (e. tw |quote| (car x))
          finally
            (when x
              (e. tw |write| " . ")
              (e. tw |quote| x)))
    (e. tw |write| ")>"))
  
  (:|printCharacter| ((tw +the-text-writer-guard+) (this 'character))
    "The print representation of a character."
    (e. tw |write| "'")
    (print-escaped tw (vector this) "\'")
    (e. tw |write| "'"))
  
  (:|printList| ((tw +the-text-writer-guard+) (this 'vector) (quote-elements 'e-boolean))
    (if (e-is-true quote-elements)
      (e. this |printOn| "[" ", " "]" tw)
      (progn
        (e. tw |print| "[" #|]|#)
        (loop for sep = "" then ", "
              for element across this
              do (e. tw |print| sep element))
        (e. tw |print| #|[|# "]"))))
  
  (:|printMethodHeader| ((tw +the-text-writer-guard+) (is-kernel 'e-boolean) doc-comment verb (params 'vector) opt-result-guard)
    "Print a to/method as in an EScript or interface sugar."
    ; XXX should we have quoting options for params and opt-result-guard?
    ; coercion of doc-comment, verb handled elsewhere

    (e. +e-printer+ |printDocComment| tw doc-comment)
    (e. tw |print| (if (e-is-true is-kernel) "method " "to "))
    (e. +e-printer+ |printVerb| tw verb)
    (e. tw |print| "(" #|)|#)
    (loop for sep = "" then ", "
          for element across params
          do (e. tw |print| sep element))
    (e. tw |print| #|(|# ")")
    (when (ref-shorten opt-result-guard)
      (e. tw |print| " :")
      (e. tw |print| opt-result-guard)))
  
  (:|printGuardedNounPattern| ((tw +the-text-writer-guard+) (opt-name '(or null string)) opt-guard)
    (if opt-name
      (e. +e-printer+ |printNoun| tw opt-name)
      (e. tw |print| "_"))
    (when (ref-shorten opt-guard)
      (e. tw |print| " :")
      (e. tw |print| opt-guard)))
  
  (:|printExprBlock| (tw node)
    "Print the EExpr 'node' on 'tw' enclosed in braces."
    (e. tw |print| "{" #|}|#)
    (let ((tw2 (e. tw |indent|)))
      (e. tw2 |println|)
      (e. node |welcome| (e. +e-printer+ |makePrintENodeVisitor| tw2 +precedence-outer+)))
    (e. tw |lnPrint| #|{|# "}"))
  
  (:|makePrintENodeVisitor| (tw)
    (e. +e-printer+ |makePrintENodeVisitor| tw +precedence-outer+))
  
  (:|makePrintENodeVisitor| ((tw +the-text-writer-guard+)
                             (precedence '(or null integer)))
    "Return an ETreeVisitor which prints nodes to 'tw' in standard E syntax. 'precedence' controls whether the expression is parenthesized.

XXX make precedence values available as constants"
    ; XXX have an in-quasi argument - print the LiteralExpr "$" as "$$", and the ${n} as ${n}
    ; XXX have a self argument for implementation inheritance
    ; strictness controls?
    ; XXX tests for the visitor's Ref-transparency
    (macrolet ((precedential ((this-min) &body body)
                 `(progn
                    (assert precedence (precedence) "This subnode cares about precedence")
                    (if (< precedence ,this-min)
                      (progn
                        (e. tw |print| "(" #|)|#)
                        ,@body
                        (e. tw |print| #|(|# ")"))
                      (progn
                        ,@body)))))
      (labels  ((subprint (node our-precedence &key (tw tw))
                  (e. node |welcome| (e. +e-printer+ |makePrintENodeVisitor| tw our-precedence)))
                (subprint-block (expr)
                  ; XXX call back to self once we have a self to preserve
                  (e. +e-printer+ |printExprBlock| tw expr))
                (subprint-guard (expr &key (tw tw))
                  "special rule for guard positions: NounExpr, GetExpr, or parenthesized"
                  ;; XXX handle GetExpr (will be recursive)
                  (if (typep (ref-shorten expr) '|NounExpr|)
                    (subprint expr +precedence-atom+)
                    (progn
                      (e. tw |write| "(" #|)|#)
                      (subprint expr +precedence-outer+)
                      (e. tw |write| #|(|# ")"))))
                (noun-pattern-printer (tag)
                  (lambda (opt-original noun-expr &optional opt-guard-expr)
                    (declare (ignore opt-original))
                    (e. tw |print| tag)
                    (subprint noun-expr nil)
                    (when (ref-shorten opt-guard-expr)
                      (e. tw |print| " :")
                      (subprint-guard opt-guard-expr))))
                (quasi-printer (tag)
                  (lambda (opt-original index)
                    (declare (ignore opt-original))
                    (e-coercef index 'integer)
                    (e. tw |print| tag "{" index "}"))))
        (e-lambda |printVisitor| ()
          (:|__printOn| ((ptw +the-text-writer-guard+))
            (e. ptw |print| "<E-syntax node visitor printing to " tw ">")
            nil)
          
          (:|visitAssignExpr| (opt-original noun-expr value-expr)
            (declare (ignore opt-original))
            (precedential (+precedence-assign+)
              (subprint noun-expr +precedence-in-assign-left+)
              (e. tw |print| " := ")
              (subprint value-expr +precedence-in-assign-right+)))
          
          (:|visitNKAssignExpr| (oo n v)
            (e. |printVisitor| |visitAssignExpr| oo n v))

          (:|visitBinaryExpr| (opt-original left-expr operator right-exprs)
            (declare (ignore opt-original))
            (precedential (+precedence-outer+)
              (subprint left-expr +precedence-atom+) ;; XXX wrong precedence
              (e. tw |write| " ")
              (e. tw |print| operator)
              (e. tw |write| " ")
              (if (samep 1 (e. right-exprs |size|))
                (subprint (eelt right-exprs 0) +precedence-atom+) ;; XXX wrong precedence
                (progn
                  (e. tw |print| "(" #|)|#)
                  (loop for sep = "" then ", "
                        for arg across (e-coerce right-exprs 'vector)
                        do (e. tw |write| sep)
                           (subprint arg +precedence-outer+))
                  (e. tw |print| #|(|# ")")))))
          
          (:|visitBindingExpr| (opt-original noun)
            (declare (ignore opt-original))
            (precedential (+precedence-slot-expr+)
              (e. tw |print| "&&")
              (subprint noun +precedence-in-slot-expr+)))
          
          (:|visitCallExpr| (opt-original rec verb args)
            (declare (ignore opt-original))
            (subprint rec +precedence-call-rec+)
            (e. tw |print| ".")
            (e. +e-printer+ |printVerb| tw verb)
            (e. tw |print| "(" #|)|#)
            (loop for sep = "" then ", "
                  for arg across (e-coerce args 'vector)
                  do (e. tw |print| sep)
                     (subprint arg +precedence-outer+))
            (e. tw |print| #|(|# ")"))
          
          (:|visitCatchExpr| (opt-original attempt catch-pattern catch-body)
            (declare (ignore opt-original))
            ; XXX don't call back to the node for printing
            (e. tw |print| "try ")
            (subprint-block attempt)
            (e. tw |print| " catch ")
            (subprint catch-pattern nil)
            (e. tw |print| " ")
            (subprint-block catch-body))
          
          (:|visitDefineExpr| (opt-original pattern opt-ejector specimen)
            (declare (ignore opt-original))
            (precedential (+precedence-define+)
              (e. tw |print| "def ")
              (subprint pattern nil)
              (when (ref-shorten opt-ejector)
                (e. tw |print| " exit ")
                ;; XXX not quite right precedence
                (subprint opt-ejector +precedence-atom+))
              (e. tw |print| " := ")
              (subprint specimen +precedence-define-right+)))
          
          (:|visitDefrecExpr| (oo p e s)
            (e. |printVisitor| |visitDefineExpr| oo p e s))
          
          (:|visitEscapeExpr| (opt-original ejector-patt body catch-patt catch-body)
            (declare (ignore opt-original))
            (e. tw |print| "escape ")
            (subprint ejector-patt nil)
            (e. tw |print| " ")
            (subprint-block body)
            (when (ref-shorten catch-patt)
              (e. tw |print| " catch ")
              (subprint catch-patt nil)
              (e. tw |print| " ")
              (subprint-block catch-body)))
          
          (:|visitFinallyExpr| (opt-original attempt unwinder)
            (declare (ignore opt-original))
            ; XXX don't call back to the node for printing
            (e. tw |print| "try ")
            (subprint-block attempt)
            (e. tw |print| " finally ")
            (subprint-block unwinder))
          
          (:|visitHideExpr| (opt-original body)
            (declare (ignore opt-original))
            (subprint-block body))
          
          (:|visitIfExpr| (opt-original cond true-block false-block)
            (declare (ignore opt-original))
            (e. tw |print| "if (" #|)|#)
            (subprint cond +precedence-outer+)
            (e. tw |print| #|(|# ") ")
            (subprint-block true-block)
            (e. tw |print| " else ")
            (subprint-block false-block))
          
          (:|visitListExpr| (opt-original args)
            (declare (ignore opt-original))
            (e. tw |print| "[" #|]|#)
            (loop for sep = "" then ", "
                  for arg across (e-coerce args 'vector)
                  do (e. tw |print| sep)
                     (subprint arg +precedence-outer+))
            (e. tw |print| #|[|# "]"))
          
          (:|visitLiteralExpr| (opt-original (value '(or string integer character float64)))
            (declare (ignore opt-original))
            (etypecase value
              (string
                (e. +e-printer+ |printString| tw value))
              (integer
                (e. tw |print| value)) ; XXX E-syntax hiding?
              (float64
                (e. tw |print| value)) ; XXX E-syntax hiding?
              (character
                (e. +e-printer+ |printCharacter| tw value))))
          
          (:|visitMetaContextExpr| (opt-original)
            (declare (ignore opt-original))
            (e. tw |print| "meta.context()"))
          
          (:|visitMetaStateExpr| (opt-original)
            (declare (ignore opt-original))
            (e. tw |print| "meta.getState()"))
          
          (:|visitMatchBindExpr| (opt-original specimen pattern)
            (declare (ignore opt-original))
            (precedential (+precedence-matchbind+)
              (subprint specimen +precedence-matchbind-left+)
              (e. tw |print| " =~ ")
              (subprint pattern nil)))
          
          (:|visitNounExpr| (opt-original noun)
            (declare (ignore opt-original))
            (e. +e-printer+ |printNoun| tw noun))
          
          (:|visitObjectExpr| (opt-original 
                               (doc-comment 'doc-comment) 
                               pattern
                               auditors
                               script)
            (declare (ignore opt-original))
            (e. +e-printer+ |printDocComment| tw doc-comment)
            (e. tw |print| "def ")
            (subprint pattern nil)
            (subprint auditors nil)
            (e. tw |print| " ")
            (subprint script nil))
          
          (:|visitSeqExpr| (opt-original subs)
            (declare (ignore opt-original))
            (precedential (+precedence-seq+)
              (loop for sep = "" then #\Newline
                    for sub across (e-coerce subs 'vector)
                    do (e. tw |print| sep)
                       (subprint sub +precedence-in-seq+))))
          
          (:|visitSlotExpr| (opt-original noun)
            (declare (ignore opt-original))
            (precedential (+precedence-slot-expr+)
              (e. tw |print| "&")
              (subprint noun +precedence-in-slot-expr+)))
          
          (:|visitAuditors| (opt-original (opt-as '(or null |EExpr|))
                                          (implements 'vector))
            (declare (ignore opt-original))
            (when opt-as
              (e. tw |write| " as ")
              (subprint opt-as nil))
            (when (plusp (length implements))
              (e. tw |write| " implements ")
              (loop for sep = "" then ", "
                    for sub across implements
                    do (e. tw |print| sep)
                       (subprint sub nil))))
          
          (:|visitEScript| (opt-original (methods 'vector) (matchers 'vector))
            (declare (ignore opt-original))
            (e. tw |print| "{" #|}|#)
            (let ((indented (e. tw |indent|)))
              (loop for method across methods do
                (e. indented |println|)
                (subprint method nil :tw indented))
            (loop for matcher across matchers do
              (e. indented |println|)
              (subprint matcher nil :tw indented)))
            (e. tw |lnPrint| #|{|# "}"))
            
          (:|visitEMethod| (opt-original
                            (doc-comment 'doc-comment)
                            verb
                            (patterns 'vector)
                            opt-result-guard
                            body)
            (declare (ignore opt-original))
            ; XXX print patterns and opt-result-guard directly
            (e. tw |println|)
            (e. +e-printer+ |printMethodHeader| tw +e-true+
              doc-comment
              verb
              (map 'vector
                (lambda (pattern)
                  (e-lambda "syntax-printer" () (:|__printOn| ((tw +the-text-writer-guard+))
                    (subprint pattern +precedence-outer+ :tw tw))))
                patterns) 
              (when (ref-shorten opt-result-guard)
                (e-lambda "syntax-printer" () (:|__printOn| ((tw +the-text-writer-guard+))
                  (subprint-guard opt-result-guard :tw tw)))))
            (e. tw |print| " ")
            (subprint-block body))
          
          (:|visitEMatcher| (opt-original pattern body)
            (declare (ignore opt-original))
            (e. tw |print| "match ")
            (subprint pattern nil)
            (e. tw |print| " ")
            (subprint-block body))
          
          (:|visitCdrPattern| (opt-original list-patt rest-patt)
            (declare (ignore opt-original))
            (subprint list-patt nil)
            (e. tw |print| " + ")
            (subprint rest-patt nil))
          
          (:|visitFinalPattern/3| (noun-pattern-printer ""))
          (:|visitBindingPattern/2| (noun-pattern-printer "&&"))
          (:|visitSlotPattern/3| (noun-pattern-printer "&"))
          (:|visitVarPattern/3| (noun-pattern-printer "var "))

          (:|visitIgnorePattern| (opt-original)
            (declare (ignore opt-original))
            (e. tw |print| "_"))
          
          (:|visitListPattern| (opt-original subpatts)
            (declare (ignore opt-original))
            (e. tw |print| "[" #|]|#)
            (loop for sep = "" then ", "
                  for patt across (e-coerce subpatts 'vector)
                  do (e. tw |print| sep)
                     (subprint patt nil))
            (e. tw |print| #|[|# "]"))
          
          ; once we have general nonkernel printing and an answer regarding visitors, this should be reenabled
          ;(:|visitSuchThatPattern| (opt-original subpatt condition)
          ;  (declare (ignore opt-original))
          ;  (subprint subpatt nil)
          ;  (e. tw |print| " ? ")
          ;  (subprint condition +precedence-in-suchthat+))
          
          (:|visitViaPattern| (opt-original function subpatt)
            (declare (ignore opt-original))
            (e. tw |print| "via (")
            (subprint function +precedence-outer+)
            (e. tw |print| ") ")
            (subprint subpatt nil))
          
          (:|visitQuasiLiteralNode/2| (quasi-printer "$"))
          (:|visitQuasiPatternNode/2| (quasi-printer "@"))
          (:|visitQuasiLiteralExpr/2| (quasi-printer "$"))
          (:|visitQuasiPatternExpr/2| (quasi-printer "@"))
          (:|visitQuasiLiteralPatt/2| (quasi-printer "$"))
          (:|visitQuasiPatternPatt/2| (quasi-printer "@"))
          
          (otherwise (mverb opt-original &rest args)
            (check-type opt-original (not null))
            (e. tw |write| "$<")
            (e. tw |print| (subseq (unmangle-verb mverb) 5))
            (loop with maker = (get (class-name (class-of opt-original)) 'static-maker)
                  for subnode-flag across (e. maker |getParameterSubnodeFlags|)
                  for sep = " " then ", "
                  for sub in args
                  do (e. tw |write| sep)
                     (if (and (e-is-true subnode-flag) (typep sub '|ENode|))
                       ;; XXX subprint doesn't handle nil/vector subnodes
                       (subprint sub +precedence-atom+)
                       (e. tw |quote| sub)))
            (e. tw |write| ">$"))))))
  
  )

; --- Parsing ---

;; if we are using the local parser, this is unused because clrune has already
;; set up the single Java process's classpath
#-e.syntax::local-parser
(defun compute-antlr-class-location ()
  ;; xxx look into making this cleaner
  (make-pathname
    :name nil
    :type nil
    :version nil
    :defaults (first (asdf:output-files (make-instance 'asdf:compile-op)
                                        (asdf:find-component
                                          (asdf:find-system
                                              :e-on-cl.antlr-parser)
                                          "e")))))

#-e.syntax::local-parser
(progn
  (defvar *parsing-lock* (make-lock "E parsing"))
  (defvar *parse-cache-hash* (make-generic-hash-table :test 'samep))
  (defvar *parser-process*))

#-e.syntax::local-parser
(defun start-parser ()
  (assert (not (boundp '*parser-process*)))
  ; XXX pipe this through common tracing architecture
  (efuncall e.knot:+sys-trace+ "starting Java-E parsing subprocess")
  (asdf:operate 'asdf:compile-op :e-on-cl.antlr-parser)
  (setf *parser-process* (run-program
    "rune"
    (list "-J-XX:ThreadStackSize=10240"
          "-cpa" (native-namestring (merge-pathnames #P"jlib/" (asdf:component-pathname +the-asdf-system+)))
          "-cpa" (namestring (compute-antlr-class-location))
          "-De.onErrorExit=report"
          (format nil "-Dfile.encoding=~A" e.extern:+standard-external-format-common-name+)
          "-")
    :search t
    :input :stream
    :output :stream
    :error t
    :wait nil))
  (format (external-process-input-stream *parser-process*)
          "def parseEToSExpression := <import:parseEToSExpression>(<unsafe>)~%")
  (values))

#-e.syntax::local-parser
(defun kill-parser ()
  (when (boundp '*parser-process*)
    (close (external-process-input-stream *parser-process*))
    (close (external-process-output-stream *parser-process*))
    (makunbound '*parser-process*)))

#-e.syntax::local-parser
(defun ensure-parser ()
  (unless (boundp '*parser-process*)
    (start-parser)))


(defun sub-save-flush ()
  "Public, but not exported"
  #-e.syntax::local-parser
  (kill-parser)
  (values))

#-e.syntax::local-parser
(defun build-nodes (tree)
  (if (consp tree)
    (case (car tree)
      (|null| nil)
      (|.tuple.|
        (map 'vector #'build-nodes (cdr tree)))
      (|.DollarHole.|
        (make-instance '|QuasiLiteralNode| :elements
          (mapcar #'build-nodes (cdr tree))))
      (|.AtHole.|
        (make-instance '|QuasiPatternNode| :elements
          (mapcar #'build-nodes (cdr tree))))
      ((|ObjectExpr| |EMethod|)
        (make-instance (car tree) :elements
          (cons (string-trim " " (cadr tree))
                (mapcar #'build-nodes (cddr tree)))))
      (otherwise
        (make-instance (car tree) :elements
          (mapcar #'build-nodes (cdr tree)))))
    tree))

(defun starts-with (specimen prefix)
  (declare (string specimen prefix))
  (and (>= (length specimen) (length prefix))
       (string= specimen prefix :end1 (length prefix))))

(define-condition e-syntax-error (parse-error simple-error) ())

(defun error-from-e-error-string (ejector s)
  (eject-or-ethrow ejector
    (cond
      ((starts-with s "problem: ")
        (format nil "~A" (subseq s #.(length "problem: "))))
      ((starts-with s "syntax error: ")
        (make-condition 'e-syntax-error :format-control "~A"
                                        :format-arguments (list (subseq s #.(length "syntax error: ")))))
      (t
        (error "~A" s)))))

#-e.syntax::local-parser
(defvar *parse-counter* 0)

#-e.syntax::local-parser
(define-condition link-out-of-sync-error (error) ())

#-e.syntax::local-parser
(defun call-to-java (verb args &key trying-again)
  (ensure-parser)

  (format (external-process-input-stream *parser-process*)
          "println(\"~S \" + parseEToSExpression(~S, ~A)); stdout.flush()~%"
          (incf *parse-counter*)
          verb
          (e-quote (coerce args 'vector)))
  (finish-output (external-process-input-stream *parser-process*))
  
  (handler-case
      (let ((return-serial
              (read (external-process-output-stream *parser-process*)))
            (tree-expr
              (let ((*package* (find-package :e.kernel)))
                (read (external-process-output-stream *parser-process*)))))
        (unless (eql *parse-counter* return-serial)
          (error 'link-out-of-sync-error))
        tree-expr)
    ((or end-of-file link-out-of-sync-error) (condition)
      (efuncall e.knot:+sys-trace+ (format nil "error in parser communication: ~A (second time: ~A)" condition trying-again))
      (kill-parser)
      (if trying-again
        (error condition)
        (progn
          (call-to-java verb args :trying-again t))))))

#-e.syntax::local-parser
(defun query-to-java (verb &rest args)
  "Memoized version of call-to-java"
  ;; This does two things:
  ;; 1. Only objects which print parseably in E may pass this point, since the objects are passed by sending (e-print object) over a pipe.
  ;; 2. Ensures that the arguments are printable under *print-readably*, and therefore won't hose the parse cache. In particular, strings must be general strings, not base-strings.
  (labels ((convert (x) (e-coercef x '(or string e-boolean null vector integer float64 e.tables:const-map))
                        (typecase x
                          (string
                            (coerce x '(vector character)))
                          (vector
                            (map 'vector #'convert x))
                          (e.tables:const-map
                            (e. +the-make-const-map+ |fromIteratable|
                              (e-lambda nil () (:|iterate| (f)
                                (e. x |iterate|
                                  (efun (k v)
                                    (efuncall f (convert k) (convert v))))))
                              +e-true+))
                          (t x))))
    (setf args (mapcar #'convert args)))
  
  (let* ((key (list verb args)))
    (with-lock-held (*parsing-lock*)
      (multiple-value-bind (cached present-p) (hashref key *parse-cache-hash*)
        (if present-p
          cached
          (e. e.knot:+sys-trace+ |doing|
            (format nil "query-to-java: ~A(~{~A...~^, ~})"
              verb (mapcar (lambda (arg) (if (stringp arg) (subseq arg 0 (position #\Newline arg)) arg))
                           args))
            (efun ()
              (let ((answer (call-to-java verb args)))
                (setf (hashref key *parse-cache-hash*) answer)
                answer))))))))

#+(or)
(defun parse-to-expr (source)
  (query-to-java "run" source))

#+(or)
(defun e-source-to-tree (source &key syntax-ejector quasi-info)
  (build-nodes (query-or-die syntax-ejector "run" source quasi-info)))

#-e.syntax::local-parser
(defun query-or-die (ejector &rest msg)
  (let ((result (apply #'query-to-java msg)))
    (if (eql (first result) 'error)
      (error-from-e-error-string ejector (second result))
      result)))

(defun parse-to-kernel (source &rest options)
  (multiple-value-bind (node props)
      (apply #'e-source-to-tree source options)
    (values (kernelize node) props)))

(defun e-source-to-tree (source &key (syntax-ejector +the-thrower+) pattern quasi-info props)
  (let ((location
          (let ((span (e. source |getOptSpan|)))
            (when span (e. span |getUri|)))))
    ;; XXX should pass multiple twine spans into the parser
    (destructuring-bind (parse-tree new-props)
        #-e.syntax::local-parser
          (query-or-die syntax-ejector "antlrParse" location source (as-e-boolean pattern) quasi-info props)
        #+e.syntax::local-parser
          (handler-case
            (antlr-parse location source pattern nil props)
            (error (c)
              (eject-or-ethrow syntax-ejector c)))
      (values (antlr-root parse-tree location) new-props))))

(declaim (inline e-coercer))
(defun e-coercer (type-or-guard)
  (lambda (x) (e-coerce x type-or-guard)))

(defun map-e-list (function list)
  (map 'vector function (e-coerce list 'vector)))

(defun mapper-e-list (function)
  (lambda (x) (map-e-list function x)))

(defobject +prim-parser+ "org.cubik.cle.prim.parser"
    (:stamped +deep-frozen-stamp+)
  ;; XXX regularize this interface
  (:|run| (source quasi-info syntax-ejector)
    (elt (e. +prim-parser+ |parseWithProps| source (e. +the-make-const-map+ |fromPairs| #()) quasi-info syntax-ejector) 0))
  (:|parseWithProps| ((source 'twine)
                      (props 'e.tables:const-map)
                      quasi-info
                      syntax-ejector)
    (setf quasi-info (ref-shorten quasi-info))
    (when quasi-info
      ;; vector of vectors of integers ([valueHoles, patternHoles])
      (setf quasi-info
        (map-e-list (mapper-e-list (e-coercer 'integer))
                    quasi-info)))
    (coerce (multiple-value-list (e-source-to-tree source :quasi-info quasi-info :syntax-ejector syntax-ejector :props props)) 'vector))
  (:|run| ((source 'twine))
    (e-source-to-tree source))
  (:|pattern| ((source 'twine) quasi-info syntax-ejector)
    (e-source-to-tree source :quasi-info quasi-info :syntax-ejector syntax-ejector :pattern t))
  (:|isIdentifier| (noun)
    (as-e-boolean (is-identifier noun))))

;;; --- Antlr parser support ---

#+e.syntax::local-parser
(progn
  (defun jvector (element-type &rest elements)
    (jnew-array-from-array element-type (coerce elements 'vector)))
  (declaim (ftype function antlr-parse))
  (defglobal +ast-get-type+     (jmethod "antlr.collections.AST" "getType"))
  (defglobal +ast-get-text+     (jmethod "antlr.collections.AST" "getText"))
  (defglobal +ast-get-line+     (jmethod "antlr.collections.AST" "getLine"))
  (defglobal +ast-get-column+   (jmethod "antlr.collections.AST" "getColumn"))
  (defglobal +ast-first-child+  (jmethod "antlr.collections.AST" "getFirstChild"))
  (defglobal +ast-next-sibling+ (jmethod "antlr.collections.AST" "getNextSibling"))
  (defglobal +token-names+ (jfield-raw #|jfield did an incomplete translation|# "EParser" "_tokenNames"))
  (defun nodes-to-list (first-node)
    (loop for c = first-node
                then (jcall +ast-next-sibling+ c)
          while c
          collect c)))

#+e.syntax::local-parser
(let* ((c-ealexer (jconstructor "EALexer" "java.io.Reader"))
       (c-quasi-lexer (jconstructor "QuasiLexer" "antlr.LexerSharedInputState"))
       (c-string-reader (jconstructor "java.io.StringReader" "java.lang.String"))
       (get-input-state (jmethod "antlr.CharScanner" "getInputState"))
       (c-token-multi-buffer (jconstructor "antlr.TokenMultiBuffer" "[Ljava.lang.String;" #|]|# "[Lantlr.TokenStream;" #|]|#))
       (set-selector (jmethod "antlr.SwitchingLexer" "setSelector" "antlr.TokenMultiBuffer"))
       (set-filename (jmethod "antlr.CharScanner" "setFilename" "java.lang.String"))
       (c-e-parser (jconstructor "EParser" "antlr.TokenBuffer"))
       (parser-set-filename (jmethod "antlr.Parser" "setFilename" "java.lang.String"))
       (e-parser-set-pocket (jmethod "EParser" "setPocket" "antlr.Token" "java.lang.String"))
       (c-common-token (jconstructor "antlr.CommonToken" "int" "java.lang.String"))
       (get-ast (jmethod "antlr.Parser" "getAST"))
       (get-props (jmethod "EParser" "getPocket"))
       (pattern-parse-method (jmethod "EParser" "pattern"))
       (expr-parse-method (jmethod "EParser" "start")))
  (defun antlr-parse (fname text is-pattern is-quasi props)
    ;; XXX this code is stale
    (let* ((elexer (jnew c-ealexer (jnew c-string-reader text)))
           (qlexer (jnew c-quasi-lexer (jcall get-input-state elexer)))
           (tb (jnew c-token-multi-buffer
                     (jvector "java.lang.String" "e" "quasi")
                     (jvector "antlr.TokenStream" elexer qlexer))))
      (dolist (x (list elexer qlexer))
        (jcall set-selector x tb)
        (jcall set-filename x fname))
      (let ((parser (jnew c-e-parser tb)))
        (jcall parser-set-filename parser fname)
        (jcall e-parser-set-syntax parser (jnew c-common-token -1 "0.8"))
        (when props
          (e. props |iterate| (efun (k v)
            (jcall e-parser-set-pocket (jnew c-common-token -1 k) v))))
        (jcall (if is-pattern
                 pattern-parse-method
                 expr-parse-method)
               parser)
        (values (nodes-to-list (jcall get-ast parser))
                (e. +the-make-const-map+ |fromProperties| (jcall get-props parser)))))))

#-e.syntax::local-parser
(defmacro ast-node-bind ((tag-var line-var column-var text-var children-var) node-var &body body)
  `(destructuring-bind ((,tag-var ,line-var ,column-var ,text-var) &rest ,children-var) ,node-var
     ,@body))

#+e.syntax::local-parser
(defmacro ast-node-bind ((tag-var line-var column-var text-var children-var) node-var &body body)
  `(let* ((,tag-var  (intern (jarray-ref +token-names+
                                         (jcall +ast-get-type+ ,node-var))
                             :e.grammar))
          (,line-var (jcall +ast-get-line+ ,node-var))
          (,column-var (jcall +ast-get-column+ ,node-var))
          (,text-var (jcall +ast-get-text+ ,node-var))
          (,children-var (nodes-to-list (jcall +ast-first-child+ ,node-var))))
     ,@body))

(defun is-legitimate-toplevel-node (node)
  (if (typep node '(or |EExpr| |Pattern|))
    node
    (error "Non-expression/pattern ~S leaked out of antlr builder" node)))

(defun antlr-root (ast-nodes uri)
  (let ((enodes (map 'list #'is-legitimate-toplevel-node (map 'vector (lambda (tree) (build-antlr-nodes tree :uri uri)) ast-nodes))))
    (etypecase (length enodes)
      ((integer 0 0)
        (mn '|NullExpr|))
      ((integer 1 1)
        (first enodes))
      ((integer 2)
        (apply #'mn '|SeqExpr| enodes)))))

(defun build-antlr-nodes (ast-node &key uri path enclosing-doc-comment)
  (ast-node-bind (tag line column text in-children) ast-node
    (let* ((next-path (cons ast-node path))
           (out-children
             (unless (eql tag 'e.grammar::|DocComment|)
               (mapcar (lambda (c) (build-antlr-nodes c :path next-path :uri uri))
                       in-children)))
           (span (when (and uri (> line 0))
                   (e. +the-make-source-span+ |run|
                     uri +e-true+ line (1- column) line (1- column)))))
     (labels ((make-from-tag (&rest elements)
                (make-instance (or (find-symbol (symbol-name tag) :e.kernel)
                                   (find-symbol (symbol-name tag) :e.nonkernel))
                  :elements elements
                  :source-span span))
              (pass () (apply #'make-from-tag out-children)))
      (case tag
        ;; -- misc. leaves and 'special' nodes --
        ((e.grammar::|IDENT|)
          (assert (null out-children))
          text)
        ((e.grammar::|DOC_COMMENT|)
          (assert (null out-children))
          ; XXX the lexer should do this stripping
          (if (string= text "")
            nil ; absent doc comment
            (string-trim " 	
" (subseq text 3 (- (length text) 2)))))
        ((e.grammar::|DocComment|)
          (destructuring-bind (comment-node body) in-children
            (build-antlr-nodes body
              :path next-path
              :uri uri
              :enclosing-doc-comment (build-antlr-nodes comment-node
                                       :uri uri
                                       :path next-path))))
        ((e.grammar::|INT|)
          (assert (null out-children))
          (parse-integer text :radix 10))
        ((e.grammar::|HEX|)
          (assert (null out-children))
          (parse-integer text :radix 16))
        ((e.grammar::|FLOAT64|)
          (assert (null out-children))
          (let ((*read-default-float-format* 'double-float)
                (*read-eval* nil))
            ;; this should be safe because the parser's already checked it for float syntax
            ;; XXX implement actual E float syntax
            (read-from-string text)))
        ((e.grammar::|STRING|)
          (assert (null out-children))
          text)
        ((e.grammar::|CHAR_LITERAL|)
          (assert (null out-children))
          (destructuring-bind (character) (coerce text 'list)
            character))
        ((e.grammar::|URI|)
          (assert (null out-children))
          text)
        (e.grammar::|Absent|
          (assert (null out-children))
          nil)
        (e.grammar::|True|
          (assert (null out-children))
          +e-true+)
        (e.grammar::|False|
          (assert (null out-children))
          +e-false+)
        (e.grammar::|FunctionVerb|
          (assert (null out-children))
          ;; XXX should be handled by the expansion layer instead
          "run")
        (e.grammar::|URIGetter|
          (assert (null out-children))
          (cons tag text))
        ((e.grammar::|List|)
          (coerce out-children 'vector))

        ;; -- doc-comment introduction --
        ((e.grammar::|ThunkExpr| e.grammar::|ObjectHeadExpr|)
          (apply #'make-from-tag enclosing-doc-comment out-children))

        ;; -- negated-operator introduction --
        ((e.grammar::|SameExpr|)
          (destructuring-bind (left right) out-children
            (make-from-tag left right (ecase (find-symbol text :keyword)
                                         (:!= +e-true+)
                                         (:== +e-false+)))))
        ((e.grammar::|SamePattern|)
          (destructuring-bind (value) out-children
            (make-from-tag value (ecase (find-symbol text :keyword)
                                       (:!= +e-true+)
                                       (:== +e-false+)))))

        ;; -- other --
        (e.grammar::|SeqExpr|
          ;; XXX this is more like an expansion issue
          (if (null out-children)
            (mnp '|NullExpr| span)
            (pass)))
        (e.grammar::|NounExpr|
          (destructuring-bind (noun) out-children
            (etypecase noun
              (string (make-from-tag noun))
              ((cons (eql e.grammar::|URIGetter|) t)
                (mnp '|URISchemeExpr| span (cdr noun))))))
        ((e.grammar::|InterfaceExpr|)
          (destructuring-bind (name &rest rest) out-children
            (apply #'make-from-tag
              enclosing-doc-comment
              (if (typep name 'string)
                (mnp '|LiteralExpr| span name)
                name)
              rest)))

        (otherwise
          (pass)))))))

; --- Parse cache files ---

(defun load-parse-cache (stream)
  ; XXX options to disable this output
  (e. e.knot:+sys-trace+ |doing|
    (format nil "Loading parse cache from ~A" (enough-namestring (pathname stream)))
    (efun ()
      (loop
        for (source tree) in
          (with-standard-io-syntax
            (let ((*package* (find-package :e.kernel)))
              (read stream)))
        do (setf (hashref source *parse-cache-hash*) tree))
      (values))))

(defun save-parse-cache (stream)
  (e. e.knot:+sys-trace+ |doing|
    (format nil "Writing parse cache to ~A" (enough-namestring (pathname stream)))
    (efun ()
      (let ((data '()))
        (hashmap (lambda (source tree)
                   (push (list source tree) data))
                 *parse-cache-hash*)
        (with-standard-io-syntax
          (let ((*package* (find-package :e.kernel)))
            (write data :stream stream))))
      (values))))

(defun load-parse-cache-file (file)
  "Returns T if the file exists and therefore was loaded, or nil if it does not exist or could not be loaded."
  (with-simple-restart (continue "Skip loading parse cache ~S." file)
    (with-open-file (s file :direction :input
                            :if-does-not-exist nil
                            :external-format e.extern:+standard-external-format+)
      (when s
        (load-parse-cache s)
        t))))

(defun save-parse-cache-file (file)
  (with-simple-restart (continue "Skip writing parse cache ~S." file)
    (with-open-file (s file :direction :output
                            :if-exists :supersede
                            :external-format e.extern:+standard-external-format+)
      (save-parse-cache s))
    (values)))

(defmacro with-parse-cache-file ((file-form) &body body &aux (file-var (gensym "parse-cache-file-")))
  `(let ((,file-var ,file-form))
    (load-parse-cache-file ,file-var)
    (unwind-protect
      (progn ,@body)
      (save-parse-cache-file ,file-var))))

; --- ---

(def-vtable e-syntax-error
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |print| "syntax error: ")
    (e. tw |print| (princ-to-string this))))
