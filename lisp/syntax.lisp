; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang.syntax)

; --- ---

(defun is-identifier (text)
  (query-to-java "isIdentifier" text))

(defun convention-uncapitalize (string)
  (if (and (plusp (length string))
           (string/= string (string-upcase string)))
    (concatenate 'string (string (char-downcase (aref string 0))) 
                         (subseq string 1))
    string))

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

(defconstant +precedence-in-seq+ 80)
(defconstant +precedence-seq+ 80)

(defconstant +precedence-outer+ 100)

(defconstant +precedence-in-assign-left+ nil)


; xxx stomping on erights.org namespace
(defglobal +e-printer+ (e-lambda "org.erights.e.elang.syntax.ePrinter"
    (:doc "Centralized object for generating E source and E-like text."
     :stamped +deep-frozen-stamp+)
  
  (:|__printOn| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<E-syntax printer>")
    nil)
  
  (:|printDocComment| (tw text)
    ; XXX have a strictness (about */) argument
    "Print a \"/** */\" documentation comment with a trailing line break on 'tw', if 'text' is not an empty string."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef text 'string)
    (when (string/= text "")
      (when (search "*/" text)
        ; xxx is there an escaping mechanism we can use?
        ; using print-not-readable is slightly wrong as this isn't a *lisp* printing problem
        (error "doc comment containing \"*/\" cannot be printed: ~A" (e-quote text)))
      (e. tw |print| "/** " text " */")
      (e. tw |println|))
    nil)
  
  (:|printVerb| (tw verb)
    "Print a verb with appropriate quoting, as in CallExpr or EMethod."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef verb 'string)
    (if (is-identifier verb)
      (e. tw |print| verb)
      (e. tw |quote| verb))
    nil)
   
  (:|printNoun| (tw noun)
    "Print a noun with appropriate quoting, as in NounExpr."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef noun 'string)
    (if (is-identifier noun)
      (e. tw |print| noun)
      (progn
        (e. tw |print| "::")
        (e. tw |quote| noun)))
    nil)
  
  (:|printPropertySlot| (tw prop-name)
    "The print representation of a property-slot object. Not parsable."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef prop-name 'string)
    (e. tw |print| "_::&")
    (if (is-identifier prop-name)
      (e. tw |write| prop-name)
      (e. tw |quote| prop-name)))
  
  (:|printString| (tw this)
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef this 'string)
    (e. tw |write| "\"")
    (print-escaped tw this "\"")
    (e. tw |write| "\""))
  
  (:|printCons| (tw this)
    "The print representation of a Lisp cons. Not parsable."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef this 'cons)
    ; XXX this is a lousy print syntax
    (e. tw |write| "< ")
    (e. tw |quote| (car this))
    (e. tw |write| " > + ")
    (e. tw |quote| (cdr this)))
  
  (:|printCharacter| (tw this)
    "The print representation of a character."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef this 'character)
    ; XXX escape when appropriate
    (e. tw |write| "'")
    (print-escaped tw (vector this) "\'")
    (e. tw |write| "'"))
  
  (:|printList| (tw this quote-elements)
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef this 'vector)
    (e-coercef quote-elements 'e-boolean)
    (if (e-is-true quote-elements)
      (e. this |printOn| "[" ", " "]" tw)
      (progn
        (e. tw |print| "[" #|]|#)
        (loop for sep = "" then ", "
              for element across this
              do (e. tw |print| sep element))
        (e. tw |print| #|[|# "]"))))
  
  (:|printMethodHeader| (tw is-kernel doc-comment verb params opt-result-guard)
    "Print a to/method as in an EScript or interface sugar."
    ; XXX should we have quoting options for params and opt-result-guard?
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef is-kernel 'e-boolean)
    ; coercion of doc-comment, verb handled elsewhere
    (e-coercef params 'vector)

    (e. +e-printer+ |printDocComment| tw doc-comment)
    (e. tw |print| (if (e-is-true is-kernel) "method " "to "))
    (e. +e-printer+ |printVerb| tw verb)
    (e. tw |print| "(" #|)|#)
    (loop for sep = "" then ", "
          for element across params
          do (e. tw |print| sep element))
    (e. tw |print| #|(|# ")")
    (when opt-result-guard
      (e. tw |print| " :")
      (e. tw |print| opt-result-guard)))
  
  (:|printGuardedNounPattern| (tw opt-name opt-guard)
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef opt-name '(or null string))
    (if opt-name
      (e. +e-printer+ |printNoun| tw opt-name)
      (e. tw |print| "_"))
    (when opt-guard
      (e. tw |print| " :")
      (e. tw |print| opt-guard)))

  (:|printExprBlock| (tw node)
    "Print the EExpr 'node' on 'tw' enclosed in braces."
    (e. tw |print| "{" #|}|#)
    (let ((tw2 (e. tw |indent|)))
      (e. tw2 |println|)
      (e. node |welcome| (e. +e-printer+ |makePrintENodeVisitor| tw +precedence-outer+)))
    (e. tw |lnPrint| #|{|# "}"))

  (:|makePrintENodeVisitor| (tw)
    (e. +e-printer+ |makePrintENodeVisitor| tw +precedence-outer+))
  
  (:|makePrintENodeVisitor| (tw precedence)
    "Return an ETreeVisitor which prints nodes to 'tw' in standard E syntax. 'precedence' controls whether the expression is parenthesized. 

XXX make precedence values available as constants"
    ; XXX have an in-quasi argument - print the LiteralExpr "$" as "$$", and the ${n} as ${n}
    ; XXX have a self argument for implementation inheritance
    ; strictness controls?
    ; XXX tests for the visitor's Ref-transparency
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef precedence '(or null integer))
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
                (noun-pattern-printer (tag)
                  (lambda (opt-original noun-expr opt-guard-expr)
                    (declare (ignore opt-original))
                    (e. tw |print| tag)
                    (subprint noun-expr nil)
                    (when opt-guard-expr
                      (e. tw |print| " :")
                      (subprint opt-guard-expr +precedence-in-guard+))))
                (quasi-printer (tag)
                  (lambda (opt-original index)
                    (declare (ignore opt-original))
                    (e-coercef index 'integer)
                    (e. tw |print| tag "{" index "}"))))
        (e-lambda "$printVisitor" ()
          (:|__printOn| (ptw)
            (e-coercef ptw +the-text-writer-guard+)
            (e. ptw |print| "<E-syntax node visitor printing to " tw ">")
            nil)
          
          (:|visitAssignExpr| (opt-original noun-expr value-expr)
            (declare (ignore opt-original))
            (precedential (+precedence-assign+)
              (subprint noun-expr +precedence-in-assign-left+)
              (e. tw |print| " := ")
              (subprint value-expr +precedence-in-assign-right+)))
          
          (:|visitCallExpr| (opt-original rec verb args)
            (declare (ignore opt-original))
            (subprint rec +precedence-call-rec+)
            (e. tw |print| ".")
            (e. +e-printer+ |printVerb| tw verb)
            (e. tw |print| "(" #|)|#)
            (loop for sep = "" then ", "
                  for arg across args
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
          
          (:|visitDefineExpr| (opt-original pattern specimen opt-ejector)
            (declare (ignore opt-original))
            (precedential (+precedence-define+)
              (e. tw |print| "def ")
              (subprint pattern nil)
              (e. tw |print| " := ")
              (if opt-ejector
                (progn
                  (e. tw |print| "(")
                  (subprint specimen +precedence-outer+)
                  (e. tw |print| ", ")
                  (subprint specimen +precedence-outer+)
                  (e. tw |print| ")"))
                (subprint specimen +precedence-define-right+))))
          
          (:|visitEscapeExpr| (opt-original ejector-patt body catch-patt catch-body)
            (declare (ignore opt-original))
            (e. tw |print| "escape ")
            (subprint ejector-patt nil)
            (e. tw |print| " ")
            (subprint-block body)
            (when catch-patt
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
            (when false-block
              (e. tw |print| " else ")
              (subprint-block false-block)))
          
          (:|visitLiteralExpr| (opt-original value)
            (declare (ignore opt-original))
            (e-coercef value '(or string integer character float64))
            (etypecase value
              (string
                (e. +e-printer+ |printString| tw value))
              (integer
                (e. tw |print| value)) ; XXX E-syntax hiding?
              (float64
                (e. tw |print| value)) ; XXX E-syntax hiding?
              (character
                (e. +e-printer+ |printString| tw value))))
          
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
          
          (:|visitObjectExpr| (opt-original doc-comment qualified-name auditors script)
            (declare (ignore opt-original))
            (e. +e-printer+ |printDocComment| tw doc-comment)
            (e. tw |print| "def ")
            (e. +e-printer+ |printString| tw qualified-name)
            (when (> (length auditors) 0)
              (e. tw |print| " implements ")
              (loop for sep = "" then ", "
                    for sub across auditors
                    do (e. tw |print| sep)
                       (subprint sub nil)))
            (subprint script nil))
          
          (:|visitSeqExpr| (opt-original subs)
            (declare (ignore opt-original))
            (precedential (+precedence-seq+)
              (loop for sep = "" then #\Newline
                    for sub across subs
                    do (e. tw |print| sep)
                       (subprint sub +precedence-in-seq+))))
          
          (:|visitSlotExpr| (opt-original noun)
            (declare (ignore opt-original))
            (precedential (+precedence-slot-expr+)
              (e. tw |print| "&")
              (subprint noun +precedence-in-slot-expr+)))
          
          (:|visitEScript| (opt-original opt-methods matchers)
            (declare (ignore opt-original))
            (e-coercef opt-methods '(or null vector))
            (e-coercef matchers 'vector)
            ; XXX print patterns and opt-result-guard directly
            (if opt-methods
              (progn
                (e. tw |print| " {" #|}|#)
                (let ((indented (e. tw |indent|)))
                  (loop for method across opt-methods do
                    (e. indented |println|)
                    (subprint method nil :tw indented))
                (loop for matcher across matchers do
                  (e. indented |println|)
                  (subprint matcher nil :tw indented)))
                (e. tw |lnPrint| #|{|# "}"))
              (progn
                (assert (= (length matchers) 1) () "XXX Don't know what to do here")
                (e. tw |print| " ")
                (subprint (aref matchers 0) nil))))
            
          (:|visitEMethod| (opt-original doc-comment verb patterns opt-result-guard body)
            (declare (ignore opt-original))
            (e. tw |println|)
            (e. +e-printer+ |printMethodHeader| tw +e-true+
              doc-comment
              verb
              (map 'vector
                (lambda (pattern)
                  (e-lambda "syntax-printer" () (:|__printOn| (tw)
                    (e-coercef tw +the-text-writer-guard+)
                    (subprint pattern +precedence-outer+ :tw tw))))
                patterns) 
              (when opt-result-guard
                (e-lambda "syntax-printer" () (:|__printOn| (tw)
                  (e-coercef tw +the-text-writer-guard+)
                  (subprint opt-result-guard +precedence-in-guard+ :tw tw)))))
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
          (:|visitSlotPattern/3| (noun-pattern-printer "&"))
          (:|visitVarPattern/3| (noun-pattern-printer "var "))

          (:|visitIgnorePattern| (opt-original)
            (declare (ignore opt-original))
            (e. tw |print| "_"))
          
          (:|visitListPattern| (opt-original subpatts)
            (declare (ignore opt-original))
            (e. tw |print| "[" #|]|#)
            (loop for sep = "" then ", "
                  for patt across subpatts
                  do (e. tw |print| sep)
                     (subprint patt nil))
            (e. tw |print| #|[|# "]"))
          
          (:|visitSuchThatPattern| (opt-original subpatt condition)
            (declare (ignore opt-original))
            (subprint subpatt nil)
            (e. tw |print| " ? ")
            (subprint condition +precedence-in-suchthat+))
          
          (:|visitQuasiLiteralNode/2| (quasi-printer "$"))
          (:|visitQuasiPatternNode/2| (quasi-printer "@"))
          (:|visitQuasiLiteralExpr/2| (quasi-printer "$"))
          (:|visitQuasiPatternExpr/2| (quasi-printer "@"))
          (:|visitQuasiLiteralPatt/2| (quasi-printer "$"))
          (:|visitQuasiPatternPatt/2| (quasi-printer "@"))
          
          ))))
  
  ))

; --- Parsing ---

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

(defvar *parse-cache-hash* (make-hash-table :test #'equal))

(defvar *parser-process*)

(defun start-parser ()
  (assert (not (boundp '*parser-process*)))
  ; XXX pipe this through common tracing architecture
  (format *trace-output* "~&; starting Java-E parsing subprocess~%")
  (asdf:operate 'asdf:compile-op :e-on-cl.antlr-parser)
  (setf *parser-process* (e.util:run-program
    "/usr/bin/env"
    (list "rune"
          "-J-XX:ThreadStackSize=10240"
          "-cpa" (namestring (merge-pathnames #P"jlib/" (asdf:component-pathname +the-asdf-system+)))
          "-cpa" (namestring (compute-antlr-class-location))
          "-De.onErrorExit=report"
          (format nil "-Dfile.encoding=~A" e.extern:+standard-external-format-common-name+)
          "-") 
    :input :stream
    :output :stream
    :error t
    :wait nil))
  (format (e.util:external-process-input-stream *parser-process*)
          "def parseEToSExpression := <import:parseEToSExpression>(<unsafe>)~%")
  (values))

; disabling this for now because starting the parser at load time introduces dependencies (particularly, e.extern:+standard-external-format-common-name+ being defined first)
; ; XXX stubbing out the parser for systems where we haven't made it work yet
; #-(or abcl clisp) (start-parser)

(defun kill-parser ()
  (when (boundp '*parser-process*)
    (close (e.util:external-process-input-stream *parser-process*))
    (close (e.util:external-process-output-stream *parser-process*))
    (makunbound '*parser-process*)))

(defun ensure-parser ()
  (unless (boundp '*parser-process*)
    (start-parser)))

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

(defvar *parse-counter* 0)

(define-condition link-out-of-sync-error (error) ())

(defun call-to-java (verb args &key trying-again)
  (ensure-parser)

  (format (e-util:external-process-input-stream *parser-process*) 
          "println(\"~S \" + parseEToSExpression(~S, ~A)); stdout.flush()~%"
          (incf *parse-counter*) 
          verb
          (e-quote (coerce args 'vector)))
  (finish-output (e-util:external-process-input-stream *parser-process*))
  
  (handler-case 
      (let ((return-serial
              (read (e-util:external-process-output-stream *parser-process*)))
            (tree-expr 
              (let ((*package* (find-package :e.elang.vm-node)))
                (read (e-util:external-process-output-stream *parser-process*)))))
        (unless (eql *parse-counter* return-serial) 
          (error 'link-out-of-sync-error))
        ;(format t "got back: ~S~%" tree-expr)
        tree-expr)
    ((or end-of-file link-out-of-sync-error) (condition)
      (format *trace-output* "~&; error in parser communication: ~A (second time: ~A)~%" condition trying-again)
      (kill-parser)
      (if trying-again
        (error condition)
        (progn
          (call-to-java verb args :trying-again t))))))

(defun query-to-java (verb arg)
  "Memoized version of call-to-java"
  ; XXX multiple arg support eventually

  (e-coercef arg 'string) ; XXX Twine
  
  ; Ensure that the source string is printable under *print-readably*, and therefore won't hose the parse cache. (simple-base-strings are not, in SBCL.)
  ; XXX check if this works on implementations other than SBCL 
  (setf arg (coerce arg '(vector character)))
  
  (let* ((args (list arg))
         (key (list verb args)))
    (multiple-value-bind (cached present-p) (gethash key *parse-cache-hash*)
    (if present-p
      cached
      (progn
        (format *trace-output* "~&; query-to-java: ~A(~A...)" verb (subseq arg 0 (position #\Newline arg)))
        (force-output *trace-output*)
        (let ((answer (call-to-java verb args)))
          (setf (gethash key *parse-cache-hash*) answer)
          (format *trace-output* " done~%")
          answer))))))

#+(or)
(defun parse-to-expr (source)
  (query-to-java "run" source))

#+(or)
(defun e-source-to-tree (source &key syntax-ejector)
  (build-nodes (query-or-die syntax-ejector "run" source)))

(defun query-or-die (ejector &rest msg)
  (let ((result (apply #'query-to-java msg)))
    (if (eql (first result) 'error)
      (error-from-e-error-string ejector (second result))
      result)))

(defun e-source-to-tree (source &key syntax-ejector)
  (antlr-root (query-or-die syntax-ejector "antlrParse" source)))

(defglobal +prim-parser+ (e-lambda "org.cubik.cle.prim.parser"
    (:stamped +deep-frozen-stamp+)
  (:|run| (source syntax-ejector)
    (e-source-to-tree source :syntax-ejector syntax-ejector))
  (:|run| (source)
    (e-source-to-tree source))))

;;; --- Antlr parser support ---

(defun noun-to-resolver-noun (noun)
  (make-instance '|NounExpr| :elements (list (concatenate 'string (first (node-elements noun)) "__Resolver"))))

(defun mn (name &rest elements)
  (make-instance name :elements elements))

(defparameter *b* (e-lambda "$builder" ()
  ;; xxx implements EBuilder
  (:|assign| (l r) 
    ;; XXX needs to expand some structures
    (make-instance '|AssignExpr| :elements (list l r)))
  (:|call| (r v a)
    (e-coercef a 'vector)
    (make-instance '|CallExpr| :elements (list* r v (coerce a 'list))))
  (:|define| (l r ej) 
    ;; XXX needs to expand cyclic defines
    (make-instance '|DefineExpr| :elements (list l r ej)))
  (:|escape| (ej-pattern body) 
    (make-instance '|EscapeExpr| :elements (list ej-pattern body nil nil)))
  (:|escape| (ej-pattern body catch-pattern catch-body) 
    (make-instance '|EscapeExpr| :elements (list ej-pattern body catch-pattern catch-body)))
  (:|finalPattern| (noun guard)
    (make-instance '|FinalPattern| :elements (list noun guard)))
  (:|varPattern| (noun guard)
    (make-instance '|VarPattern| :elements (list noun guard)))
  (:|slotPattern| (noun guard)
    (make-instance '|SlotPattern| :elements (list noun guard)))
  (:|_noun| (noun)
    "XXX what should this really be?"
    (make-instance '|NounExpr| :elements (list noun)))
  (:|literal| (value)
    "XXX interface doc says 'tokenOrData'"
    (make-instance '|LiteralExpr| :elements (list value)))
  (:|matchBind| (specimen pattern invert)
     (if (e-is-true invert)
      (mn '|CallExpr| (e. *b* |matchBind| specimen pattern +e-false+) "not")
      (make-instance '|MatchBindExpr| :elements (list specimen pattern))))
  (:|sequence| (subs)
    (e-coercef subs 'vector)
    (make-instance '|SeqExpr| :elements (coerce subs 'list)))
  (:|listPattern| (subs)
    (e-coercef subs 'vector)
    (make-instance '|ListPattern| :elements (coerce subs 'list)))
  (:|suchThat| (pattern test)
    (make-instance '|SuchThatPattern| :elements (list pattern test)))
  (:|ignore| ()
    (load-time-value (make-instance '|IgnorePattern| :elements '())))
  (:|hide| (expr)
    (make-instance '|HideExpr| :elements (list expr)))
  (:|if| (test then)
    ;; DIFFERENCE: existence of 2-arg form
    (make-instance '|IfExpr| :elements (list test then (make-instance '|NounExpr| :elements '("null")))))
  (:|if| (test then else)
    ;; DIFFERENCE: "ifx"
    (make-instance '|IfExpr| :elements (list test then else)))
  (:|vTable| (opt-methods matchers)
    ;; XXX "vTable" is a lousy name
    (make-instance '|EScript| :elements (list opt-methods matchers)))
  (:|object| (doc-comment qualified-name parent auditors script)
    (assert (null parent))
    (etypecase (ref-shorten qualified-name)
      (string
        (mn '|ObjectExpr| doc-comment qualified-name auditors script))
      (|Pattern|
        ;; non-kernel
        (mn '|DefineExpr| qualified-name (mn '|ObjectExpr| doc-comment (concatenate 'string "$" (or (pattern-opt-noun qualified-name) "_")) auditors script) nil))))
  (:|doMeta| (keyword verb args)
    (declare (ignorable args))
    (ecase (intern keyword :keyword)
      (:|meta|
        (ecase (intern verb :keyword)
          (:|context|
            (mn '|MetaContextExpr|))
          (:|getState|
            (mn '|MetaStateExpr|))))
      (:|pragma|
        (make-instance '|NounExpr| :elements '("null")))))
  
  (:|null| ()
    ;; DIFFERENCE: "getNULL"
    (make-instance '|NounExpr| :elements '("null")))
  (:|send| (r v a)
    (e-coercef a 'vector)
    (mn '|CallExpr|
      (mn '|NounExpr| "E")
      "send"
      (make-instance '|CallExpr| :elements (list* (mn '|NounExpr| "__makeList") "run" r (mn '|LiteralExpr| v) (coerce a 'list)))))
  (:|forward| (noun-string) 
    (let* ((noun (mn '|NounExpr| noun-string))
           (resolver-noun (noun-to-resolver-noun noun)))
      (make-instance '|SeqExpr| :elements (list
        (make-instance '|DefineExpr| :elements (list 
          (make-instance '|ListPattern| :elements (list 
            (make-instance '|FinalPattern| :elements (list noun nil))
            (make-instance '|FinalPattern| :elements (list resolver-noun nil))))
          (load-time-value (make-instance '|CallExpr| :elements (list (make-instance '|NounExpr| :elements '("Ref")) "promise")))
          (make-instance '|NounExpr| :elements '("null"))))
        resolver-noun))))
  (:|bindDefiner| (noun opt-guard)
    ;; XXX *WRONG* temporary name - nondeterministic
    (let ((temp (make-instance '|NounExpr| :elements (list (symbol-name (gensym))))))
      (make-instance '|SuchThatPattern| :elements (list
        (make-instance '|FinalPattern| :elements (list
          temp
          opt-guard))
        (make-instance '|SeqExpr| :elements (list
          (make-instance '|CallExpr| :elements (list
            (noun-to-resolver-noun noun)
            "resolve"
            temp))
          (make-instance '|NounExpr| :elements '("true"))))))))
  (:|update| (l verb r)
    (make-instance '|AssignExpr| :elements (list l (make-instance '|CallExpr| :elements (list l verb r)))))
  (:|tuple| (elements)
    (e-coercef elements 'vector)
    (make-instance '|CallExpr| :elements (list*
      (load-time-value (make-instance '|NounExpr| :elements '("__makeList")))
      "run"
      (coerce elements 'list))))
  (:|uriExpr| (uri)
    (e-coercef uri 'string)
    (make-instance '|CallExpr| :elements (list
      (make-instance '|NounExpr| :elements (list (concatenate 'string (subseq uri 0 (position #\: uri)) "__uriGetter")))
      "get"
      (make-instance '|LiteralExpr| :elements (list (subseq uri (1+ (position #\: uri))))))))
  (:|cast| (expr guard)
    (make-instance '|CallExpr| :elements (list
      (make-instance '|CallExpr| :elements (list
        (make-instance '|NounExpr| :elements '("ValueGuard"))
        "coerce"
        guard
        (make-instance '|NounExpr| :elements '("null"))))
      "coerce"
      expr
      (make-instance '|NounExpr| :elements '("null")))))
  (:|till| (left right)
    (make-instance '|CallExpr| :elements (list
      (make-instance '|NounExpr| :elements '("__makeOrderedSpace"))
      "op__till"
      left
      right)))
  (:|thru| (left right)
    (make-instance '|CallExpr| :elements (list
      (make-instance '|NounExpr| :elements '("__makeOrderedSpace"))
      "op__thru"
      left
      right)))
  (:|same| (left right invert)
    (if (e-is-true invert)
      (mn '|CallExpr| (e. *b* |same| left right +e-false+) "not")
      (mn '|CallExpr| (mn '|NounExpr| "__equalizer") "sameEver" left right)))
  (:|compare| (op left right)
    (mn '|CallExpr| (mn '|NounExpr| "__comparer") op left right))
  (:|return| (value)
    (mn '|CallExpr| (mn '|NounExpr| "__return") "run" value))

  (:|unaryVerb| (op)
    (case (find-symbol op :keyword)
      (:! "not")
      (:~ "complement")
      (otherwise op)))
  (:|binaryVerb| (op)
    (case (find-symbol op :keyword)
      (:+ "add")
      (:- "subtract")
      (:* "multiply")
      (:/ "approxDivide")
      (:// "floorDivide")
      (:% "mod")
      (:%% "remainder")
      (:& "and")
      (:\| "or")
      (otherwise op)))
  (:|compareVerb| (op)
    (case (find-symbol op :keyword)
      (:<=> "asBigAs")
      (:< "lessThan")
      (:<= "ltEq")
      (:> "greaterThan")
      (:>= "gtEq")
      (otherwise op)))
  (:|to| (doc msg-patt body)
    ;; XXX entirely wrong
    (e. *b* |method| doc msg-patt body))
  (:|method| (doc msg-patt body)
    (destructuring-bind (verb patterns opt-result-guard) msg-patt
      (make-instance '|EMethod| :elements (list doc verb patterns opt-result-guard body))))
  (:|methHead| (verb patterns opt-result-guard)
    (list verb patterns opt-result-guard))
  (:|flowAnd| (left right)
    ;; XXX handle bindings
    (mn '|IfExpr| left right (mn '|NounExpr| "false")))
  (:|flowOr| (left right)
    ;; XXX handle bindings
    (mn '|IfExpr| left (mn '|NounExpr| "true") right))))

(defun is-an-expr (expr)
  ;; XXX in the presence of arbitrary builders this isn't actually always expected
  (if (typep expr '|EExpr|)
    expr
    (error "Non-expression ~S leaked out of antlr builder" expr)))

(defun antlr-root (ast-nodes)
  (e. *b* |sequence| (map 'vector #'is-an-expr (map 'vector #'build-antlr-nodes ast-nodes))))

(defun unwrap-noun (string) 
  "defunct stub"
  string)

#+(or)
(defun unwrap-noun (noun)
  "uses of this are places where there are IDENTs that aren't NounExprs"
  (check-type noun |NounExpr|)
  (first (node-elements noun)))

(defun build-antlr-nodes (expr &key path)
  (destructuring-bind ((tag text) &rest in-children) expr
    (let* ((next-path (cons expr path))
           (out-children (mapcar (lambda (c) (build-antlr-nodes c :path next-path))
                                 in-children)))
      (case tag
        ((e.grammar::|DOC_COMMENT|) (e. *b* |null|))
        ;; GRUMBLE: nouns should be wrapped so that I can apply this to only nouns and not general identifiers (like verbs)
        ((e.grammar::|IDENT|) text)
        ((e.grammar::|INT|) (read-from-string text))
        ((e.grammar::|FLOAT64|) (let ((*read-default-float-format* 'double-float)) (read-from-string text) #| XXX implement actual E float syntax|#))
        ((e.grammar::|STRING|)
          (assert (null out-children))
          text)
        ((e.grammar::|CHAR_LITERAL|) 
          (assert (null out-children))
          (destructuring-bind (character) (coerce text 'list)
            character))
        ((e.grammar::|LiteralPattern|)
          ;; XXX the name LiteralPattern is utterly unrelated to reality: this is actually the FQN field in non-define-sugared ObjectExprs
          text)
        ((e.grammar::|"&"| e.grammar::|"-"| e.grammar::|"+"| e.grammar::|"*"| e.grammar::|"/"| e.grammar::|"//"| e.grammar::|"%"| e.grammar::|"%%"|) 
          (e. *b* |binaryVerb| (subseq (symbol-name tag) 1 (1- (length (symbol-name tag))))))
        ((e.grammar::|"!"| e.grammar::|"~"|)
          (assert (null out-children))
          (symbol-name tag))
        ((e.grammar::|URI|) text)
        ((e.grammar::|List| e.grammar::|Extends| e.grammar::|Implements|) out-children)
        (e.grammar::|Absent|
          nil)
        ((e.grammar::|"pragma"| e.grammar::|"meta"| e.grammar::|"implements"| e.grammar::|"extends"|)
          ;; XXX review these nodes; some of them are not being generated any more 
          (assert (null out-children))
          tag)
        ((e.grammar::|"=~"| e.grammar::|"!~"| e.grammar::|"_"| e.grammar::|".."| e.grammar::|"=="| e.grammar::|"!="| e.grammar::|"<=>"|  e.grammar::|"<"| e.grammar::|"<="| e.grammar::|">"| e.grammar::|">="| e.grammar::|"=>"|) 
          (assert (null out-children))
          tag)
        (e.grammar::|"catch"| 
          (destructuring-bind (pattern body) out-children
            (list pattern body)))
        
        (e.grammar::|"interface"|
          (e. *b* |interface| #|...|#))
        (e.grammar::|"for"|
          (e. *b* |for| #|...|#))
        
        (e.grammar::|LiteralExpr|
          (destructuring-bind (sub) out-children
            (e. *b* |literal| sub)))
        (e.grammar::|NounExpr|
          (destructuring-bind (sub) out-children
            (e. *b* |_noun| sub)))
        
        (e.grammar::|AssignExpr|
          ;; GRUMBLE: updates should have a separate node type
          (ecase (length out-children)
          (2 (destructuring-bind (l r) out-children
               (e. *b* |assign| l r)))
          (3 (destructuring-bind (l op r) out-children
               (e. *b* |update| 
                 l 
                 (etypecase op
                   (string (subseq op (1- (length op))))
                   (|NounExpr| (unwrap-noun op))) 
                 r)))))
                 
        ((e.grammar::|BinaryExpr|)
          (destructuring-bind (left &rest rights) out-children
            (e. *b* |call| left (e. *b* |binaryVerb| text) (coerce rights 'vector))))
        ((e.grammar::|CompareExpr|)
          (destructuring-bind (left right) out-children
            (e. *b* |compare| (e. *b* |compareVerb| text) left right)))

        ((e.grammar::|SameExpr|)
          (destructuring-bind (left right) out-children
            (e. *b* |same| left right (ecase (find-symbol text :keyword)
                                        (:!= +e-true+)
                                        (:== +e-false+)))))
            
        ((e.grammar::|CallExpr| e.grammar::|"."|)
          ;; GRUMBLE: shouldn't need to match "."
          ;; GRUMBLE: things like + == ..! should be reported as 'binary operator', not same as CallExpr
          (destructuring-bind (r verboid &rest a) out-children
            (let ((verb (etypecase verboid
                          (|NounExpr|    (unwrap-noun verboid))
                          (|LiteralExpr| (first (node-elements verboid)))
                          (string        verboid)
                          ((member e.grammar::|".."|
                                   e.grammar::|"..!"|)
                                         '#:meaningless))))
              (cond 
                ((member r '(e.grammar::|"meta"| e.grammar::|"pragma"|))
                  (e. *b* |doMeta|
                    (subseq (symbol-name r) 1 (1- (length (symbol-name r))))
                    verb
                    (coerce a 'vector)))
                ((member verboid '(e.grammar::|".."| e.grammar::|"..!"|))
                  (assert (= 1 (length a)))
                  (e-call *b* (case verboid
                                (e.grammar::|".."| "thru")
                                (e.grammar::|"..!"| "till")) 
                              (list r (first a))))
                (t
                  (e. *b* |call| r 
                                 verb
                                 (coerce a 'vector)))))))
        ((e.grammar::|SendExpr|)
          (destructuring-bind (r verboid &rest a) out-children
            (let ((verb (etypecase verboid
                          (|NounExpr|    (unwrap-noun verboid))
                          (|LiteralExpr| (first (node-elements verboid)))
                          (string        verboid))))
              (cond
                (t
                  (e. *b* |send| r 
                                 verb
                                 (coerce a 'vector)))))))
        (e.grammar::|CoerceExpr|
          (destructuring-bind (expr guard) out-children
            (e. *b* |cast| expr guard)))
        (e.grammar::|DefineExpr|
          (destructuring-bind (l r &optional guard) out-children
            (e. *b* |define| l r guard)))
        (e.grammar::|EscapeExpr|
          ;; GRUMBLE: an empty body makes the parser return only the pattern node - it should return an "empty-body node" or some such instead
          (destructuring-bind (patt &optional (body (e. *b* |null|)) ((catch-patt catch-body) '(nil nil))) out-children
            (e. *b* |escape| patt body catch-patt catch-body)))
        (e.grammar::|ForwardExpr|
          (destructuring-bind (noun-expr) out-children
            (e. *b* |forward| (first (node-elements noun-expr)))))
        (e.grammar::|HideExpr|
          (destructuring-bind (sub) out-children
            (e. *b* |hide| sub)))
        (e.grammar::|IfExpr|
          (e-call *b* "if" out-children))
        (e.grammar::|MatchBindExpr|
          (destructuring-bind (specimen pattern) out-children
            (e. *b* |matchBind| specimen pattern (ecase (find-symbol text :keyword)
                                                   (:!~ +e-true+)
                                                   (:=~ +e-false+)))))
        (e.grammar::|MetaContextExpr|
          (e. *b* |doMeta| "meta" "context" #()))
        (e.grammar::|MetaStateExpr|
          (e. *b* |doMeta| "meta" "getState" #()))
        (e.grammar::|PrefixExpr|
          (destructuring-bind (operator operand) out-children
            (e. *b* |call| operand (e. *b* |unaryVerb| operator) #())))
        (e.grammar::|TupleExpr|
          (e. *b* |tuple| (coerce out-children 'vector)))
        (e.grammar::|ReturnExpr|
          (destructuring-bind (value-expr) out-children
            (e. *b* |return| value-expr)))
          
        (e.grammar::|ConditionalExpr|
          (destructuring-bind (left right) out-children
            (e-call *b* (ecase (find-symbol text :keyword)
                          (:&& "flowAnd")
                          (:|| "flowOr"))
                        (list left right))))
        
        ((e.grammar::|BindPattern|)
          (destructuring-bind (noun &optional guard) out-children
            (e. *b* |bindDefiner| noun guard)))
        ((e.grammar::|FinalPattern| e.grammar::|SlotPattern| e.grammar::|VarPattern|)
          (destructuring-bind (noun &optional guard) out-children
            (e-call *b* (convention-uncapitalize (symbol-name tag)) (list noun guard))))
        ((e.grammar::|IgnorePattern|)
          (e. *b* |ignore|))
        (e.grammar::|ListPattern|
          (e. *b* |listPattern| (coerce out-children 'vector)))
        ((e.grammar::|SuchThatPattern|)
          (destructuring-bind (pattern test) out-children
            (e. *b* |suchThat| pattern test)))
        (e.grammar::|URIExpr| (e. *b* |uriExpr| (first out-children)))
        (e.grammar::|SeqExpr| (e. *b* |sequence| (coerce out-children 'vector)))
        ((e.grammar::|ObjectExpr|)
          (ecase (length out-children)
            (2 (destructuring-bind (qualified-name (extends implements script)) out-children
                 (e. *b* |object| "" qualified-name extends (coerce implements 'vector) script)))
            (4 (destructuring-bind (qname-thing params result-guard body) out-children
                 ;; GRUMBLE: this should either be a separate node type, or a *method* node should take the place of the script node
                 (e. *b* |object| "" qname-thing nil #() (e. *b* |vTable| (vector (e. *b* |to| "" (e. *b* |methHead| "run" (coerce params 'vector) result-guard) body)) #()))))))
        ((e.grammar::|EScript|)
          (destructuring-bind (extends implements todo) out-children
            (let ((methods '())
                  (matchers '()))
              (loop while (typep (first todo) '|EMethod|) do
                (push (pop todo) methods))
              (loop while (typep (first todo) '|EMatcher|) do
                (push (pop todo) matchers))
              (when todo
                (error "unexpected element type or misplaced method in EScript: ~S remaining (whole is ~S)" todo out-children))
              (list extends
                    implements
                    (e. *b* |vTable| (coerce (nreverse methods) 'vector)
                                     (coerce (nreverse matchers) 'vector))))))
        ((e.grammar::|EMethod|)
          (let ((doc-comment ""))
            (when (eql (caaar in-children) '|DOC_COMMENT|)
              ;; ick
              (setf doc-comment (cadar (pop in-children))))
            (destructuring-bind (verb-ident param-list return-guard body) out-children
              (when (string= text "fn")
                ;; xxx crude
                (setf text "to"))
              (assert (member text '("to" "method") :test #'string=))
              (e-call *b* text (list doc-comment
                                     (e. *b* |methHead| (unwrap-noun verb-ident) (coerce param-list 'vector) return-guard)
                                     body)))))
        (otherwise
          (cond
            ((and (string= (aref (symbol-name tag) 0) "\"") 
                  (string= (aref (symbol-name tag) (- (length (symbol-name tag)) 2)) "="))
              (e. *b* |binaryVerb| text))
            (t (error "don't know what to do with ~S <- ~A" expr (write-to-string path :length 5)))))))))

; --- Parse cache files ---
 
(defun load-parse-cache (stream)
  ; XXX options to disable this output
  (format *trace-output* "~&; Loading parse cache from ~A..." (enough-namestring (pathname stream)))
  (force-output *trace-output*)
  (loop
    for (source tree) in
      (with-standard-io-syntax
        (let ((*package* (find-package :e.elang.vm-node))
              (*read-eval* nil)) 
          (read stream)))
    do (setf (gethash source *parse-cache-hash*) tree))
  (format *trace-output* "done~%")
  (values))
    
(defun save-parse-cache (stream)
  (format *trace-output* "~&; Writing parse cache to ~A..." (enough-namestring (pathname stream)))
  (force-output *trace-output*)
  (let ((data (loop for source being each hash-key of *parse-cache-hash* using (hash-value tree)
                    collect (list source tree))))
    (with-standard-io-syntax
      (let ((*package* (find-package :e.elang.vm-node)))
        (write data :stream stream))))
  (format *trace-output* "done~%")
  (values))

(defun load-parse-cache-file (file)
  "Returns T if the file exists and therefore was loaded."
  (with-open-file (s file :direction :input 
                          :if-does-not-exist nil
                          :external-format e.extern:+standard-external-format+)
    (when s
      (load-parse-cache s)
      t)))

(defun save-parse-cache-file (file)
  (with-open-file (s file :direction :output
                          :if-exists :supersede
                          :external-format e.extern:+standard-external-format+)
    (save-parse-cache s))
  (values))

(defmacro with-parse-cache-file ((file-form) &body body &aux (file-var (gensym "parse-cache-file-")))
  `(let ((,file-var ,file-form))
    (load-parse-cache-file ,file-var)
    (unwind-protect
      (progn ,@body)
      (save-parse-cache-file ,file-var))))

; --- ---

(def-vtable e-syntax-error
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "syntax error: ")
    (e. tw |print| (princ-to-string this))))
    
