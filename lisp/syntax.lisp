; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang.syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :e.nonkernel)) ;; XXX remove once nonkernel package is defined before syntax package

'#.(when (member :abcl *features*) 
    (pushnew 'e.syntax::local-parser *features*))

; --- ---

(defun is-identifier (text)
  #-e.syntax::local-parser
  (query-to-java "isIdentifier" text)
  #+e.syntax::local-parser
  ;; XXX inadequate stub originally written to get the local-parser mode working
  (or (string= text "")
      (not (digit-char-p (aref text 0)))))

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
      (e. node |welcome| (e. +e-printer+ |makePrintENodeVisitor| tw2 +precedence-outer+)))
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
                  (lambda (opt-original noun-expr opt-guard-expr)
                    (declare (ignore opt-original))
                    (e. tw |print| tag)
                    (subprint noun-expr nil)
                    (when opt-guard-expr
                      (e. tw |print| " :")
                      (subprint-guard opt-guard-expr))))
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
                  (subprint opt-ejector +precedence-outer+)
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
            (if (string= qualified-name "_")
              ;; XXX is this condition correct?
              (e. tw |print| "_")
              (e. +e-printer+ |printString| tw qualified-name))
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
  
  ))

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
(defvar *parse-cache-hash* (make-hash-table :test #'equal))

#-e.syntax::local-parser
(defvar *parser-process*)

#-e.syntax::local-parser
(defun start-parser ()
  (assert (not (boundp '*parser-process*)))
  ; XXX pipe this through common tracing architecture
  (e. e.knot:+sys-trace+ |run| "starting Java-E parsing subprocess")
  (asdf:operate 'asdf:compile-op :e-on-cl.antlr-parser)
  (setf *parser-process* (e.util:run-program
    "/usr/bin/env" ;; XXX platform assumption - we should have a way of telling our portable-run-program to "search" with unspecified means
    (list "rune"
          "-J-XX:ThreadStackSize=10240"
          "-cpa" (native-namestring (merge-pathnames #P"jlib/" (asdf:component-pathname +the-asdf-system+)))
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

#-e.syntax::local-parser
(defun kill-parser ()
  (when (boundp '*parser-process*)
    (close (e.util:external-process-input-stream *parser-process*))
    (close (e.util:external-process-output-stream *parser-process*))
    (makunbound '*parser-process*)))

#-e.syntax::local-parser
(defun ensure-parser ()
  (unless (boundp '*parser-process*)
    (start-parser)))

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
        tree-expr)
    ((or end-of-file link-out-of-sync-error) (condition)
      (e. e.knot:+sys-trace+ |run| (format nil "error in parser communication: ~A (second time: ~A)" condition trying-again))
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
  (labels ((convert (x) (e-coercef x '(or string e-boolean null vector integer float64))
                        (typecase x
                          (string
                            (coerce x '(vector character)))
                          (vector
                            (map 'vector #'convert x))
                          (t x))))
    (setf args (mapcar #'convert args)))
  
  (let* ((key (list verb args)))
    (multiple-value-bind (cached present-p) (gethash key *parse-cache-hash*)
    (if present-p
      cached
      (e. e.knot:+sys-trace+ |doing|
        (format nil "query-to-java: ~A(~{~A...~^, ~})"
          verb (mapcar (lambda (arg) (if (stringp arg) (subseq arg 0 (position #\Newline arg)) arg)) 
                       args))
        (efun ()
          (let ((answer (call-to-java verb args)))
            (setf (gethash key *parse-cache-hash*) answer)
            answer)))))))

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
  (kernelize (apply #'e-source-to-tree source options)))

(defun e-source-to-tree (source &key syntax-ejector pattern quasi-info)
  (antlr-root 
    #-e.syntax::local-parser
      (query-or-die syntax-ejector "antlrParse" "unknown" source (as-e-boolean pattern) quasi-info)
    #+e.syntax::local-parser
      (handler-case
        (antlr-parse "unknown" source pattern nil)
        (error (c)
          (eject-or-ethrow syntax-ejector c)))))

(declaim (inline e-coercer))
(defun e-coercer (type-or-guard)
  (lambda (x) (e-coerce x type-or-guard)))

(defun map-e-list (function list)
  (map 'vector function (e-coerce list 'vector)))

(defun mapper-e-list (function)
  (lambda (x) (map-e-list function x)))

(defglobal +prim-parser+ (e-lambda "org.cubik.cle.prim.parser"
    (:stamped +deep-frozen-stamp+)
  (:|run| (source quasi-info syntax-ejector)
    (e-coercef source 'twine)
    (setf quasi-info (ref-shorten quasi-info))
    (when quasi-info
      ;; vector of vectors of integers ([valueHoles, patternHoles])
      (setf quasi-info 
        (map-e-list (mapper-e-list (e-coercer 'integer)) 
                    quasi-info)))
    (e-source-to-tree source :quasi-info quasi-info :syntax-ejector syntax-ejector))
  (:|run| (source)
    (e-coercef source 'twine)
    (e-source-to-tree source))
  (:|pattern| (source quasi-info syntax-ejector)
    (e-coercef source 'twine)
    (e-source-to-tree source :quasi-info quasi-info :syntax-ejector syntax-ejector :pattern t))))

;;; --- Antlr parser support ---

#+e.syntax::local-parser
(progn
  (defun jvector (element-type &rest elements)
    (jnew-array-from-array element-type (coerce elements 'vector)))
  (declaim (ftype function antlr-parse))
  (defglobal +ast-get-type+     (jmethod "antlr.collections.AST" "getType"))
  (defglobal +ast-get-text+     (jmethod "antlr.collections.AST" "getText"))
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
       (plumbing-token (jnew c-common-token -1 "plumbing"))
       (pattern-parse-method (jmethod "EParser" "pattern"))
       (expr-parse-method (jmethod "EParser" "start")))
  (defun antlr-parse (fname text is-pattern is-quasi)
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
        (jcall e-parser-set-pocket parser plumbing-token "enable")
        (jcall (if is-pattern
                 pattern-parse-method
                 expr-parse-method)
               parser)
        (nodes-to-list (jcall get-ast parser))))))

#-e.syntax::local-parser
(defmacro ast-node-bind ((tag-var text-var children-var) node-var &body body)
  `(destructuring-bind ((,tag-var ,text-var) &rest ,children-var) ,node-var
     ,@body))

#+e.syntax::local-parser
(defmacro ast-node-bind ((tag-var text-var children-var) node-var &body body)
  `(let* ((,tag-var  (intern (jarray-ref +token-names+
                                         (jcall +ast-get-type+ ,node-var))
                             :e.grammar))
          (,text-var (jcall +ast-get-text+ ,node-var))
          (,children-var (nodes-to-list (jcall +ast-first-child+ ,node-var))))
     ,@body))

(defun mn (name &rest elements)
  (make-instance name :elements elements))

(defun is-legitimate-toplevel-node (node)
  (if (typep node '(or |EExpr| |Pattern|))
    node
    (error "Non-expression/pattern ~S leaked out of antlr builder" node)))

(defun antlr-root (ast-nodes)
  (let ((enodes (map 'list #'is-legitimate-toplevel-node (map 'vector #'build-antlr-nodes ast-nodes))))
    (etypecase (length enodes)
      ((integer 1 1)
        (first enodes))
      ((integer 2)  
        (apply #'mn '|SeqExpr| enodes)))))

(defun build-antlr-nodes (ast-node &key path enclosing-doc-comment)
  (ast-node-bind (tag text in-children) ast-node
    (let* ((next-path (cons ast-node path))
           (out-children 
             (unless (eql tag 'e.grammar::|DocComment|)
               (mapcar (lambda (c) (build-antlr-nodes c :path next-path))
                       in-children))))
     (labels ((make-from-tag (&rest elements)
                (make-instance (or (find-symbol (symbol-name tag) :e.kernel)
                                   (find-symbol (symbol-name tag) :e.nonkernel))
                               :elements elements))
              (pass () (apply #'make-from-tag out-children)))
      (case tag
        ;; -- no special treatment --
        ((e.grammar::|AssignExpr|
          e.grammar::|AccumExpr|
          e.grammar::|AccumPlaceholderExpr|
          e.grammar::|CallExpr|
          e.grammar::|CoerceExpr|
          e.grammar::|CurryExpr|
          e.grammar::|ForExpr|
          e.grammar::|ForwardExpr|
          e.grammar::|FunCallExpr|
          e.grammar::|FunSendExpr|
          e.grammar::|GetExpr|
          e.grammar::|HideExpr|
          e.grammar::|IfExpr|
          e.grammar::|If1Expr|
          e.grammar::|ListExpr|
          e.grammar::|LiteralExpr|
          e.grammar::|MatchBindExpr|
          e.grammar::|MessageDescExpr|
          e.grammar::|MetaContextExpr|
          e.grammar::|MetaStateExpr|
          e.grammar::|MismatchExpr|
          e.grammar::|ModPowExpr|
          e.grammar::|NKAssignExpr|
          e.grammar::|PropertyExpr|
          e.grammar::|PropertySlotExpr|
          e.grammar::|QuasiExpr|
          e.grammar::|QuasiExprHole|
          e.grammar::|QuasiParserExpr|
          e.grammar::|QuasiPatternHole|
          e.grammar::|SendExpr|
          e.grammar::|SlotExpr|
          e.grammar::|SwitchExpr|
          e.grammar::|ParamDescExpr|
          e.grammar::|URIExpr|
          e.grammar::|WhenExpr|
          e.grammar::|WhenFnExpr|
          e.grammar::|WhileExpr|
          e.grammar::|MapPattern|
          e.grammar::|MapPatternAssoc|
          e.grammar::|MapPatternImport|
          e.grammar::|MapPatternOptional|
          e.grammar::|MapPatternRequired|
          e.grammar::|ListPattern|
          e.grammar::|QuasiPattern|
          e.grammar::|SuchThatPattern|
          e.grammar::|TailPattern|
          e.grammar::|EMethod|
          e.grammar::|ETo|
          e.grammar::|EMatcher|
          
          e.grammar::|QuasiLiteralExpr|
          e.grammar::|QuasiLiteralPattern|
          e.grammar::|QuasiPatternExpr|
          e.grammar::|QuasiPatternPattern|)
          (pass))
      
        ;; -- misc. leaves and 'special' nodes --
        ((e.grammar::|IDENT|) 
          (assert (null out-children))
          text)
        ((e.grammar::|DOC_COMMENT|) 
          (assert (null out-children))
          ; XXX the lexer should do this stripping
          (if (string= text "")
            ""
            (string-trim " 	
" (subseq text 3 (- (length text) 2)))))
        ((e.grammar::|DocComment|) 
          (destructuring-bind (comment-node body) in-children
            (build-antlr-nodes body 
              :path next-path 
              :enclosing-doc-comment (build-antlr-nodes comment-node
                                       :path next-path))))
        ((e.grammar::|INT|) 
          (assert (null out-children))
          (let ((*read-eval* nil))
            (read-from-string text)))
        ((e.grammar::|HEX|)
          (assert (null out-children))
          (let ((*read-eval* nil)
                (*read-base* 16))
            ;; XXX some input validation wouldn't hurt here
            (read-from-string text)))
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

        ;; -- special-purpose branches ---
        ((e.grammar::|List| e.grammar::|Implements|)
          (coerce out-children 'vector))
        (e.grammar::|Extends|
          (destructuring-bind (&optional extends) out-children
            extends))
        (e.grammar::|"catch"| 
          (destructuring-bind (pattern body) out-children
            (mn '|EMatcher| pattern body)))
        (e.grammar::|Assoc|
          (list* 'assoc out-children))
        (e.grammar::|Export|
          ;; XXX kpreid expects this to go away by handling MapExpr the same way as MapPattern
          (list* 'export out-children))
        
        ;; -- node type de-confusion --  
        (e.grammar::|UpdateExpr|
          (mn '|UpdateExpr| 
            (destructuring-bind (target verboid &rest args) out-children
              (etypecase verboid
                ((cons (eql update-op)) (mn '|BinaryExpr| (cdr verboid) target (coerce args 'vector)))
                (string (apply #'mn '|CallExpr| target verboid args))))))
        
        ;; -- operator handling --        
        (e.grammar::|PrefixExpr|
          (destructuring-bind (op arg) out-children
            (check-type op (cons (eql op) string))
            (mn '|PrefixExpr| (cdr op) arg)))
        ((e.grammar::|BinaryExpr|)
          (destructuring-bind (left &rest rights) out-children
            ;; GRUMBLE: either .., ..! should be marked as RangeExpr by the parser or CompareExpr should be another kind of BinaryExpr
            (mn '|BinaryExpr| text left (coerce rights 'vector))))

        ;; -- text introduction --
        ((e.grammar::|CompareExpr| 
          e.grammar::|ConditionalExpr| 
          e.grammar::|ExitExpr|)
          (apply #'make-from-tag text out-children))

        ;; -- doc-comment introduction --
        ((e.grammar::|ThunkExpr|)
          (apply #'make-from-tag (or enclosing-doc-comment "") out-children))

        ;; -- de-optioning --
        ((e.grammar::|IgnorePattern|)
          (destructuring-bind (&optional guard) out-children
            (if guard
              #+(or) (mn '|AcceptsPattern|)
              (error "reserved syntax: anon guard") ;; XXX source position in error
              (mn '|IgnorePattern|)))) 
        (e.grammar::|DefrecExpr|
          (destructuring-bind (l r &optional ejector) out-children
            (mn '|DefrecExpr| l r ejector)))
        (e.grammar::|EscapeExpr|
          (destructuring-bind (patt body &optional catcher) out-children
            (mn '|EscapeExpr| 
              patt body 
              (when catcher (e. catcher |getPattern|)) 
              (when catcher (e. catcher |getBody|)))))
        ((e.grammar::|FinalPattern| e.grammar::|SlotPattern| e.grammar::|VarPattern| e.grammar::|BindPattern|)
          (destructuring-bind (noun &optional guard) out-children
            (make-from-tag noun guard)))

        ;; -- negated-operator introduction --
        ((e.grammar::|SameExpr|)
          (destructuring-bind (left right) out-children
            (mn '|SameExpr| left right (ecase (find-symbol text :keyword)
                                         (:!= +e-true+)
                                         (:== +e-false+)))))
        ((e.grammar::|SamePattern|)
          (destructuring-bind (value) out-children
            (mn '|SamePattern| value (ecase (find-symbol text :keyword)
                                       (:!= +e-true+)
                                       (:== +e-false+)))))

        ;; -- other --
        (e.grammar::|SeqExpr|
          ;; XXX this is more like an expansion issue
          (if (null out-children)
            (mn '|NullExpr|)
            (apply #'mn '|SeqExpr| out-children)))
        (e.grammar::|NounExpr|
          (destructuring-bind (noun) out-children
            (etypecase noun
              (string (mn '|NounExpr| noun))
              ((cons (eql e.grammar::|URIGetter|) t)
                (mn '|URISchemeExpr| (cdr noun))))))
        (e.grammar::|MapExpr|
          ;; needs processing of its children
          (apply #'make-from-tag
            (loop for item in out-children collect
                  ;; XXX the expander should be doing most of this instead
                    (etypecase item
                      ((cons (eql assoc) (cons t (cons t null)))
                        (destructuring-bind (key value) (rest item)
                          (mn '|ListExpr| key value)))
                      ((cons (eql export) (cons t null))
                        (destructuring-bind (key-value) (rest item)
                          (mn '|ListExpr|
                            (mn '|LiteralExpr|
                              (etypecase key-value
                                (|NounExpr| (e. key-value |getName|))
                                (|SlotExpr| (format nil "&~A" (e. (e. key-value |getNoun|) |getName|)))))
                            key-value)))
                      (e.nonkernel.impl::|MapPatternPart|
                        item)))))
        (e.grammar::|TryExpr|
          (destructuring-bind (expr &rest stuff) out-children
            (loop for thing in stuff do
              (cond 
                ((typep thing '|EMatcher|)
                  (setf expr (mn '|CatchExpr| expr (e. thing |getPattern|) (e. thing |getBody|))))
                ((typep thing '|EExpr|)
                  ;; GRUMBLE: this is a finally block, and it isn't marked
                  (setf expr (mn '|FinallyExpr| expr thing)))
                (t
                  (error "unexpected TryExpr element: ~S" thing))))
            expr))
        ((e.grammar::|InterfaceExpr|)
          (destructuring-bind (name &rest rest) out-children
            (apply #'mn '|InterfaceExpr| 
              (or enclosing-doc-comment "")
              (if (typep name 'string)
                (mn '|LiteralExpr| name)
                name)
              rest)))

        ;; -- object-expr stuff --
        ((e.grammar::|ObjectExpr|)
          (destructuring-bind (qualified-name (extends implements script)) out-children
            (mn '|NKObjectExpr| (or enclosing-doc-comment "") qualified-name extends implements script)))
        ((e.grammar::|MethodObject|)
          (destructuring-bind (extends implements script) out-children
            (list extends implements script)))
        ((e.grammar::|FunctionObject|)
          (destructuring-bind (parameters result-guard body is-easy-return) out-children
            (list nil #() (mn '|FunctionScript| (coerce parameters 'vector) 
                                                result-guard body
                                                is-easy-return))))
        ((e.grammar::|PlumbingObject|)
          (destructuring-bind (implements matcher) out-children
            (list nil implements (mn '|EScript| nil (vector matcher)))))
        ((e.grammar::|EScript|)
          (let ((todo out-children))
            (let ((methods '())
                  (matchers '()))
              (loop while (typep (first todo) '(or |ETo| |EMethod|)) do
                (push (pop todo) methods))
              (loop while (typep (first todo) '|EMatcher|) do
                (push (pop todo) matchers))
              (when todo
                (error "unexpected element type or misplaced method in EScript: ~S remaining (whole is ~S)" todo out-children))
              (mn '|EScript| (coerce (nreverse methods) 'vector)
                             (coerce (nreverse matchers) 'vector)))))

        ;; -- quasi stuff --
        ((e.grammar::|QUASIBODY|) 
          (assert (null out-children))
          ;; XXX parser or this level needs to unescape quasi syntax escapes
          (mn '|QuasiText| text))
        ((e.grammar::|QuasiLiteralExpr|)
          (apply #'mn '|QuasiExpr| out-children))
        
        (otherwise
          (cond
            ((and (string= (aref (symbol-name tag) 0) "\"") 
                  (string= (aref (symbol-name tag) (- (length (symbol-name tag)) 2)) "=")
                  (null out-children))
              (cons 'update-op (subseq text 0 (1- (length text)))))
            ((and (string= (aref (symbol-name tag) 0) "\"") 
                  (string= (aref (symbol-name tag) (- (length (symbol-name tag)) 1)) "\"")
                  (null out-children))
              (cons 'op text))
            (t (error "don't know what to do with ~S <- ~A" ast-node (write-to-string path :length 5))))))))))

; --- Parse cache files ---
 
(defun load-parse-cache (stream)
  ; XXX options to disable this output
  (e. e.knot:+sys-trace+ |doing| 
    (format nil "Loading parse cache from ~A" (enough-namestring (pathname stream)))
    (efun ()
      (loop
        for (source tree) in
          (with-standard-io-syntax
            (let ((*package* (find-package :e.elang.vm-node))) 
              (read stream)))
        do (setf (gethash source *parse-cache-hash*) tree))
      (values))))
    
(defun save-parse-cache (stream)
  (e. e.knot:+sys-trace+ |doing| 
    (format nil "Writing parse cache to ~A" (enough-namestring (pathname stream)))
    (efun ()
      (let ((data (loop for source being each hash-key of *parse-cache-hash* using (hash-value tree)
                        collect (list source tree))))
        (with-standard-io-syntax
          (let ((*package* (find-package :e.elang.vm-node)))
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
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "syntax error: ")
    (e. tw |print| (princ-to-string this))))
    
