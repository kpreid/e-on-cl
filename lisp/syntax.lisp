(in-package :e.elang.syntax)

; --- ---

(defun is-identifier (text)
  (query-to-java "isIdentifier" text))

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
        ;((#\Linefeed) (out "\\n"))
        ((#\\) (out "\\\\"))
        (otherwise
          (if (find char specials)
            (out (list #\\ char))
            (out (list char))))))
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
(defvar +e-printer+ (e-named-lambda "org.erights.e.elang.syntax.ePrinter"
  "Centralized object for generating E source and E-like text."
  :stamped +deep-frozen-stamp+
  
  (:|__printOn/1| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<E-syntax printer>")
    nil)
  
  (:|printDocComment/2| (tw text)
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
  
  (:|printVerb/2| (tw verb)
    "Print a verb with appropriate quoting, as in CallExpr or EMethod."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef verb 'string)
    (if (is-identifier verb)
      (e. tw |print| verb)
      (e. tw |quote| verb))
    nil)
   
  (:|printNoun/2| (tw noun)
    "Print a noun with appropriate quoting, as in NounExpr."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef noun 'string)
    (if (is-identifier noun)
      (e. tw |print| noun)
      (progn
        (e. tw |print| "::")
        (e. tw |quote| noun)))
    nil)
  
  (:|printPropertySlot/2| (tw prop-name)
    "The print representation of a property-slot object. Not parsable."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef prop-name 'string)
    (e. tw |print| "_::&")
    (if (is-identifier prop-name)
      (e. tw |write| prop-name)
      (e. tw |quote| prop-name)))
  
  (:|printString/2| (tw this)
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef this 'string)
    (e. tw |write| "\"")
    (print-escaped tw this "\"")
    (e. tw |write| "\""))
  
  (:|printCons/2| (tw this)
    "The print representation of a Lisp cons. Not parsable."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef this 'cons)
    ; XXX this is a lousy print syntax
    (e. tw |write| "< ")
    (e. tw |quote| (car this))
    (e. tw |write| " > + ")
    (e. tw |quote| (cdr this)))
  
  (:|printCharacter/2| (tw this)
    "The print representation of a character."
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef this 'character)
    ; XXX escape when appropriate
    (e. tw |write| "'")
    (print-escaped tw (vector this) "\'")
    (e. tw |write| "'"))
  
  (:|printList/3| (tw this quote-elements)
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
  
  (:|printMethodHeader/6| (tw is-kernel doc-comment verb params opt-result-guard)
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
  
  (:|printGuardedNounPattern/3| (tw opt-name opt-guard)
    (e-coercef tw +the-text-writer-guard+)
    (e-coercef opt-name '(or null string))
    (if opt-name
      (e. +e-printer+ |printNoun| tw opt-name)
      (e. tw |print| "_"))
    (when opt-guard
      (e. tw |print| " :")
      (e. tw |print| opt-guard)))

  (:|printExprBlock/2| (tw node)
    "Print the EExpr 'node' on 'tw' enclosed in braces."
    (e. tw |print| "{" #|}|#)
    (let ((tw2 (e. tw |indent|)))
      (e. tw2 |println|)
      (e. node |welcome| (e. +e-printer+ |makePrintENodeVisitor| tw +precedence-outer+)))
    (e. tw |lnPrint| #|{|# "}"))

  (:|makePrintENodeVisitor/1| (tw)
    (e. +e-printer+ |makePrintENodeVisitor| tw +precedence-outer+))
  
  (:|makePrintENodeVisitor/2| (tw precedence)
    "Return an ETreeVisitor which prints nodes to 'tw' in standard E syntax. 'precedence' controls whether the expression is parenthesized. 

XXX make precedence values available as constants"
    ; XXX have an in-quasi argument - print the LiteralExpr "$" as "$$", and the ${n} as ${n}
    ; XXX have a self argument for implementation inheritance
    ; strictness controls?
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
        (e-named-lambda "org.erights.e.elang.syntax.ePrinter$printVisitor"
          (:|__printOn/1| (ptw)
            (e-coercef ptw +the-text-writer-guard+)
            (e. ptw |print| "<E-syntax node visitor printing to " tw ">")
            nil)
          
          (:|visitAssignExpr/3| (opt-original noun-expr value-expr)
            (declare (ignore opt-original))
            (precedential (+precedence-assign+)
              (subprint noun-expr +precedence-in-assign-left+)
              (e. tw |print| " := ")
              (subprint value-expr +precedence-in-assign-right+)))
          
          (:|visitCallExpr/4| (opt-original rec verb args)
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
          
          (:|visitCatchExpr/4| (opt-original attempt catch-pattern catch-body)
            (declare (ignore opt-original))
            ; XXX don't call back to the node for printing
            (e. tw |print| "try ")
            (subprint-block attempt)
            (e. tw |print| " catch ")
            (subprint catch-pattern nil)
            (e. tw |print| " ")
            (subprint-block catch-body))
          
          (:|visitDefineExpr/3| (opt-original pattern specimen)
            (declare (ignore opt-original))
            (precedential (+precedence-define+)
              (e. tw |print| "def ")
              (subprint pattern nil)
              (e. tw |print| " := ")
              (subprint specimen +precedence-define-right+)))
          
          (:|visitEscapeExpr/5| (opt-original ejector-patt body catch-patt catch-body)
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
                    
          (:|visitFinallyExpr/3| (opt-original attempt unwinder)
            (declare (ignore opt-original))
            ; XXX don't call back to the node for printing
            (e. tw |print| "try ")
            (subprint-block attempt)
            (e. tw |print| " finally ")
            (subprint-block unwinder))
          
          (:|visitHideExpr/2| (opt-original body)
            (declare (ignore opt-original))
            (subprint-block body))
          
          (:|visitIfExpr/4| (opt-original cond true-block false-block)
            (declare (ignore opt-original))
            (e. tw |print| "if (" #|)|#)
            (subprint cond +precedence-outer+)
            (e. tw |print| #|(|# ") ")
            (subprint-block true-block)
            (when false-block
              (e. tw |print| " else ")
              (subprint-block false-block)))
          
          (:|visitLiteralExpr/2| (opt-original value)
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
          
          (:|visitMetaContextExpr/1| (opt-original)
            (declare (ignore opt-original))
            (e. tw |print| "meta.context()"))
          
          (:|visitMetaStateExpr/1| (opt-original)
            (declare (ignore opt-original))
            (e. tw |print| "meta.getState()"))
          
          (:|visitMatchBindExpr/3| (opt-original specimen pattern)
            (declare (ignore opt-original))
            (precedential (+precedence-matchbind+)
              (subprint specimen +precedence-matchbind-left+)
              (e. tw |print| " =~ ")
              (subprint pattern nil)))
          
          (:|visitNounExpr/2| (opt-original noun)
            (declare (ignore opt-original))
            (e. +e-printer+ |printNoun| tw noun))
          
          (:|visitObjectExpr/5| (opt-original doc-comment qualified-name auditors script)
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
          
          (:|visitSeqExpr/2| (opt-original subs)
            (declare (ignore opt-original))
            (precedential (+precedence-seq+)
              (loop for sep = "" then #\Newline
                    for sub across subs
                    do (e. tw |print| sep)
                       (subprint sub +precedence-in-seq+))))
          
          (:|visitSlotExpr/2| (opt-original noun)
            (declare (ignore opt-original))
            (precedential (+precedence-slot-expr+)
              (e. tw |print| "&")
              (subprint noun +precedence-in-slot-expr+)))
          
          (:|visitEScript/3| (opt-original opt-methods opt-matcher)
            (declare (ignore opt-original))
            ; XXX print patterns and opt-result-guard directly
            (if opt-methods
              (progn
                (e. tw |print| " {" #|}|#)
                (let ((indented (e. tw |indent|)))
                  (loop for method across opt-methods do
                    (e. indented |println|)
                    (subprint method nil :tw indented))
                (when opt-matcher
                  (e. indented |println|)
                  (subprint opt-matcher nil :tw indented)))
                (e. tw |lnPrint| #|{|# "}"))
              (progn
                (e. tw |print| " ")
                (subprint opt-matcher nil))))
            
          (:|visitEMethod/6| (opt-original doc-comment verb patterns opt-result-guard body)
            (declare (ignore opt-original))
            (e. tw |println|)
            (e. +e-printer+ |printMethodHeader| tw +e-true+
              doc-comment
              verb
              (map 'vector
                (lambda (pattern)
                  (e-lambda (:|__printOn/1| (tw)
                    (e-coercef tw +the-text-writer-guard+)
                    (subprint pattern +precedence-outer+ :tw tw))))
                patterns) 
              (when opt-result-guard
                (e-lambda (:|__printOn/1| (tw)
                  (e-coercef tw +the-text-writer-guard+)
                  (subprint opt-result-guard +precedence-in-guard+ :tw tw)))))
            (e. tw |print| " ")
            (subprint-block body))
          
          (:|visitEMatcher/3| (opt-original pattern body)
            (declare (ignore opt-original))
            (e. tw |print| "match ")
            (subprint pattern nil)
            (e. tw |print| " ")
            (subprint-block body))
          
          (:|visitCdrPattern/3| (opt-original list-patt rest-patt)
            (declare (ignore opt-original))
            (subprint list-patt nil)
            (e. tw |print| " + ")
            (subprint rest-patt nil))
          
          (:|visitFinalPattern/3| (noun-pattern-printer ""))
          (:|visitSlotPattern/3| (noun-pattern-printer "&"))
          (:|visitVarPattern/3| (noun-pattern-printer "var "))

          (:|visitIgnorePattern/1| (opt-original)
            (declare (ignore opt-original))
            (e. tw |print| "_"))
          
          (:|visitListPattern/2| (opt-original subpatts)
            (declare (ignore opt-original))
            (e. tw |print| "[" #|]|#)
            (loop for sep = "" then ", "
                  for patt across subpatts
                  do (e. tw |print| sep)
                     (subprint patt nil))
            (e. tw |print| #|[|# "]"))
          
          (:|visitSuchThatPattern/3| (opt-original subpatt condition)
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

(defvar *parse-cache-hash* (make-hash-table :test #'equal))

(defvar *parser-process*)

(defun start-parser ()
  (assert (not (boundp '*parser-process*)))
  ; XXX pipe this through common tracing architecture
  (format *trace-output* "~&; starting Java-E parsing subprocess~%")
  (setf *parser-process* (e.util:run-program
    "/usr/bin/env"
    (list "rune"
          "-J-XX:ThreadStackSize=10240"
          "-cpa" (namestring (merge-pathnames #P"jlib/" (asdf:component-pathname (asdf:find-system :cl-e))))
          "-De.onErrorExit=report"
          (format nil "-Dfile.encoding=~A" e.extern:+standard-external-format-common-name+)
          "-") 
    :input :stream
    :output :stream
    :error t
    :wait nil))
  (format (e.util:external-process-input-stream *parser-process*)
        "def parseEToSExpression := <import:parseEToSExpression>~%")
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
      (evm-node::|null| nil)
      (evm-node::|.tuple.|
        (map 'vector #'build-nodes (cdr tree)))
      (evm-node::|.DollarHole.|
        (make-instance 'evm-node::|QuasiLiteralNode| :elements
          (mapcar #'build-nodes (cdr tree))))
      (evm-node::|.AtHole.|
        (make-instance 'evm-node::|QuasiPatternNode| :elements
          (mapcar #'build-nodes (cdr tree))))
      ((evm-node::|ObjectExpr| evm-node::|EMethod|)
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
              (let ((*package* (find-package :evm-node)))
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
  (setf source (coerce arg '(vector character)))
  
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

(defun parse-to-expr (source)
  (query-to-java "run" source))

(defun e-source-to-tree (source &key syntax-ejector
    &aux (tree-expr (parse-to-expr source)))
  (if (eql (first tree-expr) 'evm-node::error)
    (error-from-e-error-string syntax-ejector (second tree-expr))
    (build-nodes tree-expr)))

(defvar +prim-parser+ (e-named-lambda
  "org.cubik.cle.prim.parser"
  :stamped +deep-frozen-stamp+
  (:|run/2| (source syntax-ejector)
    (e-source-to-tree source :syntax-ejector syntax-ejector))
  (:|run/1| (source)
    (e-source-to-tree source))))

; --- Parse cache files ---
 
(defun load-parse-cache (stream)
  ; XXX options to disable this output
  (format *trace-output* "~&; Loading parse cache from ~A..." (enough-namestring (pathname stream)))
  (force-output *trace-output*)
  (loop
    for (source tree) in
      (let ((*package* (find-package :evm-node))) (read stream))
    do (setf (gethash source *parse-cache-hash*) tree))
  (format *trace-output* "done~%")
  (values))
    
(defun save-parse-cache (stream)
  (format *trace-output* "~&; Writing parse cache to ~A..." (enough-namestring (pathname stream)))
  (force-output *trace-output*)
  (let ((*package* (find-package :evm-node)))
    (write
      (loop for source being each hash-key of *parse-cache-hash* using (hash-value tree)
        collect (list source tree))
      :stream stream
      :readably t))
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
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "syntax error: ")
    (e. tw |print| (princ-to-string this))))
    
