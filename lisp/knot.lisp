; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.knot)

(defglobals +e-ref-kit-slot+
            +the-make-path-loader+)

; --- direct E-to-Lisp interfaces and powers ---

; XXX look into whether e-lambda, now that it has matcher support, would be cleaner for implementing this
(defun wrap-function (f &key stamps)
  "Return an E function value corresponding to the given Lisp function value."
  (labels ((wrapper (mverb &rest args)
      (cond
        ((eql mverb (e-util:mangle-verb "run" (length args)))
          (apply f args))
        ((eql mverb (e-util:mangle-verb "tuple" (length args)))
          (multiple-value-call #'vector (apply f args)))
        (t
          (case mverb
            ; XXX provide getAllegedType
            ((:|__printOn/1|) (destructuring-bind (tw) args
              (e-coercef tw +the-text-writer-guard+)
              (e. tw |print| "<native function ")
              (e. tw |quote| 
                (nth-value 2
                  (function-lambda-expression
                    (etypecase f
                      (function f)
                      (t        (fdefinition f))))))
              (e. tw |print| ">")
              nil))
            ((:|__respondsTo/2|) (destructuring-bind (verb arity) args
              (e-coercef verb 'string)
              (e-coercef arity 'integer)
              (as-e-boolean
                (or (and (or (string= verb "run") 
                             (string= verb "tuple"))
                         (e-util:function-responds-to f arity))
                    (e-is-true (elib:miranda #'wrapper mverb args #'funcall))))))
            ((elib:audited-by-magic-verb) (destructuring-bind (auditor) args
              (not (not (find auditor stamps :test #'samep)))))
            (otherwise
              (elib:miranda #'wrapper mverb args #'funcall)))))))
    #'wrapper))

(defun make-symbol-accessor (symbol)
  "Return an E object providing access to the mutable properties of 'symbol'."
  (e-lambda "org.cubik.cle.prim.lisp$symbolAccessor" ()
    (:|__printOn| ((tw +the-text-writer-guard+))
      (e. tw |write| "<")
      (e. tw |print| 
        (with-standard-io-syntax
          (prin1-to-string symbol)))
      (e. tw |write| ">")
      nil)
    (:|asSymbol| ()
      "The symbol object itself."
      symbol)
    (:|get| ()
      "CL:SYMBOL-VALUE"
      (symbol-value symbol))
    ;; XXX a useful additional feature would be a version of getFunction which ref-shortens any arguments to the function; when we add this, review where it should be used
    (:|getFunction| ()
      "CL:SYMBOL-FUNCTION with a wrapper"
      (wrap-function (symbol-function symbol)))
    (:|put| (new)
      "(SETF CL:SYMBOL-VALUE)"
      (setf (symbol-value symbol) new)
      nil)))

(defobject +lisp+ "org.cubik.cle.prim.lisp"
    (:doc "This object is the maximum possible authority that an E program may hold, offering near-complete access to the underlying Lisp system. Handle with care.")
  (:|get| ((package-name 'string)
           (symbol-name 'string))
    ; xxx should allow ejectors/absent-thunks for missing package and symbol
    "Returns the named symbol in the named package. This is CL:FIND-SYMBOL, except that it throws an exception instead of returning nil, and takes the package argument first."
    (unless (find-package package-name)
      (error "the package ~S does not exist" package-name))
    (multiple-value-bind (symbol status) (find-symbol symbol-name package-name)
      (if status
        (make-symbol-accessor symbol)
        (error "the symbol ~S does not exist in ~S" symbol-name package-name))))
  (:|unsealingConditionGuard| (class-name)
    "Return a guard which accepts and unseals thrown (that is, SIGNALed) conditions of the given class."
    (e-lambda "$unsealingConditionGuard" 
        (:stamped +deep-frozen-stamp+)
      (:|coerce| (specimen opt-ejector)
        (let ((c (e-problem-unseal (ref-shorten specimen))))
          (if (typep c class-name)
            c
            (eject-or-ethrow opt-ejector
              (format nil "~A is not a ~S, even unsealed" (e-quote specimen) class-name))))))))

;;; --- miscellaneous ---

(defvar *antlr-jar*)

; --- emaker loading ---

(defun system-file (offset)
  (e.extern:pathname-to-file (merge-pathnames
                               offset
                               (asdf:component-pathname +the-asdf-system+))))

(defglobal +builtin-emaker-loader-desc+ 
  (list (system-file (make-pathname :directory '(:relative "lib"))) 
        (system-file (make-pathname :directory '(:relative "compiled-lib")))))

(defvar *emaker-search-list*
  (list +builtin-emaker-loader-desc+)
  "List of (list emaker-file-root opt-compiled-file-root). Changes will apply only to newly-created safe scopes.")

(defun found-e-on-java-home (dir-pathname)
  "Called (usually by clrune) to report the location of an E-on-Java installation."
  (setf *antlr-jar* (merge-pathnames #p"e.jar" dir-pathname))
  (setf *emaker-search-list* 
    (append *emaker-search-list*
      (list
        (list
          (handler-case
              (if (member :e.saving-image *features*)
                (error "saving an image")
                (funcall (system-symbol "OPEN-JAR" :e.jar :e-on-cl.jar) 
                         (merge-pathnames #p"e.jar" dir-pathname)))
            (error (c)
              (warn "Could not use e.jar because: ~A" c)
              (e.extern:pathname-to-file (merge-pathnames #p"src/esrc/" dir-pathname))))
          (second +builtin-emaker-loader-desc+))))))

(defun fqn-to-slash-path (fqn typoid)
  ;; XXX what to do with slashes in the input?
  (concatenate 'string (substitute #\/ #\. fqn) "." typoid))

;; CLISP has trouble with complex fasls - '*** - PRINT: not enough stack space for carrying out circularity analysis' - so we won't try to compile. XXX better to handle compilation failure?
(defconstant +compile-emakers+ #+clisp nil #-clisp t)

(defun scope-for-fqn (scope fqn)
  (let* ((&trace (make-instance 'e-simple-slot :value (make-tracer :label fqn)))
         (scope (e. scope |nestOuter|))
         (scope (e. scope |withPrefix| (concatenate 'string fqn "$")))
         (scope (e. scope |withSlot| "trace" &trace))
         (scope (e. scope |withSlot| "traceln" &trace))
         (scope (e. scope |nestOuter|)))
    scope))

;;; XXX arrange to also fall back to eval-e if we fail to compile or to load
(defun load-emaker-from-file (file fqn scope opt-compiled-file)
  (let* ((fqn-prefix (concatenate 'string fqn "$"))
         (compile-target (when opt-compiled-file (e.extern::file-ref-pathname opt-compiled-file)))
         (scope (scope-for-fqn scope fqn)))
    (e.rune::file-incorporated *vat* file (e. file |_clFileWriteDate|)
      #| XXX once we have proper filesystem change hiding, this operation should
         happen at exactly that point |#)
    (if (and +compile-emakers+ compile-target)
      (progn
        (let ((*standard-output* *trace-output*))
          (ensure-directories-exist compile-target :verbose nil))
        (when (or (not (probe-file compile-target))
                  ;; XXX fix this interface - exposing CL things not good
                  (> (e. file |_clFileWriteDate|)
                     (file-write-date compile-target)))
          ;; If we have a compile-target, then we have the authority
          ;; to write to it.
          (e.compiler:compile-e-to-file
            (e.syntax:parse-to-kernel (e. file |getTwine|))
            compile-target
            fqn-prefix
            scope))
        (e.compiler:load-compiled-e compile-target scope))
      (elang:eval-e (e.syntax:parse-to-kernel (e. file |getTwine|))
                    scope))))

(defglobal +emaker-fasl-type+ 
  (concatenate 'string 
    "emaker-"
    (pathname-type (compile-file-pathname #p"foo.lisp" #|eww|#))))

(defun load-function-for-root (source-file-root opt-compiled-file-root)
  (lambda (fqn absent-thunk scope)
    (e-coercef fqn 'string)
    (let* ((file (e. source-file-root |getOpt| (fqn-to-slash-path fqn "emaker"))))
      (if file
        (load-emaker-from-file file fqn 
                               scope
                               (when opt-compiled-file-root
                                 (eelt opt-compiled-file-root
                                   (fqn-to-slash-path fqn +emaker-fasl-type+))))
        (efuncall absent-thunk)))))

(defun make-load-function-from-desc (desc)
  (apply #'load-function-for-root desc))

(defglobal +builtin-emaker-loader+
  (destructuring-bind (source-file-root opt-compiled-file-root) +builtin-emaker-loader-desc+
    (let ((load-function (load-function-for-root source-file-root opt-compiled-file-root)))
      (e-lambda "$emakerLoader" ()
        (:|fetch| (fqn absent-thunk)
          (funcall load-function fqn absent-thunk (vat-safe-scope *vat*)))))))

;;; this is here because it's closely related to emaker loading
(defun make-resource-loader (search-list) 
  (e-lambda |resource__uriGetter|
      (:stamped +deep-frozen-stamp+)
    (:|get| (subpath)
      ;; XXX there should be a getter-shell for this common method - is path-loader fit for that?
      (e. |resource__uriGetter| |fetch| subpath
        (e-lambda "$getFailureThunk" () (:|run| () 
          (error "~A can't find ~A" (e-quote |resource__uriGetter|) (e-quote subpath))))))
    (:|fetch| ((subpath 'string) absent-thunk)
      (loop for (root) in search-list
            for file = (e. root |getOpt| subpath)
            when file
              return (e. file |readOnly|)
            finally (return (efuncall absent-thunk))))))


(defmacro prefix-scope ((fqn-form prefix) &body entries)
  (declare (string prefix))
  `(make-scope ,fqn-form (list
    ,@(loop for (name value-form) in entries collect
      `(list ,(concatenate 'string "&" prefix (string name))
             (e. +the-make-simple-slot+ |run| ,value-form))))))

(defmacro lazy-prefix-scope ((fqn-form prefix) &body entries)
  (declare (string prefix))
  `(make-scope ,fqn-form (list
    ,@(loop for (name value-form) in entries collect
      `(list ,(concatenate 'string "&" prefix (string name))
             (make-lazy-apply-slot (lambda () ,value-form)))))))

(defun selfless-authorize (fqn)
  "Perform the typical load and authorization for an E-implemented maker of selfless objects."
  (efuncall (e-import (concatenate 'string fqn "Author")) 
            elib:+selfless-stamp+))

(defglobal +shared-safe-loader+
  (prefix-scope ("org.cubik.cle.prim.sharedPrimLoader" "org.cubik.cle.prim.")
    ;; control/primitives
    (:|equalizer|        (make-equalizer)) ;; yes, equalizers are thread safe
    (:|E|                +the-e+)
    (:|gc|               e.extern:+gc+)
    (:|loop|             +the-looper+)
    (:|throw|            +the-thrower+)
    (:|makeProxy|        +the-make-proxy+)
    
    ;; exceptions
    (:|makeCoercionFailure| +the-make-coercion-failure+)
    (:|makeException|    +the-make-exception+)
    (:|makeStringException| +the-make-string-error+)
    (:|StructureException| (type-specifier-to-guard 'e-structure-exception))
    (:|Throwable|        elib:+the-exception-guard+)
    
    ;; simple makers
    (:|getCharacter|     +the-get-character+)
    (:|makeArray|        e.elib.tables:+the-make-array+)
    (:|makeConstList|    +the-make-list+)
    (:|makeFinalSlot|    +the-make-simple-slot+)
    (:|makeFlexMap|      elib:+the-make-flex-map+)
    (:|makeGuardedSlot|  +the-make-guarded-slot+)
    (:|makeScope|        +the-make-scope+)
    (:|makePriorityQueue| elib:+the-make-priority-queue+)
    (:|makeStaticScope|  elang:+the-make-static-scope+)
    (:|makeTextWriter|   elib:+the-make-text-writer+)
    (:|makeVarSlot|      +the-make-var-slot+)
    
    (:|makeTypeDesc|     +the-make-type-desc+)
    (:|makeMessageDesc|  +the-make-message-desc+)
    (:|makeParamDesc|    +the-make-param-desc+)
    
    ;; data guards
    (:|char|             (type-specifier-to-guard 'character))
    (:|ConstList|        (type-specifier-to-guard 'vector)) ; XXX and not string?
    (:|ConstMap|         elib:+the-map-guard+) ; used by nonprimitive map guard
    (:|float64|          (type-specifier-to-guard 'float64))
    (:|int|              (type-specifier-to-guard 'integer))

    ;; guards
    (:|PassByConstruction| elib:+pass-by-construction+)
    
    ;; tools
    (:|makeSafeScope|    (wrap-function 'make-safe-scope))    
    ))

(defglobal +sharable-importer+
  (let ((real-loader +shared-safe-loader+)) ; might become a path loader
    (e-lambda "org.cubik.cle.prim.SharableImporter"
        (:stamped +deep-frozen-stamp+
         :stamped +thread-sharable-stamp+ 
         :stamped +standard-graph-exit-stamp+)
      (:|__printOn| ((out +the-text-writer-guard+))
        (e. out |write| "<shared:*>"))
      (otherwise (mverb &rest args)
        (apply #'e-call-dispatch real-loader mverb args)))))

; xxx review which of these ought to be moved to safe-extern-scope
(defun make-primitive-loader ()
  (lazy-prefix-scope ("org.cubik.cle.prim.primLoader" "org.cubik.cle.prim.")
    (:|Ref|              (e. +e-ref-kit-slot+ |get|)) ; XXX reduce indirection
    
    (:|DeepFrozen|
      (efuncall (e. +builtin-emaker-loader+ |fetch| "org.erights.e.elib.serial.DeepFrozenAuthor" (e-lambda "org.erights.e.elib.prim.safeScopeDeepFrozenNotFoundThunk" () (:|run| () (error "DeepFrozenAuthor missing")))) 
                elib:+deep-frozen-stamp+))
    
    (:|makeBaseGuard|
      (efuncall (e-import "org.erights.e.elib.slot.makeBaseGuardAuthor") 
                elib:+deep-frozen-stamp+ elib:+selfless-stamp+))
    
    (:|makeBrand|
      (efuncall (e-import "org.erights.e.elib.sealing.makeBrandAuthor") 
                elib:+deep-frozen-stamp+))
    
    (:|memoize|
      (efuncall (e-import "org.cubik.cle.memoizeAuthor")
                elib:+deep-frozen-stamp+))))

(defglobal +selfless-maker-fqns+
  '("org.erights.e.elib.tables.makeConstSet"
    "org.erights.e.elib.slot.finalSlotGuardSugar"
    "org.quasiliteral.astro.makeAstroTag"
    "org.quasiliteral.term.makeTerm"))

(defun make-selfless-loader ()
  "Makes the loader for those makers which would be plain .emakers if they did not need the SelflessStamp."
  (make-scope "org.cubik.cle.prim.selflessMakers" 
    (mapcar (lambda (fqn)
              (list (concatenate 'string "&" fqn)
                    (make-lazy-apply-slot 
                      (lambda () (selfless-authorize fqn)))))
            +selfless-maker-fqns+)))

;; XXX consider rewriting this in terms of Evaluator#getKernelNodes?
(defglobal +vm-node-maker-importer+
  (let* ((prefixes '("org.erights.e.elang.evm.make")))
    (e-lambda "vm-node-maker-importer"
        (:stamped +deep-frozen-stamp+)
      (:|fetch| ((fqn 'string) absent-thunk)
        (let ((local-name (some (lambda (p) (without-prefix fqn p)) 
                                prefixes)))
          (if local-name
            (let* ((sym (find-symbol local-name :e.elang.vm-node)))
              (or (and sym
                       (get sym 'static-maker))
                  (efuncall absent-thunk)))
            (efuncall absent-thunk))))
      (:|optUnget| (specimen)
        ; XXX O(N) not good - have elang-nodes.lisp build a hash table of makers at load time
        (block opt-unget
          (do-symbols (node-type (find-package :e.elang.vm-node))
            (when (same-yet-p specimen (get node-type 'static-maker))
              (return-from opt-unget 
                (concatenate 'string (first prefixes) 
                                     (string node-type)))))
          nil)))))

;; XXX support optUnget
(defobject +vm-node-type-importer+ "vm-node-type-importer"
    (:stamped +deep-frozen-stamp+)
  (:|fetch| ((fqn 'string) absent-thunk)
    (let ((local-name (without-prefix fqn "org.erights.e.elang.evm.type.")))
      (if local-name
        (let* ((sym (find-symbol local-name :e.elang.vm-node)))
          (if sym
            (type-specifier-to-guard sym)
            (efuncall absent-thunk)))
        (efuncall absent-thunk)))))

(defun make-tracer (&key (label "UNDEFINED TRACE LABEL") (stream *trace-output*))
  (let* ((first-prefix (format nil "; ~A " (e-print label)))
         (rest-prefix (format nil "; ~v@T" (- (length first-prefix) 3))))
    (labels ((make-trace-writer ()
               ;; TextWriters are vat-specific
               (e. (e.elib::make-text-writer-to-cl-stream stream :autoflush t) |indent| rest-prefix)))
      (e-lambda |trace|
          (:stamped +deep-frozen-stamp+ #|XXX must remove this or make it conditional/privileged if we ever let the stream be user-suppliable|#)
        (:|run| (message)
          ; xxx use stream writer on *trace-output*?
          ; xxx Should we tag traceln according to the FQN-prefix of the safe scope, or in nested scopes such as emakers' FQN-prefix-scopes?
          (fresh-line stream)
          (let ((tw (make-trace-writer)))
            (e. tw |print| first-prefix "trace: ")
            (e. (e. tw |indent|         "       ") |print| message)
            (fresh-line stream))
          (force-output stream))
        (:|doing| (message thunk)
          ;; XXX make nested doings, other traces while doing, etc. look good
          (fresh-line stream)
          (let ((tw (make-trace-writer)))
            (e. tw |print| first-prefix "doing: ")
            (e. (e. tw |indent|         "       ") |print| message)
            (e. tw |write| "..."))
          (unwind-protect
            (efuncall thunk)
            (format stream "done.~%")))
        (:|runAsTurn| (thunk context-thunk)
          "Call the given thunk. If it throws, the exception is logged for debugging (unsealed), and a broken reference (sealed) is returned. If it ejects, no special handling is performed.
    
    If a log message is produced, context-thunk is run to produce a string describing the origin of the failure."
          (handler-case-with-backtrace
            (efuncall thunk)
            (error (condition backtrace)
              (efuncall |trace|
                (e-lambda nil ()
                  (:|__printOn| ((tw +the-text-writer-guard+))
                    (e. tw |print| "caught problem in ")
                    (e. tw |quote| (efuncall context-thunk))
                    (e. tw |print| ": " (e-print condition)))))
              (make-unconnected-ref
                (transform-condition-for-e-catch condition
                                                 :backtrace backtrace)))))))))

;; NOTE on threading: if CL streams (in particular *trace-output* which make-tracer defaults to) are not thread-safe in the implementation, then we'd need to make these non-global
(defglobal +trace+ (make-tracer :label "misc"))
(defglobal +sys-trace+ (make-tracer :label ""))

; XXX merge these two: make lazy-apply robust in the presence of failure, and make lazy-eval use lazy-apply with a particular thunk

(defun make-lazy-eval-slot (scope source &aux value this)
  (setf this (e-lambda "LazyEvalSlot"
      (:stamped +deep-frozen-stamp+) ; XXX this stamp is only appropriate when the resulting value is also DeepFrozen, so this maker is not safe
    (:|get| ()
      (when source
        (let ((source-here source))
          (multiple-value-bind (promise resolver) (make-promise)
            (setf value  promise
                  source nil)
            (e. resolver |resolve| (e. +sys-trace+ |runAsTurn|
              (efun ()
                (setf value (elang:eval-e (e.syntax:parse-to-kernel source-here) (e. scope |withPrefix| (format nil "~A<lazy-eval>$" (e. scope |getFQNPrefix|))))))
              (efun () (format nil "knot lazy eval of ~S" source)))))))
      value)
    (:|put| (new)
      (declare (ignore new))
      (error "not an assignable slot: ~A" (e-quote this)))
    (:|isFinal| () elib:+e-true+))))

(defun make-lazy-apply-slot (maker &aux value-box)
  (e-lambda "lazyApplySlot"
      (:stamped +deep-frozen-stamp+)
    (:|get| ()
      (unless value-box
        (multiple-value-bind (promise resolver) (make-promise)
          ; XXX doesn't handle failure
          (setf value-box (list promise))
          (unwind-protect
            (e. resolver |resolve| (funcall maker))
            (unless (ref-is-resolved promise)
              (setf value-box nil)))))
      (car value-box))
    (:|isFinal| () elib:+e-true+)))

;; XXX review the things put in safe-extern-loader vs. in shared-safe-loader; seems arbitrary

(defun make-safe-extern-loader ()
  (lazy-prefix-scope ("__cle_safe_extern" "")

    ("org.erights.e.elib.base.makeSourceSpan" e.elib:+the-make-source-span+)

    ("org.apache.oro.text.regex.makePerl5Compiler" e.extern:+rx-perl5-compiler+)
    ("org.apache.oro.text.regex.makePerl5Matcher"  e.extern:+rx-perl5-matcher+)
    
    ("org.cubik.cle.parser.makeLALR1Parser"
     (efuncall (e-import "org.cubik.cle.parser.makeLALR1ParserAuthor") 
               ;; XXX smaller authority: actually just wants access to cl-yacc
               +lisp+))
    
    ("org.cubik.cle.prim.parser"               e.syntax:+prim-parser+)
    ("org.cubik.cle.prim.ePrinter"             e.syntax:+e-printer+)
    ("org.cubik.cle.prim.makeFirstCharSplitter" +make-first-char-splitter+)
    ("org.cubik.cle.prim.makePathLoader"       +the-make-path-loader+)
    ("org.cubik.cle.prim.simplifyFQName" 
      (e-lambda "org.cubik.cle.prim.simplifyFQName" 
          (:stamped +deep-frozen-stamp+)
        (:|run| (x) (elib:simplify-fq-name (elib:e-coerce x 'string))))) ; XXX replace this with wrap-function
    ("org.cubik.cle.io.makeSocket"             e.streams:+the-make-socket+)
    ))

(defun f+ (f1 f2)
  (lambda (&rest args)
    (multiple-value-call f1 (apply f2 args))))

; XXX simplify the amount of wrapping this requires / make those of these primitives which are safe (all of them?) importable
(defglobal +e-ref-kit-slot+ (make-lazy-apply-slot (lambda ()
  (efuncall (e. +builtin-emaker-loader+ |fetch|
        "org.erights.e.elib.ref.RefAuthor" 
        (e-lambda "org.erights.e.elib.prim.RefAuthorNotFoundThunk" () (:|run| () (error "RefAuthor missing")))) 
    (wrap-function (f+ #'vector #'make-promise)
                   :stamps (list +deep-frozen-stamp+))
    (wrap-function #'ref-shorten
                   :stamps (list +deep-frozen-stamp+))
    (wrap-function #'ref-state
                   :stamps (list +deep-frozen-stamp+))
    (wrap-function (f+ #'as-e-boolean #'ref-is-resolved)
                   :stamps (list +deep-frozen-stamp+))
    (wrap-function #'make-unconnected-ref
                   :stamps (list +deep-frozen-stamp+))
    (wrap-function #'ref-opt-problem
                   :stamps (list +deep-frozen-stamp+))
    (wrap-function #'ref-opt-sealed-dispatch
                   :stamps (list +deep-frozen-stamp+))
    'elib:broken
    'elib:near
    'elib:eventual
    +deep-frozen-stamp+))))

(defun sameness-is-eq-p (specimen)
  ;; XXX unify this with fast-sameness-test
  (and (typep specimen 'function)
       (not (approvedp +selfless-stamp+ specimen))))

(defun make-caching-emaker-loader (source)
  (let* ((load-functions
           (mapcar #'make-load-function-from-desc source))
         (deep-frozen-cache (make-hash-table :test #'equal))
         (unget-table (tg:make-weak-hash-table :weakness :key :test #'eq)))
    (e-lambda "org.cubik.cle.internal.emakerImporter"
        (:stamped +deep-frozen-stamp+)
      (:|optUnget| (specimen)
        (setf specimen (ref-shorten specimen))
        (or (gethash specimen unget-table)
            (setf (gethash specimen unget-table) nil)))
      (:|fetch| ((fqn 'string) absent-thunk)
        (multiple-value-bind (cache-value cache-present) (gethash fqn deep-frozen-cache)
          (if cache-present
            cache-value
            (let* ((exit-stamp (e-lambda "org.cubik.cle.prim.ExitViaHereStamp"
                                   (:stamped +deep-frozen-stamp+)
                                 (:|audit/1| (constantly +e-true+))))
                   (scope (e. (vat-safe-scope *vat*) |withSlot| "ExitViaHere" (make-instance 'e-simple-slot :value exit-stamp)))
                   (result (loop for f in load-functions
                                 do (block continue
                                       (return (funcall f fqn (efun () (return-from continue)) scope)))
                                 finally (efuncall absent-thunk))))
              (when (e-is-true (e. (e. (vat-safe-scope *vat*) |fetch| "DeepFrozen" +the-thrower+) |isDeepFrozen| result))
                (setf (gethash fqn deep-frozen-cache) result)
                (when (and (approvedp exit-stamp result)
                           (sameness-is-eq-p result))
                  (if (nth-value 1 (gethash result unget-table))
                    (warn "Ruined unget-table entry for ~S (would have been ~S)" fqn result)
                    (setf (gethash result unget-table) fqn))))
              result)))))))

(defglobals +scope-to-right-invertible-loader+) ; defined in knot.eexpr

(defun default-safe-scope-roots ()
  (let* ((emaker-importer (make-caching-emaker-loader *emaker-search-list*)))
    (make-scope "__defaultSafeScopeRoots.thisShouldNotBeVisible$"
      `(("&import__uriGetter"  ,(make-lazy-apply-slot (lambda ()
          ; wrapper to provide stamped DeepFrozenness since we can't currently do it 'properly' without dependency cycles
          (let ((real-loader 
                  (efuncall +the-make-path-loader+ "import" (vector 
                    (efuncall +scope-to-right-invertible-loader+ +shared-safe-loader+) ; XXX unnecessarily repeating work
                    +sharable-importer+ ;; first so that anything the sharable importer contains is agreed upon by this
                    (make-primitive-loader)
                    (make-safe-extern-loader)
                    (make-selfless-loader)
                    emaker-importer
                    +vm-node-type-importer+
                    +vm-node-maker-importer+))))
            (e-lambda "org.cubik.cle.prim.ImportLoaderMagic"
                (:stamped +deep-frozen-stamp+)
              (:|__printOn| (tw) (e. real-loader |__printOn| tw))
              (otherwise (mverb &rest args)
                (apply #'e-call-dispatch real-loader mverb args)))))))))))

(defobject +validate-for+ "$__validateFor"
    (:stamped +deep-frozen-stamp+)
  (:|run| (flag)
    (unless (e-is-true flag)
      (error "For-loop body isn't valid after for-loop exits."))))

(defobject +standard-graph-exit+ "StandardGraphExit"
    (:stamped +deep-frozen-stamp+)
  ;; XXX should be a guard
  ;; XXX this name and protocol to be reviewed. used only to implement Data, so far
  (:|coerce/2| (standard-coerce 
                 (lambda (s) (approvedp +standard-graph-exit-stamp+ s))
                 (lambda () +standard-graph-exit+)))
  (:|is| (ref)
    (as-e-boolean (approvedp +standard-graph-exit-stamp+ ref))))

(defglobal +shared-safe-scope+
  (labels ((prim (name) (e. +shared-safe-loader+ |fetch| name +the-thrower+)))
    (make-scope "__shared"
      `(("shared__uriGetter"  ,+sharable-importer+)
      
        ; --- primitive: values not available as literals ---
        ; XXX true can be defined as (0 =~ _), and false as (!true) or (0 =~ []). Do we want to do so?
        ("null"       ,nil)
        ("false"      ,elib:+e-false+)
        ("true"       ,elib:+e-true+)
        ("NaN"        ,elib:|NaN|)
        ("Infinity"   ,elib:|Infinity|)

        ; --- primitive: guards ---
        ("any"        ,(type-specifier-to-guard 't))
        ("void"       ,elib:+the-void-guard+)

        ; --- primitive: flow control ---
        ("E"          ,(prim "org.cubik.cle.prim.E")) ; XXX should be interp.E
        ("throw"      ,(prim "org.cubik.cle.prim.throw")) ; XXX should be elang.interp.throw
        ("__loop"     ,(prim "org.cubik.cle.prim.loop")) ; XXX should be elang.interp.loop

        ; --- primitive: tracing ---
        ("trace"      ,+trace+)
        ("traceln"    ,+trace+)

        ; --- data constructors (shared) ---
        ("__makeFinalSlot"     ,elib:+the-make-simple-slot+)
        ("__makeVarSlot"       ,elib:+the-make-var-slot+)
        ("__makeGuardedSlot"   ,elib:+the-make-guarded-slot+)
        ("__makeInt"           ,elib:+the-make-int+)
        ("__makeList"          ,elib:+the-make-list+)
        ("__makeMap"           ,elib:+the-make-const-map+)
        ("__makeTwine"         ,elib:+the-make-twine+)

        ; --- data guards: atomic (shared) ---
        ; XXX should boolean be an ordered space?
        ("boolean"    ,(type-specifier-to-guard 'e-boolean))
        ("String"     ,(type-specifier-to-guard 'string))
        ("TextWriter" ,elib:+the-text-writer-guard+)
        ("Twine"      ,(type-specifier-to-guard 'elib:twine))

        ; --- primitive: reference conditions ---
        ("pbc"        ,elib:+pass-by-construction+)
        ;; XXX safeScope["StandardGraphExit"] is a lousy name for this
        ("StandardGraphExit" ,+standard-graph-exit+)

        ; --- primitive: reference operations (shared) ---        
        ("__auditedBy" ,+the-audit-checker+)
        ("__equalizer" ,(prim "org.cubik.cle.prim.equalizer")) ; XXX should be elib.tables.equalizer

        ; --- used by expansions ---
        ("__validateFor"    ,+validate-for+)
        
        )))
  "Environment containing only objects which are common across vats, and are therefore safe for sharing, file compilation, etc.")

(defun make-union-scope (prefix base desc)
  (e. (make-scope prefix desc) |or| base))

(defun make-safe-scope (&optional (fqn-prefix "__safe$") (roots (default-safe-scope-roots))
    &aux (&<import> (e. roots |fetchSlot| "import__uriGetter" +the-thrower+)))
  (with-result-promise (safe-scope-vow)
    (labels ((typical-lazy (source)
               (make-lazy-eval-slot safe-scope-vow source))
             (lazy-import (fqn)
               (make-lazy-apply-slot (lambda () (eelt (e. &<import> |get|) fqn)))))
      (make-union-scope fqn-prefix +shared-safe-scope+
      
        `(; --- self-referential / root ---
          ("safeScope"  ,safe-scope-vow)
          ("&import__uriGetter"  ,&<import>)
          
          ; --- primitive: reference operations (non-shared) ---        
          ("&Ref"        ,(lazy-import "org.cubik.cle.prim.Ref")) ; XXX should be elib.ref.Ref
          ("&DeepFrozen" ,(lazy-import "org.cubik.cle.prim.DeepFrozen")) ; XXX fqn

          ; --- nonprimitive flow control ---
          ("&require"   ,(lazy-import "org.erights.e.elang.interp.require"))
          
          ; --- data constructors (non-shared) ---
          ("&term__quasiParser"  ,(typical-lazy "<import:org.quasiliteral.quasiterm.makeQBuilder>.getTerm__quasiParser()"))
          ("&__makeOrderedSpace" ,(lazy-import "org.erights.e.elang.coord.OrderedSpaceMaker"))
      
          ; --- data guards: atomic (non-shared) ---
          ("&char"      ,(typical-lazy "__makeOrderedSpace(<import:org.cubik.cle.prim.char>, \"char\")"))    
          ("&float64"   ,(typical-lazy "__makeOrderedSpace(<import:org.cubik.cle.prim.float64>, \"float64\")"))
          ("&int"       ,(typical-lazy "__makeOrderedSpace(<import:org.cubik.cle.prim.int>, \"int\")"))
          
          ; --- data guards: nonatomic, nonprimitive ---
          ("&all"         ,(lazy-import "org.erights.e.elib.slot.makeIntersectionGuard"))
          ("&List"        ,(lazy-import "org.erights.e.elib.slot.List"))
          ("&Map"         ,(lazy-import "org.erights.e.elib.slot.Map"))
          ("&Not"         ,(lazy-import "org.erights.e.elib.slot.makeNegatedGuard"))
          ("&Set"         ,(typical-lazy "<import:org.erights.e.elib.tables.makeConstSet>.asType()"))
          ("&Tuple"       ,(lazy-import "org.erights.e.elib.slot.Tuple"))
          ("&__Portrayal" ,(typical-lazy "Tuple[any, String, List[any]]"))
  
          ; --- protocol/guard constructors ---
          ("&__makeMessageDesc"  ,(lazy-import "org.erights.e.elib.base.makeMessageDesc"))
          ("&__makeParamDesc"    ,(lazy-import "org.erights.e.elib.base.makeParamDesc"))
          ("&__makeProtocolDesc" ,(lazy-import "org.erights.e.elang.interp.makeProtocolDesc"))
          
          ; --- utility: guard meta ---
          ; The ValueGuard and Guard guards do not currently reject anything, but this may change (e.g. DeepFrozen)
          ("&Guard"       ,(lazy-import "org.erights.e.elib.slot.type.Guard"))
          ("&ValueGuard"  ,(lazy-import "org.erights.e.elib.slot.type.ValueGuard"))
          ("&__makeGuard" ,(typical-lazy "def stubMakeGuard(_) :any { return def stubBaseGuard {} }"))
      
          ; --- utility: guards ---
          ("&notNull"   ,(lazy-import "org.erights.e.elang.interp.notNull"))
          ("&nullOk"    ,(lazy-import "org.erights.e.elib.slot.nullOk"))
          
          ; --- utility: reference conditions ---
          ("&Data"       ,(lazy-import "org.erights.e.elib.serial.Data"))
          ("DeepPassByCopy" ,(make-unconnected-ref "DeepPassByCopy is not actually possible; use Data instead."))
          ("&near"       ,(lazy-import "org.erights.e.elib.slot.near"))
          ("&PassByCopy" ,(lazy-import "org.erights.e.elib.serial.PassByCopy"))
          ("&rcvr"       ,(lazy-import "org.erights.e.elang.interp.rcvr"))
          ("&vow"        ,(lazy-import "org.erights.e.elang.interp.vow"))
          
          ; --- E language ---
          ("&epatt__quasiParser" ,(lazy-import "org.erights.e.elang.syntax.epatt__quasiParser"))
          ("&e__quasiParser" ,(lazy-import "org.erights.e.elang.syntax.makeEParser"))
          ("EAudition" ,e.elang.compiler::+e-audition-guard+)
          ("__eval" ,e.elang.compiler::+the-evaluator+) ; XXX fix package
          
          ; --- utility: data ---
          ("&rx__quasiParser" ,(lazy-import "org.erights.e.elang.interp.makePerlMatchMaker"))
          ("&simple__quasiParser" ,(lazy-import "org.quasiliteral.text.simple__quasiParser"))
              
          ; --- utility: alias loaders ---
          ("&elang__uriGetter"  ,(lazy-import "org.erights.e.elang.*"))
          ("&elib__uriGetter"   ,(lazy-import "org.erights.e.elib.*"))
          ("&type__uriGetter"   ,(lazy-import "org.erights.e.elang.interp.typeLoader"))
          
          ; --- utility: EIO ---
          ("&EIO"               ,(lazy-import "org.erights.e.elib.eio.EIO"))

          ; --- utility: used by expansions ---
          ;; XXX these FQNs and scope nouns should be discussed
          ("&__bind"         ,(lazy-import "org.erights.e.elang.expand.makeViaBinder"))
          ("&__booleanFlow"  ,(lazy-import "org.erights.e.elang.expand.booleanFlow"))
          ("&__comparer"     ,(lazy-import "org.erights.e.elang.expand.comparer"))
          ("&__getPropertySlot" ,(lazy-import "org.cubik.cle.makeDefaultPropertySlot"))
          ("&__makeVerbFacet",(lazy-import "org.erights.e.elang.expand.__makeVerbFacet"))
          ("&__mapEmpty"     ,(lazy-import "org.erights.e.elang.expand.viaEmptyMap"))
          ("&__mapExtract"   ,(lazy-import "org.erights.e.elang.expand.makeViaExtractor"))
          ("&__matchSame"    ,(lazy-import "org.erights.e.elang.expand.makeViaSame"))
          ("&__quasiMatcher" ,(lazy-import "org.erights.e.elang.expand.makeViaQuasi"))
          ("&__splitList"    ,(lazy-import "org.erights.e.elang.expand.__splitList"))
          ("&__suchThat"     ,(lazy-import "org.erights.e.elang.expand.suchThat"))
          ("&__switchFailed" ,(lazy-import "org.erights.e.elang.expand.__switchFailed"))

          ; --- utility: miscellaneous ---
          ("&opaque__uriGetter" ,(lazy-import "org.erights.e.elib.serial.opaque__uriGetter"))
          ("&__identityFunc"    ,(typical-lazy "def identityFunc(x) :any { return x }"))
          
          ; --- XXX describe this category ---
          ;; XXX remove this special and use a parameter instead
          ("resource__uriGetter" ,(make-resource-loader *emaker-search-list*))
          
          ; --- user/REPL ---
          ("&help"              ,(lazy-import "org.erights.e.elang.interp.help")))))))

; XXX should use e-extern's pathname-to-E-style-path facilities
(defglobal +eprops+
  (e. +the-make-const-map+ |fromPairs| 
    `#(#("e.home" ,(namestring (asdf:component-pathname 
                                 +the-asdf-system+))))))

(defun make-io-scope (&key (interp nil interp-supplied) ((:stdout out-cl-stream)) ((:stderr error-cl-stream)))
  (let ((vat-priv-scope
          (e. (make-scope "__localPrivileged$"
                ;; XXX the comment on the next line is stale, because thread-sharability now exists. We need to review whether these authorities should be where they are.
                ; NOTE: these stamps are process-wide, but their effect is local unless amplified with some authority allowing sharing objects between vats (e.g. a hypothetical thread-safe stamp)
                `(("DeepFrozenStamp" ,elib:+deep-frozen-stamp+)
                  ("SelflessStamp"   ,elib:+selfless-stamp+)))
            |or| (vat-safe-scope *vat*))))
    (efuncall (e-import "org.cubik.cle.makeIOScope")
        "__privileged$"
        vat-priv-scope
        (make-scope "__ioPowers$"
          `(("timer"           ,e.extern:+the-timer+)
            ("&entropy"        ,(make-lazy-apply-slot (lambda ()
              (efuncall (e-import "org.cubik.cle.wrapRandomState") (make-random-state t)))))
            ("file__uriGetter" ,(e.extern:make-file-getter '#()))
            ("makeWeakRef"     ,+the-make-weak-ref+)
            ("unsafeNearSpawn"      ,e.extern:+spawn+)
            ("&stdin"     ,(make-lazy-apply-slot (lambda ()
                             (warn "making stdin")
                             (e. (eelt (e-import "org.cubik.cle.charsets") e.extern:+standard-external-format-common-name+)
                                 |decode|
                                (efuncall (efuncall (e-import "org.cubik.cle.io.makeFDInStreamAuthor")
                                   +lisp+) (e-lambda "stdin" ()) (e.streams:stream-to-fd-ref *standard-input* :input) 4096) (e. #() |asMap|)))))
            ("stdout"     ,(make-text-writer-to-cl-stream
                            out-cl-stream
                            :autoflush t
                            :should-close-underlying nil))
            ("stderr"     ,(make-text-writer-to-cl-stream
                            error-cl-stream
                            :autoflush t
                            :should-close-underlying nil))
            ("lisp"       ,+lisp+)
            ("makeVat"    ,+the-make-vat+)
            ("props"      ,+eprops+)
            ,@(when interp-supplied
              `(("interp" ,interp)))
            ("getSocketPeerRef"  ,e.streams:+the-get-socket-peer-ref+)
            ("getSocketLocalRef" ,e.streams:+the-get-socket-local-ref+)
            ("makePipe"          ,e.streams:+the-make-pipe+)
            #||#)))))

;;; --- end ---

#+sbcl (sb-ext:lock-package #.*package*)
