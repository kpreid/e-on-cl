; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.knot)

(defglobals +e-ref-kit-slot+)

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
              (e. tw |quote| (nth-value 2 (function-lambda-expression f)))
              (e. tw |print| ">")
              nil))
            ((:|__respondsTo/2|) (destructuring-bind (verb arity) args
              (e-coercef verb 'string)
              (e-coercef arity 'integer)
              (as-e-boolean
                (or (and (or (string= verb "run") 
                             (string= verb "tuple"))
                         (e-util:function-responds-to f arity))
                    (e-is-true (elib:miranda #'wrapper mverb args nil))))))
            ((elib:audited-by-magic-verb) (destructuring-bind (auditor) args
              (not (not (find auditor stamps :test #'samep)))))
            (otherwise
              (elib:miranda #'wrapper mverb args (lambda ()
                (error "no such method on wrapped function: ~A" mverb)))))))))
    #'wrapper))

(defun make-symbol-accessor (symbol)
  "Return an E object providing access to the mutable properties of 'symbol'."
  (e-lambda "org.cubik.cle.prim.lisp$symbolAccessor" ()
    (:|__printOn| (tw)
      (e-coercef tw +the-text-writer-guard+)
      (e. tw |write| "<")
      (e. tw |print| 
        (with-standard-io-syntax
          (prin1-to-string symbol)))
      (e. tw |write| ">")
      nil)
    (:|getValue| ()
      "CL:SYMBOL-VALUE"
      (symbol-value symbol))
    ;; XXX a useful additional feature would be a version of getFunction which ref-shortens any arguments to the function; when we add this, review where it should be used
    (:|getFunction| ()
      "CL:SYMBOL-FUNCTION with a wrapper"
      (wrap-function (symbol-function symbol)))
    (:|setValue| (new)
      "(SETF CL:SYMBOL-VALUE)"
      (setf (symbol-value symbol) new)
      nil)))

(defglobal +lisp+ (e-lambda "org.cubik.cle.prim.lisp"
    (:doc "This object is the maximum possible authority that an E program may hold, offering near-complete access to the underlying Lisp system. Handle with care.")
  (:|get| (package-name symbol-name)
    ; xxx should allow ejectors/absent-thunks for missing package and symbol
    "Returns the named symbol in the named package. This is CL:FIND-SYMBOL, except that it throws an exception instead of returning nil, and takes the package argument first."
    (e-coercef package-name 'string)
    (e-coercef symbol-name 'string)
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
              (format nil "~A is not a ~S, even unsealed" (e-quote specimen) class-name)))))))))

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
              (funcall (system-symbol "OPEN-JAR" :e.jar :e-on-cl.jar) 
                       (merge-pathnames #p"e.jar" dir-pathname))
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
  (let* ((trace (make-tracer :label fqn))
         (scope (e. scope |nestOuter|))
         (scope (e. scope |withPrefix| (concatenate 'string fqn "$")))
         (scope (e. scope |with| "trace" trace))
         (scope (e. scope |with| "traceln" trace))
         (scope (e. scope |nestOuter|)))
    scope))

;;; XXX arrange to also fall back to eval-e if we fail to compile or to load
(defun load-emaker-from-file (file fqn scope opt-compiled-file)
  (let* ((fqn-prefix (concatenate 'string fqn "$"))
         (compile-target (when opt-compiled-file (e.extern::file-ref-pathname opt-compiled-file)))
         (scope (scope-for-fqn scope fqn)))
    (if (and +compile-emakers+ compile-target)
      (progn
        (let ((*standard-output* *trace-output*))
          (ensure-directories-exist compile-target :verbose t))
        (when (or (not (probe-file compile-target))
                  ;; XXX fix this interface and remove the respondsTo test
                  (and (e-is-true (e. file |__respondsTo| "_clFileWriteDate" 0))
                       (> (e. file |_clFileWriteDate|)
                          (file-write-date compile-target))))
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

(defun make-emaker-loader (source-file-root opt-compiled-file-root scope-fn)
  (e-lambda "$emakerLoader" ()
    (:|fetch| (fqn absent-thunk)
      (e-coercef fqn 'string)
      (let* ((file (e. source-file-root |getOpt| (fqn-to-slash-path fqn "emaker"))))
        (if file
          (load-emaker-from-file file fqn 
                                 (funcall scope-fn) 
                                 (when opt-compiled-file-root
                                   (e. opt-compiled-file-root |get|
                                     (fqn-to-slash-path fqn +emaker-fasl-type+))))
          (e. absent-thunk |run|))))))

(defun make-emaker-loader-from-desc (desc)
  (destructuring-bind (emaker-root compiled-root) desc
    (make-emaker-loader emaker-root compiled-root (lambda () (vat-safe-scope *vat*)))))

;; XXX threading: review for state
(defglobal +builtin-emaker-loader+
  (make-emaker-loader-from-desc +builtin-emaker-loader-desc+))

;;; this is here because it's closely related to emaker loading
(defun make-resource-loader (search-list) 
  (e-lambda |resource__uriGetter|
      (:stamped +deep-frozen-stamp+)
    (:|get| (subpath)
      ;; XXX there should be a getter-shell for this common method - is path-loader fit for that?
      (e. |resource__uriGetter| |fetch| subpath
        (e-lambda "$getFailureThunk" () (:|run| () 
          (error "~A can't find ~A" (e-quote |resource__uriGetter|) (e-quote subpath))))))
    (:|fetch| (subpath absent-thunk)
      (e-coercef subpath 'string)
      (loop for (root) in search-list
            for file = (e. root |getOpt| subpath)
            when file
              return (e. file |readOnly|)
            finally (return (e. absent-thunk |run|))))))



(defmacro lazy-value-scope ((fqn-form prefix) &body entries)
  (declare (string prefix))
  `(make-scope ,fqn-form (list
    ,@(loop for (name value-form) in entries collect
      `(list ,(concatenate 'string "&" prefix (string name))
             (make-lazy-apply-slot (lambda () ,value-form)))))))

(defun selfless-authorize (fqn)
  "Perform the typical load and authorization for an E-implemented maker of selfless objects."
  (e. (e-import (concatenate 'string fqn "Author")) 
      |run| elib:+selfless-stamp+))

; xxx review which of these ought to be moved to safe-extern-scope
(defun make-primitive-loader ()
  (lazy-value-scope ("org.cubik.cle.prim.primLoader" "org.cubik.cle.prim.")
    (:|loop|             +the-looper+)
    (:|throw|            +the-thrower+)

    (:|makeException|    +the-make-exception+)
    (:|StructureException| (make-instance 'cl-type-guard :type-specifier 'e-structure-exception))
    (:|makeCoercionFailure| e.elib::+the-make-coercion-failure+)
  
    (:|makeFinalSlot|    +the-make-simple-slot+)
    (:|makeVarSlot|      +the-make-var-slot+)

    (:|getCharacter|     +the-get-character+)
    
    (:|int|              (make-instance 'cl-type-guard :type-specifier 'integer))
    (:|float64|          (make-instance 'cl-type-guard :type-specifier 'float64))
    (:|char|             (make-instance 'cl-type-guard :type-specifier 'character))
    
    (:|ConstList|        (make-instance 'cl-type-guard :type-specifier 'vector)) ; XXX and not string?
    (:|ConstMap|         elib:+the-map-guard+) ; used by nonprimitive map guard
    (:|makeScope|        +the-make-scope+)
    (:|makeStaticScope|  elang:+the-make-static-scope+)
    (:|makeSafeScope|    (wrap-function #'make-safe-scope))
    (:|makeTextWriter|   elib:+the-make-text-writer+)
    
    (:|makeTypeDesc|     +the-make-type-desc+)
    (:|makeMessageDesc|  +the-make-message-desc+)
    (:|makeParamDesc|    +the-make-param-desc+)

    (:|makeArray|        e.elib.tables:+the-make-array+)
    (:|makeFlexMap|      elib:+the-make-flex-map+)
    (:|makeSortedQueue|  elib:+the-make-sorted-queue+)
    
    (:|FlexList|         elib:+the-flex-list-guard+)
    (:|Throwable|        elib:+the-exception-guard+)
    
    (:|equalizer|        (make-equalizer))
    (:|E|                elib:+the-e+)
    (:|Ref|              (e. +e-ref-kit-slot+ |getValue|)) ; XXX reduce indirection
    
    (:|DeepFrozen|
      (e. (e. +builtin-emaker-loader+ |fetch| "org.erights.e.elib.serial.DeepFrozenAuthor" (e-lambda "org.erights.e.elib.prim.safeScopeDeepFrozenNotFoundThunk" () (:|run| () (error "DeepFrozenAuthor missing")))) 
          |run| 
          elib:+deep-frozen-stamp+))
    
    (:|makeBaseGuard|
      (e. (e-import "org.erights.e.elib.slot.makeBaseGuardAuthor") 
          |run| elib:+deep-frozen-stamp+ elib:+selfless-stamp+))
    
    (:|makeBrand|
      (e. (e-import "org.erights.e.elib.sealing.makeBrandAuthor") 
          |run| elib:+deep-frozen-stamp+))
    
    (:|memoize|
      (e. (e-import "org.cubik.cle.memoizeAuthor")
          |run| elib:+deep-frozen-stamp+))))

(defglobal +selfless-maker-fqns+
  '("org.erights.e.elib.tables.makeConstSet"
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

(defglobal +vm-node-maker-importer+
  (let* ((prefixes '("org.erights.e.elang.evm.make"
                     "org.erights.e.elang.evm.")))
    (e-lambda "vm-node-maker-importer"
        (:stamped +deep-frozen-stamp+)
      (:|fetch| (fqn absent-thunk)
        (e-coercef fqn 'string)
        (let ((local-name (some (lambda (p) (without-prefix fqn p)) 
                                prefixes)))
          (if local-name
            (let* ((sym (find-symbol local-name :e.elang.vm-node)))
              (or (and sym
                       (get sym 'static-maker))
                  (e. absent-thunk |run|)))
            (e. absent-thunk |run|))))
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
(defglobal +vm-node-type-importer+ (e-lambda "vm-node-type-importer"
    (:stamped +deep-frozen-stamp+)
  (:|fetch| (fqn absent-thunk)
    (e-coercef fqn 'string)
    (let ((local-name (without-prefix fqn "org.erights.e.elang.evm.type.")))
      (if local-name
        (let* ((sym (find-symbol local-name :e.elang.vm-node)))
          (if sym
            (make-instance 'cl-type-guard :type-specifier sym)
            (e. absent-thunk |run|)))
        (e. absent-thunk |run|))))))

; XXX move this utility function elsewhere
; XXX avoiding loading deep-frozen auditor because we need this during the construction of the path loader - so we reject some we could accept. the Right Solution is to add lazy auditing as MarkM suggested
(defun deep-frozen-if-every (subs
    &aux #-(and) (deep-frozen-guard (e. (vat-safe-scope *vat*) |get| "DeepFrozen")))
  (if (every (lambda (x)
               #-(and) (e-is-true (e. deep-frozen-guard |isDeepFrozen| x))
               (e-is-true (e. +the-audit-checker+ |run| +deep-frozen-stamp+ x))) subs)
    +deep-frozen-stamp+
    (e-lambda "org.cubik.cle.prim.deepFrozenIfEveryStubAuditor" () (:|audit/2| (constantly +e-false+)))))

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
            (e. thunk |run|)
            (format stream "done.~%")))
        (:|runAsTurn| (thunk context-thunk)
          "Call the given thunk. If it throws, the exception is logged for debugging (unsealed), and a broken reference (sealed) is returned. If it ejects, no special handling is performed.
    
    If a log message is produced, context-thunk is run to produce a string describing the origin of the failure."
          (handler-case
            (e. thunk |run|)
            (error (condition)
              (e. |trace| |run|
                (e-lambda nil ()
                  (:|__printOn| (tw)
                    (e-coercef tw +the-text-writer-guard+)
                    (e. tw |print| "caught problem in ")
                    (e. tw |quote| (e. context-thunk |run|))
                    (e. tw |print| ": " (e-print condition)))))
              (make-unconnected-ref (transform-condition-for-e-catch condition)))))))))

;; XXX threading: if tracers have state they need to become nonglobal
(defglobal +trace+ (make-tracer :label "misc"))
(defglobal +sys-trace+ (make-tracer :label ""))

; XXX merge these two: make lazy-apply robust in the presence of failure, and make lazy-eval use lazy-apply with a particular thunk

(defun make-lazy-eval-slot (scope source &aux value this)
  (setf this (e-lambda "LazyEvalSlot"
      (:stamped +deep-frozen-stamp+) ; XXX this stamp is only appropriate when the resulting value is also DeepFrozen, so this maker is not safe
    (:|getValue| ()
      (when source
        (let ((source-here source))
          (multiple-value-bind (promise resolver) (make-promise)
            (setf value  promise
                  source nil)
            (handler-case
              (progn
                (setf value (elang:eval-e (e.syntax:parse-to-kernel source-here) (e. scope |withPrefix| (format nil "~A<lazy-eval>$" (e. scope |getFQNPrefix|)))))
                (e. resolver |resolve| value))
              (error (p)
                (e. resolver |smash| p))))))
      value)
    (:|setValue| (new)
      (declare (ignore new))
      (error "not an assignable slot: ~A" (e-quote this)))
    (:|isFinal| () elib:+e-true+))))

(defun make-lazy-apply-slot (maker &aux value-box)
  (e-lambda "lazyApplySlot"
      (:stamped +deep-frozen-stamp+)
    (:|getValue| ()
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

(defun make-safe-extern-loader ()
  (lazy-value-scope ("__cle_safe_extern" "")

    ("org.erights.e.elib.base.makeSourceSpan" e.elib:+the-make-source-span+)

    ("org.apache.oro.text.regex.Perl5Compiler" e.extern:+rx-perl5-compiler+)
    ("org.apache.oro.text.regex.Perl5Matcher"  e.extern:+rx-perl5-matcher+)
    
    ("org.cubik.cle.parser.makeLALR1Parser"
     (e. (e-import "org.cubik.cle.parser.makeLALR1ParserAuthor") 
         ;; XXX smaller authority: actually just wants access to cl-yacc
         |run| +lisp+))
    
    ("org.cubik.cle.prim.parser"               e.syntax:+prim-parser+)
    ("org.cubik.cle.prim.ePrinter"             e.syntax:+e-printer+)
    ("org.cubik.cle.prim.makeFirstCharSplitter" +make-first-char-splitter+)
    ("org.cubik.cle.prim.makePathLoader"       +the-make-path-loader+)
    ("org.cubik.cle.prim.simplifyFQName" 
      (e-lambda "org.cubik.cle.prim.simplifyFQName" 
          (:stamped +deep-frozen-stamp+)
        (:|run| (x) (elib:simplify-fq-name (elib:e-coerce x 'string))))) ; XXX replace this with wrap-function
    ("org.cubik.cle.io.makeSocket"
      (symbol-value (system-symbol "+THE-MAKE-SOCKET+" :e.sockets :e-on-cl.sockets)))
    ))

(defun f+ (f1 f2)
  (lambda (&rest args)
    (multiple-value-call f1 (apply f2 args))))

; XXX simplify the amount of wrapping this requires / make those of these primitives which are safe (all of them?) importable
(defglobal +e-ref-kit-slot+ (make-lazy-apply-slot (lambda ()
  (e. (e. +builtin-emaker-loader+ |fetch|
        "org.erights.e.elib.ref.RefAuthor" 
        (e-lambda "org.erights.e.elib.prim.RefAuthorNotFoundThunk" () (:|run| () (error "RefAuthor missing")))) 
      |run|
      (wrap-function (f+ #'vector #'make-promise)
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function #'ref-state
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function (f+ #'as-e-boolean #'ref-is-resolved)
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function #'make-unconnected-ref
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function #'ref-opt-problem
                     :stamps (list +deep-frozen-stamp+))
      'elib:broken
      'elib:near
      'elib:eventual
      +deep-frozen-stamp+))))

(defun default-safe-scope-roots ()
  (let* ((bare-emaker-loader
           (e. +the-make-path-loader+ |run| "__importUncachedEmaker" 
             (map 'vector 
                  #'make-emaker-loader-from-desc
                  *emaker-search-list*)))
         (emaker-importer
           (let ((deep-frozen-cache (make-hash-table :test #'equal)))
             (e-lambda "org.cubik.cle.internal.emakerImporter"
                 (:stamped +deep-frozen-stamp+)
               (:|fetch| (fqn absent-thunk)
                 (e-coercef fqn 'string)
                 (multiple-value-bind (cache-value cache-present) (gethash fqn deep-frozen-cache)
                   (if cache-present
                     cache-value
                     (let ((result (e. bare-emaker-loader |fetch| fqn absent-thunk)))
                       (when (e-is-true (e. (e. (vat-safe-scope *vat*) |get| "DeepFrozen") |isDeepFrozen| result))
                         (setf (gethash fqn deep-frozen-cache) result))
                       result))))))))
    (make-scope "__defaultSafeScopeRoots.thisShouldNotBeVisible$"
      `(("&import__uriGetter"  ,(make-lazy-apply-slot (lambda ()
          ; wrapper to provide stamped DeepFrozenness since we can't currently do it 'properly' without dependency cycles
          (let ((real-loader 
                  (e. +the-make-path-loader+ |run| "import" (vector 
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
 
(defun make-safe-scope (&optional (fqn-prefix "__safe$") (roots (default-safe-scope-roots))
    &aux (&<import> (e. roots |getSlot| "import__uriGetter")))
  (with-result-promise (safe-scope-vow)
    (labels ((typical-lazy (source)
               (make-lazy-eval-slot safe-scope-vow source))
             (lazy-import (fqn)
               (make-lazy-apply-slot (lambda () (e. (e. &<import> |getValue|) |get| fqn)))))
      (make-scope fqn-prefix
      
        `(; --- self-referential / root ---
          ("safeScope"  ,safe-scope-vow)
          ("&import__uriGetter"  ,&<import>)

          ; --- primitive: values not available as literals ---
          ; XXX true can be defined as (0 =~ _), and false as (!true) or (0 =~ []). Do we want to do so?
          ("null"       ,nil)
          ("false"      ,elib:+e-false+)
          ("true"       ,elib:+e-true+)
          ("NaN"        ,elib:|NaN|)
          ("Infinity"   ,elib:|Infinity|)
          
          ; --- primitive: guards ---
          ("any"        ,(make-instance 'cl-type-guard :type-specifier 't))
          ("void"       ,elib:+the-void-guard+)
      
          ; --- primitive: flow control ---
          ("&E"         ,(lazy-import "org.cubik.cle.prim.E")) ; XXX should be interp.E
          ("&throw"     ,(lazy-import "org.cubik.cle.prim.throw")) ; XXX should be elang.interp.throw
          ("&__loop"    ,(lazy-import "org.cubik.cle.prim.loop")) ; XXX should be elang.interp.loop
          
          ; --- primitive: tracing ---
          ("trace"      ,+trace+)
          ("traceln"    ,+trace+)

          ; --- nonprimitive flow control ---
          ("&require"   ,(lazy-import "org.erights.e.elang.interp.require"))
          
          ; --- data constructors ---
          ("__makeFinalSlot"     ,elib:+the-make-simple-slot+)
          ("__makeVarSlot"       ,elib:+the-make-var-slot+)
          ("__makeGuardedSlot"   ,elib:+the-make-guarded-slot+)
          ("__makeInt"           ,elib:+the-make-int+)
          ("__makeList"          ,elib:+the-make-list+)
          ("__makeMap"           ,elib:+the-make-const-map+)
          ("__makeTwine"         ,elib:+the-make-twine+)
          ("&__makeOrderedSpace" ,(lazy-import "org.erights.e.elang.coord.OrderedSpaceMaker"))
          ("&term__quasiParser"  ,(typical-lazy "<import:org.quasiliteral.quasiterm.makeQBuilder>.getTerm__quasiParser()"))
      
          ; --- data guards: atomic ---
          ; XXX should boolean be an ordered space?
          ("boolean"    ,(make-instance 'cl-type-guard :type-specifier 'e-boolean))
          ("&int"       ,(typical-lazy "__makeOrderedSpace(<import:org.cubik.cle.prim.int>, \"int\")"))
          ("&float64"   ,(typical-lazy "__makeOrderedSpace(<import:org.cubik.cle.prim.float64>, \"float64\")"))
          ("&char"      ,(typical-lazy "__makeOrderedSpace(<import:org.cubik.cle.prim.char>, \"char\")"))    
          ("String"     ,(make-instance 'cl-type-guard :type-specifier 'string))
          ("Twine"      ,(make-instance 'cl-type-guard :type-specifier 'elib:twine))
          ("TextWriter" ,elib:+the-text-writer-guard+)
          
          ; --- data guards: nonatomic, nonprimitive ---
          ("&all"         ,(lazy-import "org.erights.e.elib.slot.makeIntersectionGuard"))
          ("&Not"         ,(lazy-import "org.erights.e.elib.slot.makeNegatedGuard"))
          ("&List"        ,(lazy-import "org.erights.e.elib.slot.List"))
          ("&Map"         ,(lazy-import "org.erights.e.elib.slot.Map"))
          ("&Set"         ,(typical-lazy "<import:org.erights.e.elib.tables.ConstSet>.asType()"))
          ("&Tuple"       ,(lazy-import "org.erights.e.elib.slot.Tuple"))
          ("&__Portrayal" ,(typical-lazy "Tuple[any, String, List[any]]"))
  
          ; --- protocol/guard constructors ---
          ("&__makeProtocolDesc" ,(lazy-import "org.erights.e.elang.interp.makeProtocolDesc"))
          ("&__makeMessageDesc"  ,(lazy-import "org.erights.e.elib.base.makeMessageDesc"))
          ("&__makeParamDesc"    ,(lazy-import "org.erights.e.elib.base.makeParamDesc"))
          
          ; --- utility: guard meta ---
          ("&__makeGuard" ,(typical-lazy "def stubMakeGuard(_) :any { return def stubBaseGuard {} }"))
          ; The ValueGuard and Guard guards do not currently reject anything, but this may change (e.g. DeepFrozen)
          ("&ValueGuard"  ,(lazy-import "org.erights.e.elib.slot.type.ValueGuard"))
          ("&Guard"       ,(lazy-import "org.erights.e.elib.slot.type.Guard"))
      
          ; --- primitive: reference operations ---        
          ("__auditedBy" ,+the-audit-checker+)
          ("&Ref"        ,(lazy-import "org.cubik.cle.prim.Ref")) ; XXX should be elib.ref.Ref
          ("&__equalizer",(lazy-import "org.cubik.cle.prim.equalizer")) ; XXX should be elib.tables.equalizer
          ("&DeepFrozen" ,(lazy-import "org.cubik.cle.prim.DeepFrozen")) ; XXX fqn
          
          ; --- utility: guards ---
          ("&nullOk"    ,(lazy-import "org.erights.e.elib.slot.nullOk"))
          ("&notNull"   ,(lazy-import "org.erights.e.elang.interp.notNull"))
          
          ; --- utility: reference conditions ---
          ("&PassByCopy" ,(lazy-import "org.erights.e.elib.serial.PassByCopy"))
          ("&DeepPassByCopy" 
                         ,(lazy-import "org.erights.e.elib.serial.DeepPassByCopy"))
          ("&Data" 
                         ,(lazy-import "org.erights.e.elib.serial.DeepPassByCopy"))
          ("&near"       ,(lazy-import "org.erights.e.elib.slot.near"))
          ("&vow"        ,(lazy-import "org.erights.e.elang.interp.vow"))
          ("&rcvr"       ,(lazy-import "org.erights.e.elang.interp.rcvr"))
          
          ; --- E-syntax ---
          ("&e__quasiParser" ,(lazy-import "org.erights.e.elang.syntax.makeEParser"))
          ("&epatt__quasiParser" ,(lazy-import "org.erights.e.elang.syntax.epatt__quasiParser"))
          
          ; --- utility: data ---
          ("&simple__quasiParser" ,(lazy-import "org.quasiliteral.text.simple__quasiParser"))
          ("&rx__quasiParser" ,(lazy-import "org.erights.e.elang.interp.PerlMatchMakerMaker"))
              
          ; --- utility: alias loaders ---
          ("&elib__uriGetter"   ,(lazy-import "org.erights.e.elib.*"))
          ("&elang__uriGetter"  ,(lazy-import "org.erights.e.elang.*"))
          ("&type__uriGetter"   ,(lazy-import "org.erights.e.elang.interp.typeLoader"))
          
          ; --- utility: EIO ---
          ("&EIO"               ,(lazy-import "org.erights.e.elib.eio.EIO"))

          ; --- utility: used by expansions ---
          ;; XXX these FQNs and scope nouns should be discussed
          ("&__bind"         ,(lazy-import "org.erights.e.elang.interp.makeViaBinder"))
          ("&__mapEmpty"     ,(lazy-import "org.erights.e.elang.interp.viaEmptyMap"))
          ("&__mapExtract"   ,(lazy-import "org.erights.e.elang.interp.makeViaExtractor"))
          ("&__quasiMatcher" ,(lazy-import "org.erights.e.elang.interp.makeViaQuasi"))
          ("&__matchSame"    ,(lazy-import "org.erights.e.elang.interp.makeViaSame"))
          ("&__suchThat"    ,(lazy-import "org.erights.e.elang.interp.suchThat"))
          ("&__comparer"     ,(lazy-import "org.erights.e.elang.interp.comparer"))
          ("&__makeVerbFacet",(lazy-import "org.erights.e.elang.interp.__makeVerbFacet"))
          ("&__booleanFlow"  ,(lazy-import "org.erights.e.elang.interp.booleanFlow"))
          ("__validateFor"      ,(e-lambda "$__validateFor"
              (:stamped +deep-frozen-stamp+)
            (:|run| (flag)
              (unless (e-is-true flag)
                (error "For-loop body isn't valid after for-loop exits.")))))

          ; --- utility: miscellaneous ---
          ("&__identityFunc"    ,(typical-lazy "def identityFunc(x) :any { return x }"))
          ("&opaque__uriGetter" ,(lazy-import "org.erights.e.elib.serial.opaque__uriGetter"))
          
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
                ; NOTE: these stamps are process-wide, but their effect is local unless amplified with some authority allowing sharing objects between vats (e.g. a hypothetical thread-safe stamp)
                ; XXX makeProxyResolver will eventually convey GC-notification authority and so is arguably not a *local* privilege.
                `(("DeepFrozenStamp"   ,elib:+deep-frozen-stamp+)
                  ("SelflessStamp"     ,elib:+selfless-stamp+)
                  ("makeProxyResolver" ,elib:+the-make-proxy-resolver+)))
            |or| (vat-safe-scope *vat*))))
    (e. (e-import "org.cubik.cle.makeIOScope")
        |run| 
        "__privileged$"
        vat-priv-scope
        (make-scope "__ioPowers$"
          `(("timer"           ,e.extern:+the-timer+)
            ("file__uriGetter" ,(e.extern:make-file-getter '#()))
            ("gc"              ,e.extern:+gc+)
            ("makeWeakRef"     ,+the-make-weak-ref+)
            ("unsafeNearSpawn"      ,e.extern:+spawn+)
            ("&stdin"     ,(make-lazy-apply-slot (lambda ()
                             (warn "making stdin")
                             (e. (e. (e-import "org.cubik.cle.charsets") |get| e.extern:+standard-external-format-common-name+) |decode| (e. (e. (e-import "org.cubik.cle.io.makeFDInStreamAuthor")
                                 |run|
                                 +lisp+) |run| (e-lambda "stdin" ()) (funcall (system-symbol "STREAM-TO-FD-REF" :e.sockets :e-on-cl.sockets) *standard-input*) 4096) (e. #() |asMap|)))))
            ("stdout"     ,(make-text-writer-to-cl-stream
                            out-cl-stream
                            :autoflush t
                            :should-close-underlying nil))
            ("stderr"     ,(make-text-writer-to-cl-stream
                            error-cl-stream
                            :autoflush t
                            :should-close-underlying nil))
            ("lisp"       ,+lisp+)
            ("props"      ,+eprops+)
            ,@(when interp-supplied
              `(("interp" ,interp)))
            ("&IP"        ,(make-lazy-apply-slot (lambda ()
                             (e. (e-import "org.cubik.cle.IPAuthor")
                                 |run|
                                 +lisp+))))
            ("&getSocketPeerRef"        
             ,(make-lazy-apply-slot (lambda () (symbol-value (system-symbol "+THE-GET-SOCKET-PEER-REF+" :e.sockets :e-on-cl.sockets)))))
            ("&getSocketLocalRef"
             ,(make-lazy-apply-slot (lambda () (symbol-value (system-symbol "+THE-GET-SOCKET-LOCAL-REF+" :e.sockets :e-on-cl.sockets)))))
            ("&makePipe"        
             ,(make-lazy-apply-slot (lambda () (symbol-value (system-symbol "+THE-MAKE-PIPE+" :e.sockets :e-on-cl.sockets)))))
            #||#)))))

;;; --- end ---

#+sbcl (sb-ext:lock-package #.*package*)
