; Copyright 2005-2009 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :cl-user)

(cl:defpackage :e.util
  (:use :cl :bordeaux-threads)
  
  #.(cl:let ((cl:package (cl:some #'cl:find-package '( 
               #+sbcl :sb-mop
               #+openmcl :openmcl-mop
               #+ccl :ccl
               :mop #| allegro, cmucl, abcl |#
               :clos #| allegro, clisp, lispworks |#))))
    (cl:when cl:package
      `(:import-from ,(cl:package-name cl:package)
         :class-precedence-list
         :class-finalized-p
         :finalize-inheritance)))
  
  (:export
    :defglobals
    :defglobal
    :defconstantonce
  
    :aan
    :named-lambda
    :map-from-hash
    :system-symbol

    :native-pathname
    :native-namestring
    
    :run-program
    :external-process-input-stream
    :external-process-output-stream
    :external-process-error-stream
    :external-process-status
    :external-process-exit-code
    
    :mangle-verb
    :unmangle-verb
    :mverb-verb=
    
    :without-suffix
    :without-prefix
    :popping-equal-case
    :nreverse-here
    :convention-capitalize
    :convention-uncapitalize
    :guess-lowercase-string
    
    :queue
    :priority-queue
    :enqueue
    :dequeue
    :dequeue-blocking
    :queue-null
    :priority-queue-peek
    :priority-queue-snapshot
    :priority-queue-pop
    :priority-queue-put
    :priority-queue-length
    
    :class-precedence-list
    :class-finalized-p
    :finalize-inheritance
    :with-appropriate-floating-point-rules
    
    :function-lambda-list :<unknown-lambda-list>
    
    :lambda-list-arguments-range
    :function-arguments-range
    :function-responds-to
    
    :handler-case-with-backtrace
    :backtrace-value
    
    :+the-asdf-system+))

;;; 'early util' interlude
(defmacro e.util:defglobals (&rest names)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for name in names collect
         `(unless (constantp ',name)
            (proclaim '(special ,name))))))

(e.util:defglobals
  e.util:+the-asdf-system+)

(cl:defpackage :e.elib
  (:use :cl :e.util :trivial-garbage :bordeaux-threads)
  
  #+(or sbcl cmu)
  (:import-from #+sbcl sb-sys
                #+cmu sys
     :serve-event
     :add-fd-handler
     :remove-fd-handler)
  
  (:intern
    :selflessp)
  
  (:export 
    :*compatible-catch-leakage*
    :*break-on-ejections*
    :*causality-output*
    
    :unexternalizable-optimization-p
    :with-unexternalizable-optimization
    
    :get-fine-universal-time
    
    :vicious-cycle-error
    :synchronous-call-error
    :no-such-method-error :no-such-method
  
    :runner *runner*
    :make-runner-for-this-thread
    :runner-loop :top-loop
    :enqueue-turn
    :enqueue-timed
    :vr-add-io-handler
    :vr-remove-io-handler
    
    :label
    :vat :*vat*
    :with-turn
    :establish-vat
    :vat-safe-scope
    :vat-privileged-scope
    :vat-checking
    
    :sugar-cache-get 
    :sugar-cache-call
  
    :e-call :e-call-dispatch :e.
    :e-send :e-send-dispatch :e<-
    :e-send-only-dispatch
    :e-call-match
    
    :near :eventual :broken
    
    :ref-state
    :ref-is-resolved
    :ref-shorten
    :ref-opt-problem
    :ref-opt-sealed-dispatch
    :make-promise
    :make-unconnected-ref
    :with-result-promise
    
    :def-vtable
    :e-lambda :efun :e-lambda-type-desc
    :defobject
    :miranda
    :message-pairs-to-map-including-miranda-messages
    :or-miranda-message-descs
    :static-maker
    
    :e-simple-slot
    :coerced-slot
    :e-var-slot
    :e-guarded-slot
    :native-e-slot
    :+the-make-simple-slot+
    :+the-make-coerced-slot+
    :+the-make-var-slot+
    :+the-make-guarded-slot+
    
    :cl-type-fq-name
    :cl-type-simple-expr
    :cl-type-parameters
    :def-fqn
    
    :e-coerce
    :e-coercef
    :e-coerce-native
    
    :observable-type-of 
    :def-class-opaque
    :cl-type-guard
    :make-e-type-error
    :+the-make-coercion-failure+
    :type-specifier-to-guard
    :guard-to-type-specifier
    :standard-coerce
    
    :lazy-ref 
    :evaluate-lazy-ref
    :thunk-lazy-ref
    :with-node :with-node-base
    
    :+the-void-guard+
    :+the-any-guard+
    :+the-text-writer-guard+
    :+the-exception-guard+
    :same-guard :+make-same-guard+
    
    :approvedp
    :+the-audit-checker+
    :audited-by-magic-verb
    :+deep-frozen-stamp+
    :+selfless+
    :+transparent-guard+
    :+transparent-stamp+
    :+semitransparent-stamp+
    :+semitransparent-result-box-brand+
    :semitransparent-result-box-contents
    :semitransparent-result-box
    
    :+thread-sharable-stamp+
    :+pass-by-construction+
    :+standard-graph-exit+
    :+standard-graph-exit-stamp+
    
    :+e-false+ :+e-true+ :e-is-true :as-e-boolean :e-boolean
    
    :|NaN| :|Infinity| :float64 :float32 :e-list
    
    :+the-thrower+
    :ejector
    :ejector-extent-error
    :ejector-prethrow :%ejector-throw
    :eject-or-ethrow
    :e-catchable-condition
    :transform-condition-for-e-catch
    :e-problem-to-condition
    :e-problem-unseal
    
    :+the-make-coercion-failure+
    :+the-make-string-error+
    
    :make-traversal-key
    :+the-make-traversal-key+
    :+the-make-flex-map+
    :+the-make-const-map+
    :+empty-const-map+
    :+the-map-guard+
    :+the-any-map-guard+
    :+the-make-list+
    :+the-make-int+
    :+the-make-float64+
    :+the-make-priority-queue+    
    
    :e-quote
    :e-print
    :with-text-writer-to-string
    :+the-make-text-writer+
    :text-writer
    :make-text-writer-to-cl-stream
    :unguarded-text-writer-error
    
    :type-desc :message-desc :param-desc
    :+the-make-type-desc+
    :+the-make-message-desc+
    :+the-make-param-desc+
    :doc-comment
    
    :simplify-fq-name
    :join-fq-name
    :nest-fq-name :environment-fqn-prefix
    
    :make-equalizer
    :settledp
    :samep
    :same-yet-p
    :transparent-selfless-p
    :same-hash
    :traversal-key
    :def-atomic-sameness
    :insufficiently-settled-error
    :selfish-hash-magic-verb
    :make-hash-code
    
    :source-span :+the-make-source-span+
    :twine :+the-make-twine+
    
    :+the-make-weak-ref+
    :+the-make-vat+
    :+the-make-proxy+
    
    :e-slot-value :place-slot
    :def-shorten-methods
    :escape :escape-bind
    :ejerror
    :with-vat :call-when-resolved :when-resolved
    :mapping-bind

    :e-import
    :import-uncall

    :eelt :efuncall))

(e.util:defglobals
  e.elib:+the-make-simple-slot+
  e.elib:+the-make-var-slot+
  e.elib:+the-make-guarded-slot+

  e.elib:+the-make-coercion-failure+

  e.elib:+the-any-map-guard+
  e.elib:+the-make-const-map+
  e.elib:+the-make-twine+
  e.elib:+the-text-writer-guard+

  e.elib:+the-make-type-desc+
  e.elib:+the-make-param-desc+
  e.elib:+the-make-message-desc+

  #-sbcl e.elib:+e-false+
  #-sbcl e.elib:+e-true+
  e.elib:+the-exception-guard+
  e.elib:+the-map-guard+
  
  e.elib:+the-thrower+
  e.elib:+the-make-coercion-failure+
  e.elib:+the-make-string-error+

  e.elib:+the-make-flex-map+
  e.elib:+the-make-text-writer+
  e.elib:+the-make-list+
  e.elib:+the-make-int+
  e.elib:+the-make-float64+
  e.elib:+the-make-priority-queue+

  e.elib:+the-audit-checker+
  e.elib:+deep-frozen-stamp+
  
  e.elib:+thread-sharable-stamp+
  e.elib:+pass-by-construction+
  e.elib:+standard-graph-exit+
  e.elib:+standard-graph-exit-stamp+

  e.elib:+the-make-source-span+
  e.elib:+the-make-twine+

  e.elib:+the-make-weak-ref+
  e.elib:+the-make-traversal-key+
  e.elib:+the-make-vat+
  e.elib:+the-make-proxy+)

(declaim (ftype function
  e.elib:ejector
  e.elib:simplify-fq-name
  e.elib:join-fq-name
  e.elib:make-equalizer
  e.elib:cl-type-simple-expr
  e.elib:same-hash
  e.elib:make-text-writer-to-cl-stream
  e.elib:e-import))

(cl:defpackage :e.elib.tables
  (:nicknames :e.tables)
  (:use :cl :e.util :e.elib :net.hexapodia.hashtables)
  (:documentation "Collection implementations.")
  (:export
    :~span
    :const-map ; type
    :+the-make-array+
    :vector-from-iteratable))

(e.util:defglobals
  e.elib.tables:+the-make-array+)

(cl:defpackage :e.knot
  (:use :cl :e.util :e.elib)
  (:export
    :scope
    :make-scope
    :+the-make-scope+
    :require-node-fits-scope
    
    :+the-make-path-loader+
    
    :+shared-safe-loader+
    :+shared-safe-scope+
    :+sharable-importer+
    
    :make-safe-scope
    :make-io-scope
    
    :found-e-on-java-home
    :*emaker-search-list*
    
    :+the-e+
    :+lisp+
    :+trace+
    :+sys-trace+))
  
(e.util:defglobals
  e.knot:+sharable-importer+
  e.knot:+the-e+
  e.knot:+lisp+
  e.knot:+trace+
  e.knot:+sys-trace+
  e.knot:+the-make-path-loader+)
  
(cl:defvar e.knot:*emaker-search-list*)

(cl:defpackage :e.grammar
  (:documentation "Node types from the Antlr grammar.")
  (:use))

(cl:defpackage :e.kernel
  (:documentation "All symbols in this package are the names of subclasses of ENode, eccept for null, error and .tuple..")
  ; XXX arrange so that null, error and .tuple. are not produced by parseEToSExpression?
  (:use)
  (:import-from :cl
    :error)
  (:export
    :|null|

    :|.tuple.|
    :|.AtHole.|
    :|.DollarHole.|
    
    :|AssignExpr|
    :|Auditors|
    :|BindingExpr|
    :|BindingPattern|
    :|CallExpr|
    :|CatchExpr|
    :|DefineExpr|
    :|EExpr|
    :|EMatcher|
    :|EMethod|
    :|EMethodoid|
    :|ENode|
    :|EScript|
    :|EScriptoid|
    :|EscapeExpr|
    :|FinalPattern|
    :|FinallyExpr|
    :|HideExpr|
    :|IfExpr|
    :|IgnorePattern|
    :|ListPattern|
    :|LiteralExpr|
    :|MetaContextExpr|
    :|MetaStateExpr|
    :|NounExpr|
    :|NounPattern|
    :|ObjectExpr|
    :|Pattern|
    :|QuasiLiteralExpr|
    :|QuasiLiteralNode|
    :|QuasiLiteralPatt|
    :|QuasiNode|
    :|QuasiPatternExpr|
    :|QuasiPatternNode|
    :|QuasiPatternPatt|
    :|SeqExpr|
    :|VarPattern|
    :|ViaPattern|)
  #+sbcl (:lock t))

;; XXX for now, e.nonkernel.* packages are defined in nonkernel.lisp

(cl:defpackage :e.elang.node-impl
  (:use)
  (:export
    :reject-definition-usage
    :define-node-class
    :def-scope-rule))

(cl:defpackage :e.elang
  (:use :cl :e.util :e.elib :e.kernel :e.elang.node-impl :bordeaux-threads)
  (:export
    :eval-e
    :get-translation
    :require-kernel-e
    
    :e-macroexpand-1
    :e-macroexpand
    :e-macroexpand-all
    :kernelize

    :+the-make-static-scope+
    
    :mn :mnp :node-quote
    
    :node-elements
    :node-visitor-arguments
    :node-arity-error
    :opt-guard-expr-to-safe-opt-guard
    :pattern-opt-noun
    :pattern-to-param-desc
    
    :unpack-auditors
    ))


(e.util:defglobals
  e.elang:+the-make-static-scope+)

(declaim (ftype function
  e.elang:eval-e
  e.elang:get-translation))

(cl:defpackage :e.syntax
  (:use :cl :e.util :e.elib :e.elang :e.kernel
        #+abcl :java ; local parser (no cache)
        #-abcl :net.hexapodia.hashtables ; for parse cache
        :bordeaux-threads)
  (:export
    +e-printer+
    
    :e-source-to-tree
    :parse-to-kernel
    
    :load-parse-cache
    :save-parse-cache
    :load-parse-cache-file
    :save-parse-cache-file
    :with-parse-cache-file
    
    :+prim-parser+
    ))

(when (member :abcl *features*)
  (pushnew 'e.syntax::local-parser *features*))

(e.util:defglobals
  e.syntax:+prim-parser+
  e.syntax:+e-printer+)

(declaim (ftype function 
  e.syntax:e-source-to-tree
  e.syntax:load-parse-cache
  e.syntax:save-parse-cache
  e.syntax:load-parse-cache-file
  e.syntax:save-parse-cache-file))


(cl:defpackage :e.compiler
  (:nicknames :e.compiler)
  (:use :cl :e.util :e.elib :e.elang :e.kernel :net.hexapodia.hashtables)
  (:export
    :scope-layout
    :scope-layout-noun-binding
    :scope-layout-fqn-prefix
    :scope-layout-noun-is-local
    :scope-layout-bindings
    :scope-layout-meta-state-bindings
    :scope-layout-bindings-before
    :scope-layout-bind
    :scope-layout-nest
    :scope-layout-static-context
    
    :binding-get-code
    :binding-get-slot-code
    :binding-set-code
    
    :lexical-slot-binding
    :direct-def-binding
    :direct-var-binding
    :value-binding

    :eject-via-binding-code
    :block-binding
    :+throw-binding+
    
    :binding-for-slot
    :to-compiler-binding
    :binding-reify-code
    
    ;; ejector specifier tags
    :nil
    :ejector
    :eject-function
    
    :eject-code
    :opt-ejector-make-code
    
    :static-context
    
    :object-form
    :updating-fully-qualify-name
    
    :%catch-expr-resignal
    
    :delta-extract-outer-scope
    :outer-scope-to-layout
    
    :+the-evaluator+
    
    :compile-e-to-file
    :load-compiled-e))

(e.util:defglobals
  e.compiler::+e-audition-guard+
  e.compiler:+the-evaluator+)

(cl:defpackage :e.compiler.seq
  (:use :cl :e.util :e.elib :e.elang :e.kernel :e.compiler)
  (:export
    :sequence-e-to-cl))

(declaim (ftype function
  e.compiler.seq:sequence-e-to-cl))

(cl:defpackage :e.extern
  (:use :common-lisp :e.util :e.elib :e.elang)
  (:documentation "Capability-structured IO, and interfaces to libraries.")
  (:export
    :+standard-external-format+
    :+standard-external-format-common-name+
    :+gc+
    :make-file-getter
    :pathname-to-file
    :read-entire-file
    :+the-timer+
    :+rx-perl5-compiler+
    :+rx-perl5-matcher+
    :+spawn+))

(e.util:defglobals
  e.extern:+standard-external-format+
  e.extern:+standard-external-format-common-name+
  e.extern:+gc+
  e.extern:+the-timer+
  e.extern:+rx-perl5-compiler+
  e.extern:+rx-perl5-matcher+)

(declaim (ftype function
  e.extern:make-file-getter
  e.extern:read-entire-file))

(defpackage :e.streams
  (:use :cl :e.util :e.elib
        #+sbcl :sb-bsd-sockets)
  #+openmcl (:import-from :openmcl-socket :socket-error)
  (:documentation "EIO streams, mapping to CL streams, socket interfaces.")
  (:export
    :cl-to-eio-in-stream
    :cl-to-eio-out-stream
    :stream-to-fd-ref
    
    :foo-connect-tcp
    :get-addr-info
    
    :+the-make-socket+
    :+the-get-socket-peer-ref+
    :+the-get-socket-local-ref+
    :+the-make-pipe+))
    
(e.util:defglobals
  e.streams:+the-make-socket+
  e.streams:+the-get-socket-peer-ref+
  e.streams:+the-get-socket-local-ref+
  e.streams:+the-make-pipe+)


(cl:defpackage :e.rune
  (:use :cl :e.util :e.elib :e.knot)
  (:documentation "Bootstrapping: setting up the E runtime environment after clrune starts the Lisp process.")
  (:export
    :rune
    :rune-then-exit
    :in-e-user))

(cl:defpackage :e.elib.vtable-methods
  (:use)
  (:documentation "For profiling purposes, vtable method functions may be interned in this package so they can be operated on later."))

