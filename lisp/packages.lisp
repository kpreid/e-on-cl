; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :cl-user)

(cl:defpackage :e.util
  (:nicknames :e-util) ; XXX remove use of this nickname
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
  `(declaim (special ,@names)))

(e.util:defglobals
  e.util:+the-asdf-system+)

(cl:defpackage :e.elib
  (:nicknames :elib)
  (:use :cl :e.util :trivial-garbage :bordeaux-threads)
  
  #+(or sbcl cmu)
  (:import-from #+sbcl sb-sys
                #+cmu sys
     :serve-event
     :add-fd-handler
     :remove-fd-handler)
  
  (:export 
    :*compatible-catch-leakage*
    :*break-on-ejections*
    
    :unexternalizable-optimization-p
    :with-unexternalizable-optimization
    
    :get-fine-universal-time
    
    :vicious-cycle-error
    :synchronous-call-error
    :no-such-method-error :no-such-method
  
    :+the-e+
    
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
    :e-var-slot
    :e-guarded-slot
    :native-e-slot
    :+the-make-simple-slot+
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
    
    :+the-void-guard+
    :+the-any-guard+
    :+the-text-writer-guard+
    :+the-exception-guard+
    
    :approvedp
    :+the-audit-checker+
    :audited-by-magic-verb
    :+deep-frozen-stamp+
    :+selfless-stamp+
    
    :+thread-sharable-stamp+
    :+pass-by-construction+
    :+standard-graph-exit+
    :+standard-graph-exit-stamp+
    
    :+e-false+ :+e-true+ :e-is-true :as-e-boolean :e-boolean
    
    :|NaN| :|Infinity| :float64 :float32 :e-list
    
    :+the-thrower+
    :ejector
    :ejector-prethrow
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
    :+the-map-guard+
    :+the-any-map-guard+
    :+the-make-list+
    :+the-make-int+
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
  elib:+the-make-simple-slot+
  elib:+the-make-var-slot+
  elib:+the-make-guarded-slot+

  elib:+the-make-coercion-failure+

  elib:+the-any-map-guard+
  elib:+the-make-const-map+
  elib:+the-make-twine+
  elib:+the-text-writer-guard+

  elib:+the-make-type-desc+
  elib:+the-make-param-desc+
  elib:+the-make-message-desc+

  #-sbcl elib:+e-false+
  #-sbcl elib:+e-true+
  elib:+the-exception-guard+
  elib:+the-map-guard+
  
  elib:+the-thrower+
  elib:+the-make-coercion-failure+
  elib:+the-make-string-error+

  elib:+the-make-flex-map+
  elib:+the-make-text-writer+
  elib:+the-make-list+
  elib:+the-make-int+
  elib:+the-make-priority-queue+

  elib:+the-e+

  elib:+the-audit-checker+
  elib:+deep-frozen-stamp+
  
  elib:+thread-sharable-stamp+
  elib:+pass-by-construction+
  elib:+standard-graph-exit+
  elib:+standard-graph-exit-stamp+

  elib:+the-make-source-span+
  elib:+the-make-twine+

  elib:+the-make-weak-ref+
  elib:+the-make-traversal-key+
  elib:+the-make-vat+
  elib:+the-make-proxy+)

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
  (:use :cl :e.util :elib :net.hexapodia.hashtables)
  (:documentation "Collection implementations.")
  (:export
    :~span
    :const-map ; type
    :+the-make-array+))

(e.util:defglobals
  e.elib.tables:+the-make-array+)

(cl:defpackage :e.knot
  (:use :cl :e.util :elib)
  (:export
    :scope
    :make-scope
    :require-node-fits-scope
    
    :+the-make-path-loader+
    
    :+shared-safe-loader+
    :+shared-safe-scope+
    :+sharable-importer+
    
    :make-safe-scope
    :make-io-scope
    
    :found-e-on-java-home
    :*emaker-search-list*
    
    :+lisp+
    :+trace+
    :+sys-trace+))
  
(e.util:defglobals
  e.knot:+sharable-importer+
  e.knot:+lisp+
  e.knot:+trace+
  e.knot:+sys-trace+
  e.knot:+the-make-path-loader+)
  
(cl:defvar e.knot:*emaker-search-list*)

(cl:defpackage :e.grammar
  (:documentation "Node types from the Antlr grammar.")
  (:use))

(cl:defpackage :e.kernel
  ;; XXX plan-of-the-moment is to deprecate the names other than e.kernel
  (:nicknames :e.node :e.elang.vm-node)
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
    :|SlotExpr|
    :|SlotPattern|
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
  (:nicknames :elang)
  (:use :cl :e.util :elib :e.elang.vm-node :e.elang.node-impl :bordeaux-threads)
  (:export
    :eval-e
    :get-translation
    :require-kernel-e
    
    :e-macroexpand-1
    :e-macroexpand
    :e-macroexpand-all
    :kernelize

    :+the-make-static-scope+
    
    :node-elements
    :node-visitor-arguments
    :node-arity-error
    :opt-guard-expr-to-safe-opt-guard
    :pattern-opt-noun
    :pattern-to-param-desc
    
    :slot-symbol-var-name ;; XXX stale
    ))


(e.util:defglobals
  elang:+the-make-static-scope+)

(declaim (ftype function
  e.elang:eval-e
  e.elang:get-translation))

(cl:defpackage :e.elang.syntax
  (:nicknames :e.syntax)
  (:use :cl :e.util :e.elib :e.elang :e.elang.vm-node
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
  e.elang.syntax:+prim-parser+
  e.elang.syntax:+e-printer+)

(declaim (ftype function 
  e.elang.syntax:e-source-to-tree
  e.elang.syntax:load-parse-cache
  e.elang.syntax:save-parse-cache
  e.elang.syntax:load-parse-cache-file
  e.elang.syntax:save-parse-cache-file))


(cl:defpackage :e.elang.compiler
  (:nicknames :e.compiler)
  (:use :cl :e.util :e.elib :e.elang :e.elang.vm-node)
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
    
    :direct-def-binding
    :direct-var-binding
    :value-binding
    :binding-for-slot
    
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
  e.elang.compiler::+e-audition-guard+
  e.elang.compiler:+the-evaluator+)

(cl:defpackage :e.elang.compiler.seq
  (:nicknames :e.compiler.seq)
  (:use :cl :e.util :e.elib :e.elang :e.elang.vm-node :e.elang.compiler)
  (:export
    :sequence-e-to-cl))

(declaim (ftype function
  e.elang.compiler.seq:sequence-e-to-cl))

(cl:defpackage :e.extern
  (:use :common-lisp :e.util :elib :elang)
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
  (:nicknames :e.stream)
  (:use :cl :e.util :elib 
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

