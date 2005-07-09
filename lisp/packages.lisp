; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :cl-user)

(cl:defpackage :e.util
  (:nicknames :e-util) ; XXX remove use of this nickname
  (:use :cl)
  
  #+(or sbcl ccl lispworks allegro openmcl cmu clisp) 
  (:import-from 
    #+(or allegro clisp lispworks) :clos
    #+cmu :mop
    #+sbcl :sb-mop
    #+openmcl :openmcl-mop
    #+(and (not openmcl) ccl)  :ccl
    ; #+lispworks :hcl ; XXX worked but may be incorrect, so next time we test on lispworks delete this or the above lispworks->:clos case
    :class-precedence-list)
  
  (:export
    :aan
    :named-lambda
    :run-program
    :external-process-input-stream
    :external-process-output-stream
    
    :mangle-verb
    :unmangle-verb
    
    :without-suffix
    :without-prefix
    :popping-equal-case
    
    :serve-event
    :class-precedence-list
    :with-appropriate-floating-point-rules
    
    :function-lambda-list :<unknown-lambda-list>
    
    :lambda-list-arguments-range
    :function-arguments-range
    :function-responds-to
    
    :backtrace-value))

(cl:defpackage :e.elib
  (:nicknames :elib)
  (:use :cl :e.util)
  (:export 
    :*java-e-compatible* ; XXX deprecated, to be removed and/or split into variables like the following one
    :*compatible-catch-leakage*
    :*break-on-ejections*
    
    :get-fine-universal-time
    
    :vicious-cycle-error
    :synchronous-call-error
  
    :+the-e+
    
    :vat
    :*vat*
    :vat-loop
    :with-turn
    :establish-vat
    :run-vats  ; deprecated
    :vat-safe-scope
    :vat-privileged-scope
    :vat-checking
    :vat-enqueue-turn
    :vat-enqueue-timed
  
    :e-call :e-call-dispatch :e.
    :e-send :e-send-dispatch :e<-
    :e-call-match
    
    :near :eventual :broken
    
    :ref-state
    :ref-is-resolved
    :ref-shorten
    :ref-opt-problem
    :make-promise
    :make-unconnected-ref
    :with-result-promise
    
    :def-vtable
    :e-lambda :efun
    :miranda
    :message-pairs-to-map-including-miranda-messages
    :or-miranda-message-descs
    :static-maker
    
    :e-simple-slot
    :e-var-slot
    :e-guarded-slot
    :native-e-slot
    :+the-unset-slot+
    :+the-make-simple-slot+
    :place-slot
    
    :cl-type-fq-name
    :cl-type-fq-expr :cl-type-simple-expr
    :cl-type-parameters
    :def-fqn
    
    :e-coerce
    :e-coercef
    :e-coerce-native
    
    :observable-type-of 
    :def-class-opaque
    :cl-type-guard
    :+the-void-guard+
    :+the-any-guard+
    :+the-text-writer-guard+
    :+the-exception-guard+
    :+the-flex-list-guard+
    
    :+the-audit-checker+
    :audited-by-magic-verb
    :+deep-frozen-stamp+
    :+selfless-stamp+
   
    :+e-false+ :+e-true+ :e-is-true :as-e-boolean :e-boolean
    
    :|NaN| :|Infinity| :float64 :float32
    
    :ejector
    :ejector-prethrow
    :eject-or-ethrow
    :e-catchable-condition
    :transform-condition-for-e-catch
    :e-problem-to-condition
    :e-problem-unseal
    
    :make-traversal-key
    :+the-make-traversal-key+
    :+the-make-flex-map+
    :+the-make-const-map+
    :+the-map-guard+
    :+the-any-map-guard+
    :+the-make-list+
    :+the-make-int+
    :+the-make-sorted-queue+    
    
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
    
    :make-equalizer
    :eeq-is-settled
    :eeq-is-same-ever
    :eeq-is-same-yet
    :eeq-same-yet-hash
    :traversal-key
    :eeq-hash-dispatch
    :eeq-same-dispatch
    :eeq-is-transparent-selfless
    :def-atomic-sameness
    
    :twine
    :+the-make-twine+
    
    :+the-make-weak-ref+
    
    :+the-make-proxy-resolver+
    
    :e-slot-value :place-slot
    :def-shorten-methods
    :with-vat :when-resolved))

(defvar elib:+the-unset-slot+)

(defvar elib:+the-any-map-guard+)
(defvar elib:+the-flex-list-guard+)
(defvar elib:+the-make-const-map+)
(defvar elib:+the-make-twine+)
(defvar elib:+the-text-writer-guard+)

(defvar elib:+the-make-type-desc+)
(defvar elib:+the-make-param-desc+)
(defvar elib:+the-make-message-desc+)

(defvar elib:+e-false+)
(defvar elib:+e-true+)
(defvar elib:+the-exception-guard+)
(defvar elib:+the-map-guard+)

(defvar elib:+the-make-flex-map+)
(defvar elib:+the-make-text-writer+)
(defvar elib:+the-make-list+)
(defvar elib:+the-make-int+)
(defvar elib:+the-make-sorted-queue+)

(defvar elib:+the-e+)

(defvar elib:+the-audit-checker+)
(defvar elib:+deep-frozen-stamp+)

(defvar elib:+the-make-weak-ref+)
(defvar elib:+the-make-traversal-key+)
(defvar elib:+the-make-proxy-resolver+)

(declaim (ftype function
  e.elib:make-equalizer
  e.elib:cl-type-simple-expr
  e.elib:eeq-same-yet-hash
  e.elib:make-text-writer-to-cl-stream))

(cl:defpackage :e.elib.tables
  (:nicknames :e.tables)
  (:use :cl :elib :net.hexapodia.hashtables)
  (:documentation "Collection implementations.")
  (:export
    :const-map ; type
    :+the-make-array+))

(defvar e.elib.tables:+the-make-array+)

(cl:defpackage :e.knot
  (:use :cl :elib)
  (:export
    :scope
    :make-safe-scope
    :make-io-scope
    
    :found-e-on-java-home
    :*emaker-search-list*))
  
(cl:defvar e.knot:*emaker-search-list*)

(cl:defpackage :e.elang.vm-node 
  (:nicknames)
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
    :|CdrPattern|
    :|DefineExpr|
    :|EExpr|
    :|EMatcher|
    :|EMethod|
    :|ENode|
    :|EScript|
    :|EscapeExpr|
    :|FinalPattern|
    :|FinallyExpr|
    :|HideExpr|
    :|IfExpr|
    :|IgnorePattern|
    :|ListPattern|
    :|LiteralExpr|
    :|MatchBindExpr|
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
    :|SuchThatPattern|
    :|VarPattern|)
  #+sbcl (:lock t))

(cl:defpackage :e.elang
  (:nicknames :elang)
  (:use :cl :elib :e.elang.vm-node)
  (:export
    :eval-e
    :get-translation
    
    :+the-make-static-scope+))

(cl:defvar elang:+the-make-static-scope+)

(declaim (ftype function
  e.elang:eval-e
  e.elang:get-translation))

(cl:defpackage :e.elang.syntax
  (:nicknames :e.syntax)
  (:use :cl :e.elib :e.elang :e.elang.vm-node)
  (:export
    +e-printer+
    
    :e-source-to-tree
    
    :load-parse-cache
    :save-parse-cache
    :load-parse-cache-file
    :save-parse-cache-file
    :with-parse-cache-file
    
    :+prim-parser+
  ))

(cl:defvar e.elang.syntax:+prim-parser+)
(cl:defvar e.elang.syntax:+e-printer+)

(declaim (ftype function 
  e.elang.syntax:e-source-to-tree
  e.elang.syntax:load-parse-cache
  e.elang.syntax:save-parse-cache
  e.elang.syntax:load-parse-cache-file
  e.elang.syntax:save-parse-cache-file))

(cl:defpackage :e.extern
  (:use :common-lisp :elib :elang)
  (:documentation "Capability-structured IO, and interfaces to libraries.")
  (:export
    :+standard-external-format+
    :+standard-external-format-common-name+
    :+gc+
    :make-file-getter
    :read-entire-file
    :+the-timer+
    :+rx-perl5-compiler+
    :+rx-perl5-matcher+))

(defvar e.extern:+standard-external-format+)
(defvar e.extern:+standard-external-format-common-name+)
(defvar e.extern:+gc+)
(defvar e.extern:+the-timer+)
(defvar e.extern:+rx-perl5-compiler+)
(defvar e.extern:+rx-perl5-matcher+)

(declaim (ftype function
  e.extern:make-file-getter
  e.extern:read-entire-file))

(cl:defpackage :e.rune
  (:use :cl :e.util :e.elib :e.knot)
  (:documentation "Bootstrapping: setting up the E runtime environment after clrune starts the Lisp process.")
  (:export
    :rune))

(cl:defpackage :e.elib.vtable-methods
  (:use)
  (:documentation "For profiling purposes, vtable method functions may be interned in this package so they can be operated on later."))

