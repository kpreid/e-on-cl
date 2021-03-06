; Copyright 2005-2009 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.system
  (:use :cl :asdf)
  (:export :e-core-source-file))
(in-package :e.system)

(defclass e-core-source-file (source-file)
  ())
(defmethod source-file-type ((c e-core-source-file) (s module)) "eexpr")
(defmethod output-files ((operation compile-op) (c e-core-source-file))
  (list (compile-file-pathname (component-pathname c))))
;; The perform method is defined later in compiler-entry.lisp.

(defsystem e-on-cl
  :depends-on (:bordeaux-threads
               :cl-fad
               :cl-json
               :cl-ppcre
               :genhash
               :ironclad
               :trivial-garbage
               :yacc

               #+sbcl :sb-bsd-sockets
               #| sb-introspect, but that isn't asdf (loaded from util.lisp) |#)
  :components
  ((:module "lisp" :components
    ((:file "packages")
     (:file "compile-options")
     (:file "util"
            :depends-on ("compile-options" "packages"))
     (:file "util-guts"
            :depends-on ("util"))
     (:file "bordeaux-threads-patch"
            :depends-on ("util"))
     (:file "queues"
            :depends-on ("packages"))
     (:file "base"
            :depends-on ("util" "packages"))
     (:file "event-log"
            :depends-on ("util" "packages"))
     (:file "runner"
            :depends-on ("base" "util" "packages" "queues"))
     (:file "runner-late"
            ; runner code that depends on elib/ref definitions
            :depends-on ("runner" "elib" "sugar"))
     (:file "serve-event"
            ; serve-event-based runner (planned to be sbcl/cmucl only)
            :depends-on ("runner" "queues"))
     (:file "ref"
            ; ref protocol and basic ref classes
            :depends-on ("util" "base" "packages" "event-log"))
     (:file "type-desc-early"
            :depends-on ("util"))
     (:file "smethod"
            :depends-on ("compile-options" "ref" "type-desc-early"))
     (:file "e-lambda"
            :depends-on ("smethod"))
     (:file "miranda"
            :depends-on ("compile-options" "ref" "smethod"))
     (:file "elib"
            ; def-vtable, stamps, misc.
            :depends-on ("ref" "smethod" "e-lambda" "compile-options" "util" "base" "packages" "event-log"))
     (:file "vat"
            ; vats, turns, e-send-dispatch on near refs
            :depends-on ("runner" "ref" "util" "packages" "event-log"))
     (:file "lazy"
            :depends-on ("ref"))
     (:file "sugar" 
            ; convenience operators for E semantics
            :depends-on ("ref" "elib" "vat"))
     (:file "guard"
            :depends-on ("elib" "sugar" "type-desc-early" "miranda"))
     (:file "slots"
            :depends-on ("vat" "sugar" "elib"))
     (:file "elib-guts" 
            ; the non-load-order-depended-on portion of elib
            :depends-on ("elib" "sugar" "queues"))
     (:file "proxy"
            :depends-on ("elib" "sugar"))
     (:file "same"
            ; Equalizer, hashing, etc
            :depends-on ("elib" "sugar"))
     (:file "elib-values"
            :depends-on ("elib" "sugar" "lazy"))
     (:file "float"
            :depends-on ("base" #| NaN/inf |#
                         "elib" #| def-vtable |#
                         "sugar" #| defobject |#))
     (:file "tables2"
            :depends-on ("sugar" 
                         "same" ; genhash's register-hash-function needs the values #'same-hash #'samep
                         "guard"
                         "lazy")) 
     (:file "print"
            ; TextWriter
            :depends-on ("elib" "sugar"))
     (:file "extern"
            :depends-on ("elib" "sugar"))
     (:file "random"
            :depends-on ("elib"))
     (:file "sockets"
            :depends-on ("elib"))
     (:file "sockets2"
            :depends-on ("sockets" "sugar"))
     (:file "comm"
            :depends-on ("elib" "sugar"))
     (:file "environment"
            :depends-on ("elib" "sugar"))
     (:file "knot-base"
            :depends-on ("elib" "sugar"
                         "elib-values" #|making tracer does e-print, needing string vtable to be established|#
                         "slots" #| for e-simple-slot |#))
     (:file "knot"
            :depends-on ("knot-base" "extern" "tables2" "comm" "sugar"
                         "environment" #| making scopes |#
                         "elib-guts" #| XXX determine the necessity of this - violates elib-guts's description |#
                         "elang-nodes" #| +the-make-static-scope+ |#
                         "print" #| constructing global tracers |#
                         "random" #| entropy |#
                         "proxy" #| +the-make-proxy+ |#
                         "slots" #| slot makers |#))
     (:file "elang-nodes"
            :depends-on ("elib" 
                         "util"
                         "sugar" 
                         "elib-values"
                         "tables2"))
     (:file "nonkernel"
            :depends-on ("elang-nodes"))
     (:file "syntax"
            :depends-on ("packages" "elang-nodes" "nonkernel" "sugar"))
     (:file "compiler-base"
            :depends-on ("packages"
                         "elang-nodes"
                         "sugar"
                         "slots"))
     (:file "compiler-seq"
            :depends-on ("compiler-base"))
     (:file "compiler-entry"
            :depends-on ("compiler-base"
                         "sugar" 
                         "environment"
                         "knot" #| for +lisp+ and +shared-safe-scope+ |#))
     (:file "rune"
            :depends-on ("elib" "compiler-entry" "knot" "syntax"))
     
     (:file "antlr-system"
            :depends-on ("knot"))
     (:file "final-init"
            :depends-on ("knot"))))
   (:module "e-core" :default-component-class e-core-source-file
                     :depends-on ("lisp")
                     :components
    ((:file "knot")))))

;;; --- Auxiliary systems ---

(defsystem e-on-cl.jar
  :depends-on (#|:e-on-cl would be circular|# :zip)
  :components ((:module "lisp" :components
    ((:file "jar")))))

(defsystem e-on-cl.updoc
  :depends-on (:e-on-cl)
  :components ((:module "lisp" :components
    ((:file "updoc")))))

(defsystem e-on-cl.irc-repl
  :depends-on (:cl-irc :e-on-cl)
  :components ((:module "lisp" :components
    ((:file "irc-repl")))))

;;; --- Gimmicks ---

; cl-ppcre, as of 1.0.0 to 1.2.1, causes SBCL to produce style-warnings while loading it. This muffles them.
;; XXX this should be done only if we're loading E-on-CL as standalone (via clrune)
; contributed by antifuchs of irc://irc.freenode.net/lisp -- http://paste.lisp.org/display/4831

(dolist (component (asdf:module-components (find-system :cl-ppcre)))
  (eval `(defmethod asdf:perform :around ((op asdf:load-op) (c (eql ',component)))
          (handler-bind ((style-warning #'muffle-warning))
            (call-next-method op c)))))

;;; --- Testing ---

(defmethod perform ((op test-op) (system (eql (find-system :e-on-cl))))
  (operate 'load-op :e-on-cl.lisp-test)
  (operate 'load-op :e-on-cl.updoc)
  (funcall (intern "SYSTEM-TEST" "E.LISP-TEST") op system)
  (funcall (intern "SYSTEM-TEST" "E.UPDOC")     op system))

;; Use RT unless under SBCL *and* there is no RT
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-system :rt nil)
    (pushnew :e.sb-rt *features*)))

(defsystem e-on-cl.lisp-test
  :depends-on (:e-on-cl
               #+e.sb-rt :sb-rt
               #-e.sb-rt :rt)
  :components ((:module "lisp" :components
                ((:file "lisp-test")))))
