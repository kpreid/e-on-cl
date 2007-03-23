; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
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
               :cl-ppcre
               :genhash
               :trivial-garbage
               #+sbcl :sb-bsd-sockets)
  :components
  ((:module "lisp" :components
    ((:file "packages")
     (:file "compile-options")
     (:file "util"
            :depends-on ("compile-options" "packages"))
     (:file "util-guts"
            :depends-on ("util"))
     (:file "base"
            :depends-on ("util" "packages"))
     (:file "elib"
            :depends-on ("compile-options" "util" "base" "packages"))
     (:file "sugar" 
            ; convenience macros for E semantics
            :depends-on ("elib"))
     (:file "guard"
            :depends-on ("elib" "sugar"))
     (:file "elib-guts" 
            ; the non-load-order-depended-on portion of elib
            :depends-on ("elib" "sugar"))
     (:file "proxy"
            :depends-on ("elib" "sugar"))
     (:file "same"
            ; Equalizer, hashing, etc
            :depends-on ("elib" "sugar"))
     (:file "elib-values"
            :depends-on ("elib" "sugar"))
     (:file "tables2"
            :depends-on ("sugar" 
                         "same" ; genhash's register-hash-function needs the values #'same-hash #'samep
                         "guard")) 
     (:file "print"
            ; TextWriter
            :depends-on ("elib" "sugar"))
     (:file "extern"
            :depends-on ("elib" "sugar"))
     (:file "random"
            :depends-on ("elib"))
     (:file "sockets")
     (:file "sockets2"
            :depends-on ("sockets"))
     (:file "comm"
            :depends-on ("elib" "sugar"))
     (:file "knot-base"
            :depends-on ("elib" "sugar"
                         "elib-values" #|making tracer does e-print, needing string vtable to be established|#))
     (:file "knot"
            :depends-on ("knot-base" "extern" "tables2" "comm" "sugar"
                         "elib-guts" #| XXX determine the necessity of this - violates elib-guts's description |#
                         "elang-nodes" #| +the-make-static-scope+ |#
                         "print" #| constructing global tracers |#
                         "random" #| entropy |#
                         "proxy" #| +the-make-proxy+ |#))
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
                         "elib-guts" #| needs e-simple-slot class definition - XXX violates elib-guts's description |#))
     (:file "compiler-seq"
            :depends-on ("compiler-base"))
     (:file "compiler-entry"
            :depends-on ("compiler-base"
                         "sugar" 
                         "knot-base" #| for scopes |#
                         "knot" #| for +lisp+ |#))
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
