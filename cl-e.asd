; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defsystem cl-e
  :name "cl-e"
  :depends-on (:cl-ppcre :genhash :cl-fad)
  :components ((:module "lisp" :components
    ((:file "packages")
     (:file "compile-options")
     (:file "util"
            :depends-on ("packages"))
     (:file "base"
            :depends-on ("util" "packages"))
     (:file "elib"
            :depends-on ("compile-options" "util" "base" "packages"))
     (:file "elib-guts" ; the non-load-order-depended-on portion of elib
            :depends-on ("elib"))
     (:file "elib-values"
            :depends-on ("elib"))
     (:file "elib-tables"
            :depends-on ("elib" "elib-values"))
     (:file "tables2"
            :depends-on ("elib-tables" ; for generics, superclasses, etc
                         "elib-guts")) ; genhash's register-hash-function needs #'eeq-same-yet-hash #'eeq-is-same-ever
     (:file "extern"
            :depends-on ("elib"))
     (:file "knot"
            :depends-on ("elib"))
     (:file "elang"
            :depends-on ("elib-tables" "tables2" "elib-values" "elib" "util" "packages"))
     (:file "elang-nodes"
            :depends-on ("elang"))
     (:file "syntax"
            :depends-on ("packages" "elang-nodes"))
     (:file "elang-transform"
            :depends-on ("elang" "elang-nodes"))
     (:file "rune"
            :depends-on ("elang" "knot"))))))

(defsystem cl-e.updoc
  :name "cl-e.updoc"
  :depends-on (:cl-e)
  :components ((:module "lisp" :components
    ((:file "updoc")))))

;(defsystem cl-e.ircrepl
;  :name "cl-e.ircrepl"
;  :depends-on (:cl-e)
;  :components ((:module "lisp" :components
;    ((:file "ircrepl")))))


; cl-ppcre, as of 1.0.0 to 1.2.1, causes SBCL to produce style-warnings while loading it. This muffles them.
; XXX consider whether this should still exist in production
; contributed by antifuchs of irc://irc.freenode.net/lisp -- http://paste.lisp.org/display/4831

(dolist (component (asdf:module-components (find-system :cl-ppcre)))
  (eval `(defmethod asdf:perform :around ((op asdf:load-op) (c (eql ',component)))
          (handler-bind ((style-warning #'muffle-warning))
            (call-next-method op c)))))
