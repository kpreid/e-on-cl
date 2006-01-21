; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defclass antlr-source-file (source-file) 
  ((output-names :initarg :output-names)))

(defmethod source-file-type ((c antlr-source-file) (s module)) "g")

(defmethod output-files ((operation compile-op) (c antlr-source-file))
  ;; xxx not bothering to predict what files antlr will generate
  (loop for (name type) in (slot-value c 'output-names) 
        collect
          (make-pathname :defaults (component-pathname c)
                         :name name
                         :type type
                         :version nil)
        when (string= type "java")
          collect
            (make-pathname :defaults (component-pathname c)
                           :name name
                           :type "class"
                           :version nil)))

(defmethod perform ((op compile-op) (component antlr-source-file))
  (let* ((output-files (output-files op component))
         (output-dir
           (loop with wild-path = nil
                 for output-file in output-files
                 do (if wild-path
                      (assert (pathname-match-p output-file wild-path))
                      (setf wild-path (make-pathname :defaults output-file
                                                     :name :wild
                                                     :type :wild
                                                     :version :wild)))
                 finally (assert wild-path)
                         (return (make-pathname :defaults wild-path
                                                :name nil
                                                :type nil
                                                :version nil))))
         (code (run-shell-command "java -classpath ~S antlr.Tool -o ~S ~S"
                                  "/Stuff/e/e.jar" #|XXX antlr loc|#
                                  ;; XXX should be native-namestring when
                                  ;; possible
                                  (namestring output-dir)
                                  (namestring 
                                    (component-pathname component)))))
    (unless (= code 0)
      (case (operation-on-failure op)
        (:warn (warn "~@<Antlr compiler failure when performing ~A on ~A.~@:>"
                     op component))
        (:error
         (error 'compile-failed :operation op :component component))))
    (let* ((code (run-shell-command "javac -classpath ~S:~S~{ ~S~}"
                                    "/Stuff/e/e.jar" #|XXX antlr loc|#
                                    (namestring output-dir)
                                    (loop for file in output-files
                                          when (string= (pathname-type file)
                                                        "java")
                                          collect (namestring file)))))
      (unless (= code 0)
        (case (operation-on-failure op)
          (:warn (warn "~@<Java compiler failure when performing ~A on ~A.~@:>"
                       op component))
          (:error
           (error 'compile-failed :operation op :component component))))
      ())))

(defmethod perform ((op load-op) (component antlr-source-file))
  "no-op"
  (values))


(defsystem e-on-cl
  :depends-on (:cl-ppcre :genhash :cl-fad)
  ; cl-ppcre dependency could be made lazy
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
       (:file "elib-guts" 
              ; the non-load-order-depended-on portion of elib
              :depends-on ("elib"))
       (:file "elib-values"
              :depends-on ("elib" "sugar"))
       (:file "tables2"
              :depends-on ("sugar" "elib-guts")) ; genhash's register-hash-function needs the values #'eeq-same-yet-hash #'eeq-is-same-ever
       (:file "print"
              ; TextWriter
              :depends-on ("elib"))
       (:file "extern"
              :depends-on ("elib"))
       (:file "knot-base"
              :depends-on ("elib" "sugar"))
       (:file "knot"
              :depends-on ("knot-base" "extern"))
       (:file "elang-nodes"
              :depends-on ("elib" 
                           "util"
                           "sugar" 
                           "elib-values"
                           "tables2"))
       (:file "syntax"
              :depends-on ("packages" "elang-nodes"))
       (:file "compiler-base"
              :depends-on ("packages" "elang-nodes"))
       (:file "compiler-seq"
              :depends-on ("compiler-base"))
       (:file "compiler-entry"
              :depends-on ("compiler-base"))
       (:file "rune"
              :depends-on ("elib" "compiler-entry" "knot"))))
     
     (:module "antlr" :components 
       ((:antlr-source-file "e"
                            :output-names (("EParser" "java") 
                                           ("ETokenTypes" "txt")
                                           ("ETokenTypes" "java")))
        (:antlr-source-file "elex"
                            :output-names (("EALexer" "java") 
                                           ("EALexerTokenTypes" "txt")
                                           ("EALexerTokenTypes" "java"))
                            :depends-on ("e"))
        (:antlr-source-file "quasi"
                            :output-names (("QuasiLexer" "java") 
                                           ("QuasiLexerTokenTypes" "txt")
                                           ("QuasiLexerTokenTypes" "java")))
        #||#))))

;;; --- Auxiliary systems ---

(defsystem e-on-cl.jar
  :depends-on (:e-on-cl :zip)
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

;; This is a separate system because I decided to lazily load the Lisp-side socket code, and so the relevant taming declarations cannot be loaded until the socket system is. -- kpreid 2005-04-30
(defsystem e-on-cl.sockets
  :depends-on (:e-on-cl #+sbcl :sb-bsd-sockets)
  :components ((:module "lisp" :components
    ((:file "sockets")
     (:file "sockets2"
            :depends-on ("sockets"))))))

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
