; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.antlr-parser-system
  (:use :cl :asdf))
(in-package :e.antlr-parser-system)

(defclass antlr-source-file (source-file) 
  ((output-names :initarg :output-names)))

(defmethod source-file-type ((c antlr-source-file) (s module)) "g")

(defmethod output-files ((operation compile-op) (c antlr-source-file))
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
                                  (namestring e.knot::*antlr-jar*)
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
                                    (namestring e.knot::*antlr-jar*)
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

(defsystem e-on-cl.antlr-parser
  :depends-on (:e-on-cl)
  :pathname #.(merge-pathnames #p"antlr/" (component-pathname (find-system :e-on-cl)))
  :components
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
                                        ("QuasiLexerTokenTypes" "java")))))
