; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.antlr-parser-system
  (:use :cl :asdf))
(in-package :e.antlr-parser-system)

;; XXX see if we can make antlr-source-file use antlr-java-source-file

(defclass antlr-java-source-file (java-source-file) 
  ())

(defclass antlr-source-file (source-file) 
  ((java-class-name :initarg java-class-name
                    :reader java-class-name)
   (export-vocab-name :initarg export-vocab-name
                      :reader export-vocab-name)))

(defclass antlr-static-vocabulary-file (source-file)
  ())

(defmethod source-file-type ((c antlr-source-file) (s module)) "g")
(defmethod source-file-type ((c antlr-java-source-file) (s module)) "java")
(defmethod source-file-type ((c antlr-static-vocabulary-file) (s module)) "txt")

(defmethod output-files ((operation compile-op) (c antlr-source-file))
  (flet ((mp (name suffix type)
           (make-pathname :defaults (component-pathname c)
                          :name (concatenate 'string name suffix)
                          :type type
                          :version nil)))
    (list (mp (java-class-name c) "" "java")
          (mp (java-class-name c) "" "class")
          (mp (export-vocab-name c) "TokenTypes" "txt")
          (mp (export-vocab-name c) "TokenTypes" "java")
          (mp (export-vocab-name c) "TokenTypes" "class"))))

(defmethod output-files ((operation compile-op) (c antlr-java-source-file))
  (list (make-pathname :defaults (component-pathname c)
                       :type "class"
                       :version nil)))

(defmethod output-files ((operation compile-op) (c antlr-static-vocabulary-file))
  "for compatibility with output-file redirectors; see below"
  (list (component-pathname c)))
  
(defmethod output-files :around ((operation compile-op) (c antlr-static-vocabulary-file))
  ;; Note that this is the most-specific around method, so is invoked outermost.
  "If an output-file redirector did something, then keep it; otherwise, we don't actually have an output file."
  (let ((o (call-next-method)))
    (unless (equal o (list (component-pathname c)))
      o)))

(defun same-enclosing-directory (file-pathnames)
  "Find the directory which all of the named files are in."
  (loop with wild-path = nil
        for file in file-pathnames
        do (if wild-path
             (assert (pathname-match-p file wild-path))
             (setf wild-path (make-pathname :defaults file
                                            :name :wild
                                            :type :wild
                                            :version :wild)))
        finally (assert wild-path)
                (return (make-pathname :defaults wild-path
                                       :name nil
                                       :type nil
                                       :version nil))))

(defmethod perform ((op compile-op) (component antlr-source-file))
  ;; ANTLR seems to not write the token files if they already exist; this confuses asdf into recompiling every time as they look stale, so we make sure everything is built
  (map nil #'delete-file (output-files op component))
  (let* ((output-files (output-files op component))
         (output-dir (same-enclosing-directory output-files))
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

(defmethod perform ((op compile-op) (component antlr-java-source-file))
  (let* ((output-dir (same-enclosing-directory (output-files op component)))
         (code (run-shell-command "javac -classpath ~S:~S:~S -d ~S ~S"
                                  (namestring e.knot::*antlr-jar*)
                                  (namestring output-dir)
                                  (namestring (make-pathname
                                                :defaults (component-pathname component)
                                                :name nil
                                                :type nil
                                                :version nil))
                                  (namestring output-dir)
                                  (namestring (component-pathname component)))))
      (unless (= code 0)
        (case (operation-on-failure op)
          (:warn (warn "~@<Java compiler failure when performing ~A on ~A.~@:>"
                       op component))
          (:error
           (error 'compile-failed :operation op :component component))))
      ()))

(defmethod perform ((op compile-op) (component antlr-static-vocabulary-file))
  "For compatibility with output-file redirectors such as common-lisp-controller. ANTLR apparently expects the vocabulary files to be in the directory specified with -o, so we copy the file to that location if it differs."
  (let ((input-file (component-pathname component))
        (output-files (output-files op component)))
    (ecase (length output-files)
      (1 (let ((output-file (first output-files)))
           (assert (not (pathname-match-p output-file input-file)))
             (cl-fad:copy-file input-file output-file :overwrite t)))
      (0))))

(defmethod perform ((op load-op) (component antlr-source-file))
  "no-op since the loading isn't done to this process"
  (values))

(defmethod perform ((op load-op) (component antlr-java-source-file))
  "no-op since the loading isn't done to this process"
  (values))

(defmethod perform ((op load-op) (component antlr-static-vocabulary-file))
  (values))

  
(defsystem e-on-cl.antlr-parser
  ;; This system actually depends on E-on-CL, but E-on-CL needs to load it
  ;; midway through its own loading, which asdf doesn't understand sufficiently.
  :depends-on (#+(or) :e-on-cl)
  :pathname (merge-pathnames #p"antlr/" (component-pathname (find-system :e-on-cl)))
  :components
    ((:antlr-java-source-file "ExtAST")
     (:antlr-java-source-file "ExtToken"
                              :depends-on ("e"))
     (:antlr-java-source-file "CountingCharBuffer")
     (:antlr-java-source-file "CountingLexerSharedInputState")
     (:antlr-static-vocabulary-file "CommonTokenTypes")
     (:antlr-source-file "e"
                         java-class-name "EParser"
                         export-vocab-name "E"
                         :depends-on ("CommonTokenTypes"))
     (:antlr-source-file "elex"
                         java-class-name "EALexer"
                         export-vocab-name "EALexer"
                         :depends-on ("e" "CountingLexerSharedInputState"))
     (:antlr-source-file "quasi"
                         java-class-name "QuasiLexer"
                         export-vocab-name "QuasiLexer")))
