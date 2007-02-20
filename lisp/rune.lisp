; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.rune)

(defvar *parse-cache-name* nil)

(defun global-exit (status)
  (check-type status (unsigned-byte 8))
  (when *parse-cache-name*
    (e.syntax:save-parse-cache-file *parse-cache-name*))

  ;; XXX TODO: restore the load counting mechanism
  #+(or)
  (maphash #'(lambda (fqn times)
             (when (> times 1)
               (warn "note: ~A loaded ~A times" fqn times)))
           (e. e.knot::+default-fresh-emaker-loader+
               |_getLoadCounts|))
  
  #+e.instrument.ref-shorten-uses
    (let ((entries (maphash #'list e.elib::*instrument-ref-shorten-kinds*)))
      (mapcar #'(lambda (l) (destructuring-bind (key times) l
               (when (> times 0)
                 (efuncall e.knot:+sys-trace+ (format nil "profiling: ~A shortened ~A times" key times)))))
              (sort entries #'< :key #'second)))

  #+sbcl
    (sb-ext:quit :unix-status status)
  #+ccl
    (ccl:quit status)
  #+clisp
    (ext:quit status)
  #+cmu
    (unix:unix-exit status)
  #+abcl
    (ext:quit :status status)
  #+allegro
    (excl:exit status :quiet t)
  (cl-user::quit))


(defvar *toplevel-documentation* (make-hash-table))

(defmacro define-toplevel (name lambda-list doc &body body)
  "Just like DEFUN but guaranteed to save the documentation, in *toplevel-documentation*."
  `(progn
     (setf (gethash ',name *toplevel-documentation*) ',doc)
     (defun ,name ,lambda-list ,doc ,@body)))

(define-toplevel script-toplevel (args)
  (generic-toplevel "rune script toplevel"
    (lambda ()
      (let* ((scope (make-io-scope :stdout *standard-output* :stderr *error-output*))
             (Ref (eelt scope "Ref")))
        (e. Ref |whenResolved| (efuncall (eelt scope "rune") 
                                        (coerce args 'vector))
          (e-lambda "org.erights.e.elang.interp.RuneTerminator" ()
            (:|run| (outcome 
                &aux (opt-problem (e. Ref |optProblem| outcome)))
              (when opt-problem
                (format *error-output* "# problem: ~A" (e-quote (e-problem-unseal opt-problem))))
              outcome)))))))

(defun generic-toplevel (label starter)
  (declare (ignore label)) ;; XXX reestablish use? formerly vat label
  (call-when-resolved
    (funcall starter)
    (efun (result)
      (global-exit (if (ref-opt-problem result) 255 0))))
  (top-loop))

(define-toplevel repl-toplevel (args)
  "Enter the Lisp-implemented E REPL."
  (assert (null args))
  (generic-toplevel "rune repl toplevel"
                    (system-symbol "REPL-START" "E.UPDOC" :e-on-cl.updoc)))

(define-toplevel selftest-toplevel (args)
  "Run the tests for E-on-CL."
  (assert (zerop (length args)))
  (let ((*vat* nil)
        (*runner* nil)) ;; XXX kludge
    (time (asdf:operate 'asdf:test-op +the-asdf-system+)))
  (force-output)
  (global-exit 0))

(define-toplevel lisptest-toplevel (args)
  "Run those tests for E-on-CL which do not involve executing the E language."
  (assert (zerop (length args)))
  (let ((*vat* nil)
        (*runner* nil)) ;; XXX kludge
    (funcall (system-symbol "SYSTEM-TEST" :e.lisp-test :e-on-cl.lisp-test) nil +the-asdf-system+))
  (force-output)
  (global-exit 0))

(define-toplevel updoc-toplevel (args)
  "Invoke the Lisp-implemented Updoc implementation, interpreting the
      arguments as Updoc file pathnames."
  (generic-toplevel "rune updoc toplevel"
    (lambda () (apply (system-symbol "UPDOC-RUNE-ENTRY" "E.UPDOC" :e-on-cl.updoc) args))))

(define-toplevel irc-repl-toplevel (args)
  "Start an IRC bot. The arguments are: <nick> <server> <channel>*"
  (generic-toplevel "rune irc toplevel"
    (lambda ()
      (apply (system-symbol "START-IRC-REPL" :e.irc-repl :e-on-cl.irc-repl) args)
      (make-promise))))

(define-toplevel translate-toplevel (args)
  "Parse the sole argument as E source and print the Common Lisp form
      it is compiled into."
  (assert (= 1 (length args)))
  (generic-toplevel "rune --translate toplevel"
    (lambda () 
      (print (e.elang:get-translation (e.syntax:parse-to-kernel (first args))))
      (fresh-line)
      (force-output)
      nil)))

(define-toplevel stale-test-toplevel (args)
  (check-type args null)
  (let ((answer (stale)))
    (when answer
      (format *trace-output* "~&; Stale: ~A~%" answer))
    (global-exit (if answer 1 0))))

(define-toplevel nothing-toplevel (args)
  "Do nothing. In particular, do not exit, and do not run the vat.
      This usually results in a Lisp REPL."
  (declare (ignore args))
  (values))

(defvar *image-is-detached*)
(setf (documentation '*image-is-detached* 'variable)
  "Indicates, if bound, whether the current Lisp image is intended to be an independent packaged E-on-CL system (T), or is a caching of loaded source files (NIL).")

(define-toplevel save-toplevel (args)
  "Generate an image file of the Lisp implementation with E-on-CL loaded.
      If supported, it will be executable. Under CLISP, the first argument to
      the resulting executable must be \"--\" for correct argument processing.
      The sole argument is the name of the image file to create."
  (assert (= 2 (length args))) ;; will be options later, regarding how much to include
  (let ((executable-file (native-pathname (first args)))
        (secondary-file (native-pathname (second args)))
        (executable-p t)
        #| XXX todo: (may-enter-debugger t) |#)
    
    (save-flush)
    (setf *image-is-detached* nil)
    
    #.(locally 
        (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note)) 
        (or
          #+sbcl
          (quote
            (progn
              (with-open-file (script-out executable-file :direction :output)
                (format script-out
                  "#!/bin/sh~%~
                   exec ~S --noinform --end-runtime-options \"$@\"~%"
                  (native-namestring (merge-pathnames secondary-file))))
              ;; XXX replace the below with sb-posix:chmod and a proper mode computation
              (run-program "chmod" 
                (list "+x" 
                      (native-namestring (merge-pathnames executable-file)))
                :wait t
                :search t)
              (sb-ext:save-lisp-and-die
                secondary-file
                :executable executable-p
                :toplevel #'%revive-start)))
          #+cmu
          (quote
            (ext:save-lisp 
              executable-file
              :purify t
              :init-function #'%revive-start
              :load-init-file nil
              :site-init nil
              :batch-mode nil
              :print-herald nil
              :process-command-line nil
              :executable executable-p))
          #+openmcl
          (quote
            ;; reference: http://www.clozure.com/pipermail/openmcl-devel/2003-May/001062.html
            (ccl:save-application
              executable-file
              :toplevel-function #'%revive-start
              :prepend-kernel executable-p))
          #+clisp
          (quote
            (ext:saveinitmem
              executable-file
              :quiet t
              ; :norc t ; XXX is this appropriate?
              :init-function #'%revive-start
              :start-package (find-package :cl-user)
              :keep-global-handlers nil ; XXX is this appropriate?
              :executable executable-p))
          '(error "Saving an executable/image is not supported for ~A." (lisp-implementation-type))))
        
    ;; depending on the implementation (e.g. sbcl's save-lisp-and-die) we may
    ;; or may not reach here
    (global-exit 0)))

#+sbcl (defvar *sbcl-home-transport*)

(defun save-flush ()
  (assert *vat*)

  (setf *parse-cache-name* nil)
  
  ;; make sure we've loaded everything lispy, in order to simplify the
  ;; stuff-changing-out-from-under-our-image problem
  (asdf:operate 'asdf:load-op :e-on-cl.updoc)
  #|XXX review what should go here|#
  
  ;; workaround for sbcl/asdf bugs as of 0.9.11.24: unset SBCL_HOME causes FIND-SYSTEM to fail
  #+(and sbcl (or))
  (progn
    (setf asdf:*system-definition-search-functions*
      (remove 'asdf::contrib-sysdef-search
              asdf:*system-definition-search-functions*))
    (setf asdf:*central-registry*
      (remove 'asdf::contrib-sysdef-search
              asdf:*central-registry*))
              :test #'equal)
  
  #+sbcl
  (setf *sbcl-home-transport* (sb-ext:posix-getenv "SBCL_HOME"))

  ;; XXX why is this a feature? why not an ordinary variable?
  (setf *features* (delete :e.saving-image *features*))
  
  (e. (vat-safe-scope *vat*) |iterate| (efun (k v)
    (e. e.knot:+sys-trace+ |doing| (format nil "preloading ~A" k) (efun ()
      (e. (e. v |getValue|) |__getAllegedType|)))))
  
  (e.syntax::sub-save-flush)
  
  (values))

(defun revive-flush ()
  #+sbcl (progn
    (sb-posix:putenv (format nil "SBCL_HOME=~A" *sbcl-home-transport*))
    (makunbound '*sbcl-home-transport*))

  #| XXX review what should go here
     
     - should we somehow arrange so that changes to an asdf system in our dependencies while we're running won't reload it? |#
  (values))

;;; --- Staleness testing ---

(defun deep-operation-done-p (operation component)
  (loop for (o . c) in (asdf::traverse operation component) 
        always (asdf:operation-done-p o c)))

(defun operation-used-p (operation component)
  (and (gethash (type-of operation)
                (asdf::component-operation-times component))
       t))

(defun defined-systems ()
  (map-from-hash 'list 
                 (lambda (k v) (declare (ignore k)) (cdr v)) 
                 asdf::*defined-systems*))


(defun file-incorporated (vat file time)
  (push (list file time) (incorporated-files vat)))

(defun stale-files (vat)
  (some (lambda (record)
          (destructuring-bind (file time) record
            (when (/= time (e. file |_clFileWriteDate|))
              record)))
        (incorporated-files vat)))


(defun stale ()
  (or (let ((op (make-instance 'asdf:load-op :original-initargs nil)))
        (not (every (lambda (system) 
                      (or (not (operation-used-p op system))
                          (deep-operation-done-p op system)))
                    (defined-systems))))
      (and *vat*
           (stale-files *vat*))))

;;; --- End of staleness testing ---

(defun %revive-start ()
  (revive-flush)
  (apply #'rune (or #+sbcl (rest sb-ext:*posix-argv*)
                    #+clisp ext:*args*
                    #+openmcl (rest ccl::*command-line-argument-list*)
                    #+cmucl (rest ext:*command-line-strings*)))
  (break "Fell out of ~S." 'rune)
  ;; XXX have an overall exit status policy
  (global-exit 255))

(defparameter *toplevels* '(("--updoc"      updoc-toplevel)
                            ("--selftest"   selftest-toplevel)
                            ("--lisptest"   lisptest-toplevel)
                            ("--translate"  translate-toplevel)
                            ("--lrepl"      repl-toplevel)
                            ("--irc"        irc-repl-toplevel)
                            ("--nothing"    nothing-toplevel)
                            ("--save"       save-toplevel)
                            ("--not-stale?" stale-test-toplevel)))

(defun document-toplevels (stream data colon at)
  (declare (ignore colon at))
  (destructuring-bind (option function) data
    (format stream "~:
  ~A
      Action-selecting option:
      ~A~%"
                   option
                   (gethash function *toplevel-documentation*))))

(defun rune (&rest args
    &aux (*break-on-signals*   *break-on-signals*)
         (*break-on-ejections* *break-on-ejections*)
         (toplevel             #'script-toplevel)
         parse-cache-name 
         do-usage)
  (declare (optimize (debug 2) (safety 3)))
  
  (let ((*package* (find-package :cl-user)))
  (loop while args do
    (popping-equal-case args
      (("--help")
        (setf do-usage t))
      (("--parse-cache" "-p")
        (setf parse-cache-name (native-pathname (pop args))))
      (("--bos" "--break-on-signals")
        (setf *break-on-signals* (read-from-string (pop args))))
      (("--boe" "--break-on-ejections")
        (setf *break-on-ejections* (read-from-string (pop args))))
      (("--resources" "-R")
        ;; we'll eventually want a way to specify a different (or no) compiled file location
        (push (make-list 2 :initial-element
                (e.extern:pathname-to-file
                  (merge-pathnames (native-pathname (pop args)))))
              e.knot:*emaker-search-list*))
      (otherwise
        (let ((tl (assoc (first args) *toplevels* :test #'equal)))
          (when tl
            (pop args)
            (setf toplevel (second tl))))
        (loop-finish)))))
  
  (when do-usage
    (format *query-io* "~
Lisp-level options:
  --help                  
      Print this help.
  --parse-cache|-p <file> 
      Use <file> to cache data that otherwise requires starting a Java
      process.
  --break-on-signals|--bos <type>
      Bind common-lisp:*break-on-signals* to the given value.
  --break-on-ejections|--boe <type>
      Bind e.elib:*break-on-ejections* to the given value.
  --resources|-R <dir>
      Add a directory to search for emakers and resource files.
~{~/e.rune::document-toplevels/~}
  
  If no action-selecting option is given, the E rune() function is
  called with the remaining arguments.
  
  Now attempting to start rune to give further usage help...

"
      *toplevels*)
    (push "--help" args))
  
  (when parse-cache-name
    ; the setting of *parse-cache-name* is deferred so that global-exit will not save a parse cache not including the current contents of the file
    (e.syntax:load-parse-cache-file parse-cache-name)
    (setf *parse-cache-name* parse-cache-name))

  (unless *vat*
    (establish-vat :label "initial"))
  
  (funcall toplevel args))

;; --- REPL tools ---

(defpackage :e.user
  (:nicknames :e-user)
  (:use :cl :e.util :e.elib :e.elib.tables :e.knot :e.kernel :e.elang.node-impl :e.elang :e.elang.syntax :e.elang.compiler :e.extern :e.rune))

(defun read-e-literal (stream character arg)
  (declare (ignore character))
  (check-type arg null)
  (e.syntax:parse-to-kernel (read stream t nil t)))

(defglobal +e-readtable+
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\E 'read-e-literal rt)
    rt))

(defun %in-e-user ()
  (setf *package* (find-package :e.user)
        *readtable* +e-readtable+)
  (values))

(defmacro in-e-user ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%in-e-user)))

