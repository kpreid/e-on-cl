; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.rune)

(defvar *parse-cache-name* nil)

(defun global-exit (status)
  (check-type status (unsigned-byte 8))
  (when *parse-cache-name*
    (e.syntax:save-parse-cache-file *parse-cache-name*))

  (maphash #'(lambda (fqn times)
             (when (> times 1)
               (warn "note: ~A loaded ~A times" fqn times)))
           (e. e.knot::+default-fresh-emaker-loader+
               |_getLoadCounts|))
  
  #+e.instrument.ref-shorten-uses
    (let ((entries (maphash #'list e.elib::*instrument-ref-shorten-kinds*)))
      (mapcar #'(lambda (l) (destructuring-bind (key times) l
               (when (> times 0)
                 (e. e.knot:+sys-trace+ |run| (format nil "profiling: ~A shortened ~A times" key times)))))
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
  (establish-vat :label "rune script toplevel")
  (let* ((scope (make-io-scope :stdout *standard-output* :stderr *error-output*))
         (Ref (e. scope |get| "Ref"))
         (outcome-vow (e. (e. scope |get| "rune") |run| (coerce args 'vector))))
    (e. Ref |whenResolved| outcome-vow
      (e-lambda "org.erights.e.elang.interp.RuneTerminator" ()
        (:|run| (outcome 
            &aux (opt-problem (e. Ref |optProblem| outcome)))
          (if opt-problem
            (progn
              (format *error-output* "# problem: ~A" (e-quote (e-problem-unseal opt-problem))) ; xxx no indent on multiple lines
              (global-exit -1))
            (global-exit 0)))))
    (top-loop)))

(defun generic-toplevel (label starter)
  (establish-vat :label label)
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
  (time (asdf:operate 'asdf:test-op +the-asdf-system+))
  (force-output)
  (global-exit 0))

(define-toplevel lisptest-toplevel (args)
  "Run those tests for E-on-CL which do not involve executing the E language."
  (assert (zerop (length args)))
  (funcall (system-symbol "SYSTEM-TEST" :e.lisp-test :e-on-cl.lisp-test) nil +the-asdf-system+)
  (force-output)
  (global-exit 0))

(define-toplevel updoc-toplevel (args)
  "Invoke the Lisp-implemented Updoc implementation, interpreting the
      arguments as Updoc file pathnames."
  (establish-vat :label "rune updoc toplevel")
  (apply (system-symbol "UPDOC-RUNE-ENTRY" "E.UPDOC" :e-on-cl.updoc) args)
  (force-output)
  (global-exit 0))

(define-toplevel irc-repl-toplevel (args)
  "Start an IRC bot. The arguments are: <nick> <server> <channel>*"
  (establish-vat :label "rune irc toplevel")
  (apply (system-symbol "START-IRC-REPL" :e.irc-repl :e-on-cl.irc-repl) args)
  (top-loop))

(define-toplevel translate-toplevel (args)
  "Parse the sole argument as E source and print the Common Lisp form
      it is compiled into."
  (assert (= 1 (length args)))
  (establish-vat :label "rune --translate toplevel")
  (print (e.elang:get-translation (e.syntax:e-source-to-tree (first args))))
  (fresh-line)
  (force-output)
  (global-exit 0))

(define-toplevel nothing-toplevel (args)
  "Do nothing. In particular, do not exit, and do not run the vat.
      This usually results in a Lisp REPL."
  (declare (ignore args))
  (values))

(define-toplevel save-toplevel (args)
  "Generate an image file of the Lisp implementation with E-on-CL loaded.
      If supported, it will be executable. Under CLISP, the first argument to
      the resulting executable must be \"--\" for correct argument processing.
      The sole argument is the name of the image file to create."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (assert (= 1 (length args))) ;; will be options later, regarding how much to include
  (let ((image-file (native-pathname (first args)))
        (executable-p t)
        #| XXX todo: (may-enter-debugger t) |#)
    
    (save-flush)
    
    #.(or
        #+sbcl
        (quote
          (sb-ext:save-lisp-and-die
            image-file
            :executable executable-p
            :toplevel #'%revive-start))
        #+cmucl
        (ext:save-lisp 
          image-file 
          :purify t
          :init-function #'%revive-start
          :load-init-file nil
          :site-init nil
          :batch-mode nil
          :print-herald nil
          :process-command-line nil
          :executable executable-p)
        #+openmcl
        (quote
          ;; reference: http://www.clozure.com/pipermail/openmcl-devel/2003-May/001062.html
          (ccl:save-application
            image-file
            :toplevel-function #'%revive-start
            :prepend-kernel executable-p))
        #+clisp
        (quote
          (ext:saveinitmem
            image-file
            :quiet t
            ; :norc t ; XXX is this appropriate?
            :init-function #'%revive-start
            :start-package (find-package :cl-user)
            :keep-global-handlers nil ; XXX is this appropriate?
            :executable executable-p))
        '(error "Saving an executable/image is not supported for ~A." (lisp-implementation-type)))
        
    ;; depending on the implementation (e.g. sbcl's save-lisp-and-die) we may
    ;; or may not reach here
    (global-exit 0)))

(defun save-flush ()
  (assert (null *vat*))

  (setf *parse-cache-name* nil)
  
  ;; make sure we've loaded everything lispy, in order to simplify the
  ;; stuff-changing-out-from-under-our-image problem
  (asdf:operate 'asdf:load-op :e-on-cl.updoc)
  ;(asdf:operate 'asdf:load-op :e-on-cl.sockets)
  #|XXX review what should go here|#)

(defun revive-flush ()
  #|XXX review what should go here|#)

(defun %revive-start ()
  (revive-flush)
  (apply #'rune (or #+sbcl (rest sb-ext:*posix-argv*)
                    #+clisp ext:*args*
                    #+openmcl (rest ccl::*command-line-argument-list*)
                    #+cmucl (rest ext:*command-line-strings*)))
  (warn "Fell out of ~S; exiting." 'rune)
  ;; XXX have an overall exit status policy
  (global-exit 255))

(defparameter *toplevels* '(("--updoc"     updoc-toplevel)
                            ("--selftest"  selftest-toplevel)
                            ("--lisptest"  lisptest-toplevel)
                            ("--translate" translate-toplevel)
                            ("--lrepl"     repl-toplevel)
                            ("--irc"       irc-repl-toplevel)
                            ("--nothing"   nothing-toplevel)
                            ("--save"      save-toplevel)))

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
        (push (e.extern:pathname-to-file (merge-pathnames (native-pathname (pop args))))
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

  (funcall toplevel args))
