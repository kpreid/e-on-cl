; Copyright 2005 Kevin Reid, under the terms of the MIT X license
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
                 (format *trace-output* "~&; XXX profiling: ~A shortened ~A times ~%" key times))))
              (sort entries #'< :key #'second)))

  #+sbcl
    (sb-ext:quit :unix-status status)
  #+ccl
    (ccl:quit status)
  #+clisp
    (ext:quit status)
  #+cmu  ; XXX have exit status warning
    (extensions:quit)
  #+abcl
    (progn
      (unless (zerop status)
        (warn "discarding nonzero exit status ~A due to ABCL limitations" status))
      (ext:quit))
  (error "Don't know how to global-exit"))

(defun script-toplevel (args)
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

(defun repl-toplevel (args)
  (assert (null args))
  (generic-toplevel "rune repl toplevel"
                    (system-symbol "REPL-START" "E.UPDOC" :e-on-cl.updoc)))

(defun selftest-toplevel (args)
  (assert (zerop (length args)))
  (time (asdf:operate 'asdf:test-op +the-asdf-system+))
  (force-output)
  (global-exit 0))

(defun lisptest-toplevel (args)
  (assert (zerop (length args)))
  (funcall (system-symbol "SYSTEM-TEST" :e.lisp-test :e-on-cl.lisp-test) nil +the-asdf-system+)
  (force-output)
  (global-exit 0))

(defun updoc-toplevel (args)
  (establish-vat :label "rune updoc toplevel")
  (apply (system-symbol "UPDOC-RUNE-ENTRY" "E.UPDOC" :e-on-cl.updoc) args)
  (force-output)
  (global-exit 0))

(defun irc-repl-toplevel (args)
  (establish-vat :label "rune irc toplevel")
  (apply (system-symbol "START-IRC-REPL" :e.irc-repl :e-on-cl.irc-repl) args)
  (top-loop))

(defun translate-toplevel (args)
  (assert (= 1 (length args)))
  (establish-vat :label "rune --translate toplevel")
  (print (e.elang:get-translation (e.syntax:e-source-to-tree (first args))))
  (fresh-line)
  (force-output)
  (global-exit 0))

(defun nothing-toplevel (args)
  (declare (ignore args))
  (values))

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
        (setf parse-cache-name (pop args)))
      (("--bos" "--break-on-signals")
        (setf *break-on-signals* (read-from-string (pop args))))
      (("--boe" "--break-on-ejections")
        (setf *break-on-ejections* (read-from-string (pop args))))
      (("--resources" "-R")
        (push (e.extern:pathname-to-file (merge-pathnames (pop args)))
              e.knot:*emaker-search-list*))
      (("--selftest")
        (setf toplevel #'selftest-toplevel))
      (("--lisptest")
        (setf toplevel #'lisptest-toplevel))
      (("--updoc")
        (setf toplevel #'updoc-toplevel))
      (("--translate")
        (setf toplevel #'translate-toplevel))
      (("--lrepl")
        (setf toplevel #'repl-toplevel))
      (("--irc")
        (setf toplevel #'irc-repl-toplevel))
      (("--nothing")
        (setf toplevel #'nothing-toplevel))
      (otherwise 
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
  --selftest
      Action-selecting option:
      Run the tests for E-on-CL.
  --updoc
      Action-selecting option:
      Invoke the Lisp-implemented Updoc implementation, interpreting the
      arguments as Updoc file pathnames.
  --translate
      Action-selecting option:
      Parse the sole argument as E source and print the Common Lisp form
      it is compiled into.
  --lrepl
      Action-selecting option:
      Enter the Lisp-implemented E REPL.
  --irc
      Action-selecting option:
      Start an IRC bot. The arguments are: <nick> <server> <channel>*
  --nothing
      Action-selecting option:
      Do nothing. In particular, do not exit, and do not run the vat.
      This usually results in a Lisp REPL.
  
  If no action-selecting option is given, the E rune() function is
  called with the remaining arguments.
  
  Now attempting to start rune to give further usage help...

")
    (push "--help" args))
  
  (when parse-cache-name
    ; the setting of *parse-cache-name* is deferred so that global-exit will not save a parse cache not including the current contents of the file
    (e.syntax:load-parse-cache-file parse-cache-name)
    (setf *parse-cache-name* parse-cache-name))

  (funcall toplevel args))
