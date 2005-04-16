; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.rune)

(defvar *parse-cache-name* nil)

(defun global-exit (status)
  (when *parse-cache-name*
    (e.syntax:save-parse-cache-file *parse-cache-name*))

  (maphash #'(lambda (fqn times)
             (when (> times 1)
               (format *trace-output* "~&; note: ~A loaded ~A times ~%" fqn times)))
           e.knot::*emaker-load-counts*)

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
      (when (/= 0 status)
        (warn "discarding nonzero exit status ~A due to ABCL limitations" status))
      (ext:quit))
  (error "Don't know how to global-exit"))

(defun script-toplevel (args)
  (let* ((scope (make-io-scope :stdout *standard-output* :stderr *error-output*))
         (Ref (e. scope |get| "Ref"))
         (outcome-vow (e. (e. scope |get| "rune") |run| (coerce args 'vector))))
    (e. Ref |whenResolved| outcome-vow
      (e-named-lambda "org.erights.e.elang.interp.rune$terminator"
        (:|run/1| (outcome 
            &aux (opt-problem (e. Ref |optProblem| outcome)))
          (if opt-problem
            (progn
              (format *error-output* "# problem: ~A" (e-quote opt-problem)) ; xxx no indent on multiple lines
              (global-exit -1))
            (global-exit 0)))))
    (vat-loop)))

(defun repl-toplevel (args)
  (assert (null args))
  (let ((scope (make-io-scope :stdout *standard-output* :stderr *error-output*)))
    (loop (with-simple-restart (abort "Return to E REPL.")
      (princ "cl-e? ")
      (force-output)
      (let ((source (e.syntax:e-source-to-tree 
                      (or (read-line *standard-input* nil)
                          (progn
                            (fresh-line)
                            (force-output)
                            (global-exit 0))))))
        (multiple-value-bind (result new-scope)
            (time (elang:eval-e source scope))
          (format t "~&# value: ~A~%~%" (elib:e-quote result))
          (elib:run-vats)
          (setf scope new-scope)))))))

(defun updoc-toplevel (args)
  (asdf:operate 'asdf:load-op :cl-e.updoc)
  (apply (intern "UPDOC-RUNE-ENTRY" "E.UPDOC") args)
  (force-output)
  (global-exit 0))

(defun irc-repl-toplevel (args)
  (asdf:operate 'asdf:load-op :cl-e.irc-repl)
  (apply (intern "START-IRC-REPL" :e.irc-repl) args)
  (vat-loop))

(defun translate-toplevel (args)
  (assert (= 1 (length args)))
  (print (e.elang:get-translation (e.syntax:e-source-to-tree (first args))))
  (fresh-line)
  (force-output)
  (global-exit 0))

(defmacro popping-equal-case (list-sym &rest cases &aux (arg-sym (gensym)))
  `(let ((,arg-sym (first ,list-sym)))
    (cond
      ,@(loop for (keys . body) in cases collect
        (if (eql keys 'otherwise)
          `(t 
            ,@body)
          `((member ,arg-sym ',(if (atom keys) (list keys) keys) :test #'equal)
            (pop ,list-sym)
            ,@body))))))

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
      (("--updoc")
        (setf toplevel #'updoc-toplevel))
      (("--translate")
        (setf toplevel #'translate-toplevel))
      (("--lrepl")
        (setf toplevel #'repl-toplevel))
      (("--irc")
        (setf toplevel #'irc-repl-toplevel))
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
  
  If no action-selecting option is given, the E rune() function is
  called with the remaining arguments.
  
  Now attempting to start rune to give further usage help...

")
    (push "--help" args))
  
  (when parse-cache-name
    ; the setting of *parse-cache-name* is deferred so that global-exit will not save a parse cache not including the current contents of the file
    (e.syntax:load-parse-cache-file parse-cache-name)
    (setf *parse-cache-name* parse-cache-name))

  (establish-vat)
  (funcall toplevel args))
