; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.updoc
  (:nicknames)
  (:use :cl :e.util :e.knot :e.elib)
  (:export
    :updoc-rune-entry
    :updoc-start
    :system-test
    
    :result :result+
    :result-failure-count :result-step-count))
(cl:in-package :e.updoc)

; --- Updoc parsing ---

(defun read-updoc-line (stream &aux line)
  (setf line (read-line stream nil nil))
  (unless line
    (return-from read-updoc-line nil))
  (setf line (string-left-trim '(#\Space #\Tab) line))
  (if (and (string/= line "") (member (aref line 0) '(#\? #\> #\#)))
    (list
      (cond
        ((= (length line) 1)         "")
        ((eql (aref line 1) #\Space) (subseq line 2))
        (t                           (subseq line 1)))
      (aref line 0)
      line)
    (list
      ""
      nil
      line)))

(defun read-updoc (stream &aux script expression answers answer-key answer-value)
  (labels ((finish-answer ()
             (when answer-key
               (push (list answer-key answer-value) answers)
               (setf answer-key nil)
               (setf answer-value nil)))
           (finish-testcase ()
             (finish-answer)
             (when expression
               (push (list expression (reverse answers)) script)
               (setf expression nil)
               (setf answers nil))))
    (loop
      for (line type-char) = (read-updoc-line stream)
      while line
      do ; (print (list type-char line answer-key answer-value expression script))
         (ecase type-char
           ((#\?)
             (finish-testcase)
             (setf expression line))
           ((#\>)
             (setf expression (concatenate 'string expression '(#\Newline) line)))
           ((#\#)
             ; read # lines iff we saw a ? line
             (when expression
               (if answer-value
                 (setf answer-value
                   (concatenate 'string
                     answer-value
                     '(#\Newline)
                     (subseq line (min (+ (length answer-key) 2)
                                       (length line)))))
                 (let ((point (position #\: line)))
                   (when point
                     (let ((keyword (subseq line 0 point))
                           (value (subseq line (+ 2 point))))
                       (setf answer-value value)
                       (setf answer-key keyword)))))))
           ((nil)
             (finish-answer))))
    (finish-testcase)
    (nreverse script)))

; --- Utilities ---

(defun make-problem-answer (condition)
  ; xxx sloppy
  "problem: foo -> (list \"problem\" \"foo\")"
  (let* ((s (e. +the-e+ |toQuote| (e-problem-unseal condition)))
         (colon (position #\: s)))
    (if colon
      (list (subseq s 0 colon) (subseq s (+ 2 colon) (length s)))
      (list "problem" s))))

(defun chain (combiner accumulator test getter finalizer)
  "Accumulate the eventual results of calling 'getter' into 'accumulator' using 'combiner' until 'test' returns false, then call 'finalizer' with the final value and return it."
  (multiple-value-bind (result-promise result-resolver) (make-promise)
    (labels ((proceed ()
              (handler-case-with-backtrace
                (if (funcall test)
                  (let ((element-vow (funcall getter)))
                    (call-when-resolved element-vow (efun (x)
                      (declare (ignore x))
                      (setf accumulator (funcall combiner accumulator element-vow))
                      (proceed))))
                  (progn
                    (e. result-resolver |resolve| accumulator)
                    (funcall finalizer accumulator)))
                (error (condition backtrace)
                  (let ((*print-level* 5))
                    (efuncall e.knot:+sys-trace+ (format nil "caught problem in updoc chaining: ~A~%~S" (e-print condition) backtrace)))
                  (e. result-resolver |smash| (transform-condition-for-e-catch condition :backtrace backtrace))))))
      (proceed))
    result-promise))

; --- XXX think of a name for this section ---

(defclass result ()
  ((failure-count :type integer
                  :initarg :failures
                  :initform 0
                  :reader result-failure-count)
   (step-count :type integer
               :initarg :steps
               :initform 0
               :reader result-step-count)
   (dead :type boolean
         :initarg :dead
         :initform nil
         :reader result-dead)))

(defgeneric result+ (a b))

(defmethod result+ ((a result) (b result))
  (make-instance 'result
    :failures (+ (result-failure-count a) (result-failure-count b))
    :steps    (+ (result-step-count a)    (result-step-count b))
    :dead     (or (result-dead a) (result-dead b))))

(def-shorten-methods result+ 2)
(def-shorten-methods result-failure-count 1)
(def-shorten-methods result-step-count 1)
(def-shorten-methods result-dead 1)

; --- Script running ---

(defun print-answer (answer)
  (format t "~&# ~A: ~A~%" (first answer) (second answer)))

(defun make-updoc-handler (&key file out err print-steps
                                (dead-names +empty-const-map+))
  ;; XXX too much state - ick
  (let ((new-answers nil)
        (step nil)
        (step-scope nil)
        (props nil)
        (backtrace nil)
        (skipping nil)
        (starting-time nil))
    
    (e-lambda |updocHandler| ()
      (:|begin| (nstep)
        (setf nstep (ref-shorten nstep))
        (destructuring-bind (expr answers) nstep
          (declare (ignore answers))
          (setf step nstep)
          (setf new-answers nil
                skipping nil
                backtrace nil)
          
          ;; must happen after vars are set, in case of syntax errors
          (multiple-value-bind (node new-props)
              (e.syntax:parse-to-kernel expr :props props)
            (setf props new-props)
            (setf step-scope (e. node |staticScope|))
            (setf skipping (e-is-true (e. (e. (e. dead-names |and| (e. step-scope |namesUsed|)) |size|) |aboveZero|)))
            #+#:debug (print (e. dead-names |getKeys|))
            (unwind-protect
              (if skipping
                (progn
                  (princ "x")
                  (load-time-value (make-instance 'e.kernel:|LiteralExpr| :elements '(0))))
                (progn
                  (if print-steps
                    (format t "~&? ~A~%" expr)
                    (princ "."))
                  node))
              (progn
                (force-output)
                (setf starting-time (get-internal-run-time)))))))
      (:|takeStreams| ()
        (loop for stream in (list out err)
              for label in '("stdout" "stderr")
              for string = (get-output-stream-string stream)
              unless (string= string "")
                do (e. |updocHandler| |answer| (list label string))))
      (:|answer| (answer)
        (push (ref-shorten answer) new-answers)
        (when print-steps
          (print-answer answer)))
      (:|backtrace| (bt)
        (setf backtrace (ref-shorten bt)))
      (:|finish| ()
        (when print-steps
          (format t "~&step in ~Ss~%"
            (when starting-time
              (float (/ (- (get-internal-run-time) starting-time)
                        internal-time-units-per-second)))))
        (nreverse-here new-answers)
        (flet ((adjust-liveness (live)
                 "updates whether names set by this step are guessed to not
                  match the expectations of the updoc script"
                 #+#:debug (print `(adjusting-liveness ,live ,(e. (e. step-scope |outNames|) |getKeys|)))
                 (setf dead-names
                   (e-call dead-names
                           (if live
                             "butNot"
                             "or")
                           (list (e. step-scope |outNames|))))))
          (cond
            (skipping
              (adjust-liveness nil)
              (make-instance 'result :dead t))
            ((tree-equal (second step) new-answers :test #'equal)
              (adjust-liveness t)
              (make-instance 'result :failures 0 :steps 1))
            (t
              (let ((*print-pretty* t)
                    (*package* #.*package*)
                    (*print-case* :downcase)
                    (*print-level* 7)
                    (*print-length* 20))
                (destructuring-bind (expr expected-answers) step
                  (print `(mismatch (file ,file)
                                    (source ,expr)
                                    (expects ,@expected-answers)
                                    (instead ,@new-answers)
                                    (opt-backtrace ,backtrace)))
                  (fresh-line)
                  #| names bound by this step are now dead (assumed to not match
                     future steps' expectations) iff this step had an unexpected
                     problem |#
                  (flet ((has-problem (answers)
                           (member "problem" answers :test #'equal
                                                     :key #'first)))
                    (adjust-liveness (or (not (has-problem new-answers))
                                         (has-problem expected-answers))))
                  (make-instance 'result :failures 1 :steps 1))))))))))

(defun make-stepper (&key props handler)
  (let* ((scope-slot (make-instance 'e-var-slot :value nil))
         (wait-hook-slot (make-instance 'e-var-slot :value "the arbitrary resolved value for the wait hook chain")))
    (symbol-macrolet ((scope (e-slot-value scope-slot))
                      (wait-hook (e-slot-value wait-hook-slot)))
      (values
        (lambda (step)
          (multiple-value-bind (result-promise result-resolver) (make-promise)
            
            ((lambda (f) (e<- f |run|)) (efun (&aux new-result)
              (block attempt
                (handler-bind ((error #'(lambda (condition)
                                          (e. handler |takeStreams|)
                                          (e. handler |answer| (make-problem-answer condition))
                                          (e. handler |backtrace| (backtrace-value))
                                          (return-from attempt)))
                               (warning #'muffle-warning)
                               #+sbcl (sb-ext:compiler-note #'muffle-warning))
                  (setf (values new-result scope)
                        (e.elang:eval-e (e. handler |begin| step) scope))))
              (e. handler |takeStreams|)
              (unless (same-yet-p new-result nil)
                (e. handler |answer| (list "value" (e. +the-e+ |toQuote| new-result))))
              (call-when-resolved wait-hook (efun (x)
                ;; timing constraint: whenResolved queueing happens *after* the turn executes; this ensures that stream effects from sends done by this step are collected into this step's results
                (declare (ignore x))
                (e. handler |takeStreams|)
                (e. result-resolver |resolve| (e. handler |finish|))))))
            result-promise))
        scope-slot
        (e-lambda "org.cubik.cle.updoc.interp" ()
          (:|gc| () (e. e.extern:+gc+ |run|) nil)
          (:|getProps| () props)
          ;; XXX other interp methods
          (:|waitAtTop| (ref &aux (old-wait (e. wait-hook-slot |get|)))
            "Unlike E-on-Java, always returns null, and not the ref it's passed."
            (e. wait-hook-slot |put|
              (call-when-resolved ref
                (efun (ref)
                  (declare (ignore ref))
                  old-wait)))
            nil))))))

(defmacro multiple-value-let* ((&rest bindings) &body body)
  (if bindings
    `(multiple-value-bind ,(butlast (first bindings)) ,(first (last (first bindings)))
       (multiple-value-let* ,(rest bindings) ,@body))
    `(locally ,@body)))

(defun make-capturing-stream (print-steps ordinary-destination)
  (let ((capture (make-string-output-stream)))
    (if print-steps
      (values capture
              (make-broadcast-stream capture ordinary-destination))
      (values capture capture))))

(defun updoc-file (file &key print-steps confine)
  (with-open-file (s file
      :external-format e.extern:+standard-external-format+)
    (multiple-value-let*
          ((script (read-updoc s))
           (capture-out eval-out-stream (make-capturing-stream print-steps *standard-output*))
           (capture-err eval-err-stream (make-capturing-stream print-steps *error-output*))
           (stepper scope-slot base-interp
             (make-stepper :props e.knot::+eprops+ ;; XXX internal
                           :handler (make-updoc-handler :out capture-out
                                                        :err capture-err
                                                        :print-steps print-steps
                                                        :file file)))
           (updoc-interp (e-lambda "org.cubik.cle.updoc.$updocInterp" ()
              (:|__printOn| ((tw +the-text-writer-guard+))
                ; XXX we should eventually be using an e.extern file-cap for this, at which point we shouldn't be printing <file: ourselves
                ; XXX giving all updoc scripts the authority to see their own pathname. This could be considered a problem.
                (e. tw |print| "<updoc loop for <file:" (enough-namestring file) ">>"))
              (:|pushTestStep| (expr answers-vector)
                "Add another test to be executed immediately after the current one. 'expr' should be an E expression node, and 'answers-vector' should be like [[\"value\", \"<the-expected-object>\"]]. XXX this will eventually be changed to enter tests in sequence order when called within a single turn."
                (push (list (e-print expr) ; XXX print insufficient in the general case
                            (map 'list #'(lambda (x) (coerce x 'list))
                                 answers-vector)) script)
                nil)
              (otherwise (mverb &rest args)
                (apply #'e-call-dispatch base-interp mverb args))))
           (scope (if confine
                    (efuncall (e-import "org.cubik.cle.makeIOScope")
                      "__main$"
                      (vat-safe-scope *vat*)
                      (make-scope "__updocConfinedPowers$"
                        `(("interp" ,updoc-interp)
                          ("stdout" ,(make-text-writer-to-cl-stream
                                      eval-out-stream
                                      :autoflush t
                                      :should-close-underlying nil))
                          ("stderr" ,(make-text-writer-to-cl-stream
                                      eval-err-stream
                                      :autoflush t
                                      :should-close-underlying nil))
                          ("props"  ,e.knot::+eprops+)
                          #||#)))
                    (e. (make-io-scope :stdout eval-out-stream
                                       :stderr eval-err-stream
                                       :interp updoc-interp)
                        |withPrefix| "__main$")))
           ; XXX option to run updoc scripts in unprivileged-except-for-print scope
           )
      (e. scope-slot |put| scope)
      (format t "~&~A" (enough-namestring file))
      (let ((starting-time (get-internal-run-time)))
        (chain #'result+
               (make-instance 'result)
               (lambda () script)
               (lambda () (funcall stepper (pop script)))
               (lambda (result)
                 (format t " ~A in ~Gs~%"
                         (result-step-count result)
                         (/ (- (get-internal-run-time) starting-time)
                            internal-time-units-per-second))))))))

; --- Profiling ---

(defgeneric profile-start (profiler)
  (:method ((profiler null))))
(defgeneric profile-finish (profiler)
  (:method ((profiler null))))

(defparameter *profilers*
  '(nil
    #+sbcl :sb-profile
    #+sbcl :sb-sprof
    :ecall-counter))

(defparameter *profile-package-names*
  '("E.UTIL"
    "E.ELIB"
    "E.KNOT"
    "E.ELANG"
    "E.ELANG.SYNTAX"
    "E.EXTERN"
    "E.KERNEL"
    "E.RUNE"
    "E.UPDOC"))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

#+sbcl
(defmethod profile-start ((profiler (eql :sb-profile)))
  ; profile is a macro, so I can't use mapcar
  (loop for x in *profile-package-names* do (sb-profile:profile x))
  (sb-profile:reset))

#+sbcl
(defmethod profile-finish ((profiler (eql :sb-profile)))
  (sb-profile:unprofile)
  (sb-profile:report))

#+sbcl
(defmethod profile-start ((profiler (eql :sb-sprof)))
  (sb-sprof:start-profiling))

#+sbcl
(defmethod profile-finish ((profiler (eql :sb-sprof)))
  (sb-sprof:stop-profiling)
  (sb-sprof:report))


(defvar *call-counter-method*)
(defvar *call-counts*)

(defun cc-type-of (specimen)
  (typecase specimen
    (function
      (let ((name (nth-value 2 (function-lambda-expression specimen))))
        (typecase name
          ((cons (eql labels) (cons symbol null)) (second name))
          (otherwise name))))
    (otherwise
      ;; type-of turned out to be too fine-grained; e.g. array dimensions
      (class-name (class-of specimen)))))

(defmethod profile-start ((profiler (eql :ecall-counter)))
  "Note that this makes a global change, namely installing a :before method on e-call-dispatch."
  (setf *call-counts* (make-hash-table :test #'equal)
        *call-counter-method*
          (defmethod e-call-dispatch :before (rec mverb &rest args)
            (incf (gethash (list* (cc-type-of rec)
                                  mverb
                                  (mapcar #'cc-type-of args))
                           *call-counts*
                           0)))))

(defmethod profile-finish ((profiler (eql :ecall-counter)))
  (remove-method #'e-call-dispatch *call-counter-method*)
  (format *trace-output* "~&--- Call count profile ---~%")
  (loop for (name . count) in (sort (map-from-hash 'list #'cons *call-counts*)
                                  #'< :key #'cdr)
        do (format *trace-output* "~8D ~S~%" count name))
  (format *trace-output* "~&--------------------------~%")
  (force-output *trace-output*))


; --- Entry points, etc. ---

(defun updoc-start (paths &key profiler print-steps confine
  &aux file-paths)
  
  (flet ((collect (pathname)
          (push pathname file-paths)
          (values)))
    
    ; XXX use e.extern routines for file access
    (loop for path in paths do
      (if #-clisp (cl-fad:directory-exists-p path)
          ; otherwise "UNIX error 20 (ENOTDIR): Not a directory"
          #+clisp (ignore-errors (cl-fad:directory-exists-p path))
        (cl-fad:walk-directory path #'collect
          :test (lambda (path)
                  (and (member (pathname-type path)
                         '("updoc" "emaker" "e" "e-awt" "e-swt" "caplet" "txt")
                         :test #'string-equal)
                       (not (without-prefix (pathname-name path) ".")))))
        (collect path)))
    
    (nreverse-here file-paths)
    
    (profile-start profiler)
    (chain
      #'result+
      (make-instance 'result)
      (lambda () file-paths)
      (lambda () (updoc-file (pop file-paths) :print-steps print-steps :confine confine))
      (lambda (result)
        (profile-finish profiler)
        (format t "~A~%" (describe-result result))
        (force-output)
        (values)))))

(defun describe-result (result)
  (format nil "~&~[~D test~:P passed.~:;~:*~D failure~:P in ~D test~:P.~]"
    (result-failure-count result)
    (result-step-count result)))

(defun updoc-rune-entry (&rest args
    &aux profiler print-steps confine)
  (loop while args do
    (popping-equal-case args
      (("--profile")
        (assert args (args) "--profile requires an argument")
        (setf profiler
          (let ((*package* (find-package :keyword)))
            (read-from-string (pop args))))
        (assert (member profiler *profilers*) (profiler) "~S is not a known profiler." profiler))
      (("--steps" "--print-steps")
        (setf print-steps t))
      (("--confine")
        ;; XXX should probably be default
        (setf confine t))
      (otherwise
        (loop-finish))))
  
  (call-when-resolved
      (updoc-start (mapcar #'native-pathname args) :profiler profiler :print-steps print-steps :confine confine)
    (efun (result)
      (if (plusp (result-failure-count result))
        (make-unconnected-ref (e-coerce (describe-result result) +the-exception-guard+))
        nil))))

(defun system-test (op system)
  "Invoked by an implementation of asdf:test-op."
  (declare (ignore op))
  (e.syntax:with-parse-cache-file
      ((merge-pathnames
        (make-pathname :name (format nil "test-parse-cache-~A" (lisp-implementation-type)) :type "sexp")
        (asdf:component-pathname system)))
    (let ((result
            (block test
              (with-vat (:label "updoc system-test")
                (call-when-resolved
                    (updoc-start
                      (list (merge-pathnames
                              (make-pathname :directory '(:relative "tests"))
                              (asdf:component-pathname system))))
                  (efun (result)
                    (return-from test result)))))))
      (when (eql (ref-state result) 'broken)
        (cerror "Ignore failures." "Tests for ~A did not execute properly: ~A" system (ref-opt-problem result)))
      (when (plusp (result-failure-count result))
        (cerror "Ignore failures." "Tests for ~A failed." system)))))

;;; --- Updoc-guts-based REPL ---

(defun make-repl-handler ()
  (e-lambda "$replHandler" ()
    (:|begin| (step)
      (e.syntax:parse-to-kernel step))
    (:|takeStreams| () nil)
    (:|answer/1| #'print-answer)
    (:|backtrace| (backtrace) (write backtrace :readably nil :escape t :circle t :pretty t))
    (:|finish| () nil)
    (:|answer| () nil)))

(defun repl-start ()
  (multiple-value-let*
      ((stepper scope-slot interp (make-stepper :handler (make-repl-handler)))
       (scope (e. (make-io-scope :stdout *standard-output*
                                 :stderr *error-output*
                                 :interp interp)
                  |withPrefix| "__main$"))
       (split-in (e-import "org.cubik.cle.io.splitIn"))
       (lines (efuncall split-in
                        (string #\Newline)
                        (e. split-in |strictTerminatorPolicy|)
                        (eelt scope "stdin")))
       (eio (e-import "org.erights.e.elib.eio.EIO")))
    (e. scope-slot |put| scope)
    (labels ((take ()
               (format t "~&e-on-cl? ")
               (force-output)
               (when-resolved (chunk) (e. eio |takeRange| 1 1 lines)
                 (setf chunk (ref-shorten chunk))
                 (if (or (null chunk)
                         (eql (ref-state chunk) 'broken))
                   chunk
                   (when-resolved (sr) (funcall stepper (eelt chunk 0))
                     (declare (ignore sr))
                     (take))))))
      (take))))
