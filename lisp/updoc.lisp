; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.updoc
  (:nicknames)
  (:use :cl :e.knot :e.elib)
  (:export
    :updoc-rune-entry
    :updoc-start
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

(defun slot-place (slot) ; XXX should be an elib macro util
  (e. slot |getValue|))

(defun (setf slot-place) (new slot)
  (e. slot |setValue| new))

(defun chain (combiner accumulator test getter finalizer
    &aux (ref-kit (e. (vat-safe-scope *vat*) |get| "Ref")))
  "Accumulate the eventual results of calling 'getter' into 'accumulator' using 'combiner' until 'test' returns false, then call 'finalizer' with the final value and return it."
  (multiple-value-bind (result-promise result-resolver) (make-promise)
    (labels ((proceed ()
              (handler-case
                (if (funcall test)
                  (let ((element-vow (funcall getter)))
                    (e. ref-kit |whenResolved| element-vow (efun (x)
                      (declare (ignore x))
                      (setf accumulator (funcall combiner accumulator element-vow))
                      (proceed))))
                  (progn
                    (e. result-resolver |resolve| accumulator)
                    (funcall finalizer accumulator)))
                (error (condition)
                  (format *trace-output* "~&; caught problem in updoc chaining: ~A~%~S" (e-print condition) (e.util:backtrace-value))
                  (e. result-resolver |smash| (transform-condition-for-e-catch condition))))))
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
               :reader result-step-count)))

(defgeneric result+ (a b))

(defmethod result+ ((a result) (b result))
  (make-instance 'result 
    :failures (+ (result-failure-count a) (result-failure-count b))
    :steps    (+ (result-step-count a)    (result-step-count b))))

;;; XXX another one for the elib macro collection
(defmacro define-shorten-methods (gf-name arity)
  "Define methods on the generic function 'gf-name' such that if any of its arguments is an E ref, it will be re-callled with the ref shortened. If the ref is  not NEAR, an error will be signaled."
  `(progn ,@
    (loop for i below arity collect
      (loop
        for j below arity
        for shortening = (= i j)
        for sym = (gensym)
        collect `(,sym ,(if shortening 'e.elib::ref 't)) into params
        collect (if shortening 
                  `(let ((v (ref-shorten ,sym)))
                    (assert (typep v '(not e.elib::ref)) (v)
                            "Argument ~A to ~S must be near." ',j ',gf-name)
                    v)
                  sym) into args
        finally (return
          `(defmethod ,gf-name ,params
            (funcall ',gf-name ,@args)))))))

(define-shorten-methods result+ 2)

; --- Script running ---

(defun make-stepper (file scope-slot wait-hook-slot eval-out-stream eval-err-stream)
  (symbol-macrolet ((scope (slot-place scope-slot))
                    (wait-hook (slot-place wait-hook-slot)))
    (lambda (step
        &aux new-answers new-result backtrace)
      (destructuring-bind (expr answers) step
        (multiple-value-bind (result-promise result-resolver) (make-promise)
          (princ ".")
          (force-output)
          (setf new-answers nil)
          (labels ((collect-streams ()
                    (let ((s (get-output-stream-string eval-out-stream)))
                      (unless (string= s "") (push (list "stdout" s) new-answers)))
                    (let ((s (get-output-stream-string eval-err-stream)))
                      (unless (string= s "") (push (list "stderr" s) new-answers))))
                   (finish-step ()
                    (setf new-answers (nreverse new-answers))
                    (if (not (tree-equal answers new-answers :test #'equal))
                      (let ((*print-pretty* t)
                            (*package* #.*package*)
                            (*print-case* :downcase)
                            (*print-level* 6)
                            (*print-length* 20))
                        (print `(mismatch (file ,file)
                                          (source ,expr)
                                          (expects ,@answers)
                                          (instead ,@new-answers)
                                          (opt-backtrace ,backtrace)))
                        (fresh-line)
                        (make-instance 'result :failures 1 :steps 1))
                      (make-instance 'result :failures 0 :steps 1))))
            ((lambda (f) (e<- f |run|)) (efun ()
              (block attempt
                (handler-bind ((error #'(lambda (condition) 
                                          (setf new-result nil)
                                          (collect-streams)
                                          (push (make-problem-answer condition) new-answers)
                                          (setf backtrace (e.util:backtrace-value))
                                          (return-from attempt)))
                               (warning #'muffle-warning)
                               #+sbcl (sb-ext:compiler-note #'muffle-warning))
                  (setf (values new-result scope)
                          (elang:eval-e (e.syntax:e-source-to-tree expr) scope))))
              (collect-streams)
              (if new-result
                (push (list "value" (e. +the-e+ |toQuote| new-result)) new-answers))
              (e. (e. scope |get| "Ref") |whenResolved| wait-hook (efun (x)
                ;; timing constraint: whenResolved queueing happens *after* the turn executes; this ensures that stream effects from sends done by this step are collected into this step's results
                (declare (ignore x))
                (collect-streams)
                (e. result-resolver |resolve| (finish-step)))))))
          result-promise)))))

(defun updoc-file (file)
  (with-open-file (s file
      :external-format e.extern:+standard-external-format+)
    (let* ((script (read-updoc s))
           (eval-out-stream (make-string-output-stream))
           (eval-err-stream (make-string-output-stream))
           (scope-slot (make-instance 'elib:e-var-slot :value nil))
           (wait-hook-slot (make-instance 'elib:e-var-slot :value "the arbitrary resolved value for the wait hook chain"))
           (stepper (make-stepper file scope-slot wait-hook-slot eval-out-stream eval-err-stream))
           (runner (e-lambda "org.cubik.cle.updoc.Runner" ()
              (:|__printOn| (tw)
                (e-coercef tw +the-text-writer-guard+)
                ; XXX we should eventually be using an e.extern file-cap for this, at which point we shouldn't be printing <file: ourselves
                ; XXX giving all updoc scripts the authority to see their own pathname. This could be considered a problem when we implement confined updoc.
                (e. tw |print| "<updoc runner for <file:" (enough-namestring file) ">>"))
              (:|push| (expr answers-vector)
                "Add another test to be executed immediately after the current one. 'expr' should be an E expression node, and 'answers-vector' should be like [[\"value\", \"<the-expected-object>\"]]. XXX this will eventually be changed to enter tests in sequence order when called within a single turn."
                (push (list (e-print expr) ; XXX quote insufficient in the general case
                            (map 'list #'(lambda (x) (coerce x 'list))
                                 answers-vector)) script)
                nil)))
           (interp (e-lambda "org.cubik.cle.updocInterp" ()
             ; XXX this is a hodgepodge of issues we don't care about, just existing because we need to define waitAtTop.
             (:|gc| () (e. e.extern:+gc+ |run|))
             (:|getProps| ()
               ; XXX internal symbol
               e.knot::+eprops+)
             (:|waitAtTop| (ref &aux (old-wait (e. wait-hook-slot |getValue|)))
               (e. wait-hook-slot |setValue|
                 (e. (e. (vat-safe-scope *vat*) |get| "Ref") |whenResolved| ref
                   (efun (ref)
                     (declare (ignore ref))
                     old-wait)))
               nil)))
           (scope (e. (e. (make-io-scope :stdout eval-out-stream 
                                         :stderr eval-err-stream
                                         :interp interp) 
                          |withPrefix| "__main$")
                      |with| "updoc" runner))
           ; XXX option to run updoc scripts in unprivileged-except-for-print scope
           )
      (e. scope-slot |setValue| scope)
      (format t "~&~A" (enough-namestring file))
      (chain #'result+
             (make-instance 'result)
             (lambda () script)
             (lambda () (funcall stepper (pop script)))
             (lambda (result) (format t " ~A~%" (result-step-count result)))))))

; --- Entry points, etc. ---

; XXX clean up the profile setup code; add command line options for profiling
#+sbcl (defvar *use-sprof* nil)
#+sbcl (defvar *use-profile* nil)
#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require :sb-sprof))

(defun profile-start ()
  #+sbcl (when *use-sprof* (sb-sprof:start-profiling))
  #+sbcl (when *use-profile* 
    (sb-profile:profile "E.UTIL")
    (sb-profile:profile "E.ELIB")
    (sb-profile:profile "E.KNOT")
    (sb-profile:profile "E.ELANG")
    (sb-profile:profile "E.ELANG.SYNTAX")
    (sb-profile:profile "E.EXTERN")
    (sb-profile:profile "E.ELANG.VM-NODE")
    (sb-profile:profile "E.RUNE")
    (sb-profile:profile "E.UPDOC")
    ))

(defun profile-finish ()
  #+sbcl (when *use-sprof* (sb-sprof:stop-profiling) (sb-sprof:report))
  #+sbcl (when *use-profile* (sb-profile:report)))

(defun updoc-start (paths 
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
                  (member (pathname-type path)
                    '("updoc" "emaker" "e" "e-awt" "e-swt" "caplet" "txt")
                    :test #'string-equal)))
        (collect path)))
    
    (setf file-paths (nreverse file-paths))
    
    (profile-start)    
    (chain
      #'result+
      (make-instance 'result)
      (lambda () file-paths)
      (lambda () (updoc-file (pop file-paths)))
      (lambda (result)
        (profile-finish)
        (format t "~&~[~D test~:P passed.~:;~:*~D failure~:P in ~D test~:P.~]~%" 
          (result-failure-count result) 
          (result-step-count result))
        (values)))))

(defun updoc-rune-entry (&rest paths)

  ;(when (equal (first paths) "--confine") ...) ; XXX implement this - if nothing else to avoid the expense of making an io-scope

  (block vat-loop-exit
    (e. (e. (vat-safe-scope *vat*) |get| "Ref") |whenResolved| (updoc-start paths) (efun (x)
      (declare (ignore x))
      (return-from vat-loop-exit)))
    (vat-loop))
  (force-output))
