; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.util)

; --- Globals ---

;; The definition of DEFGLOBALS is in packages.lisp.

(defmacro defglobal (&rest args)
  #+e.immutable 
    `(defconstantonce ,@args)
  #-e.immutable
    `(defparameter ,@args))

(defmacro defconstantonce (name value-form &optional documentation)
  `(progn
    (eval-when (:compile-toplevel)
      (declaim (special ,name)))
    (eval-when (:load-toplevel :execute)
      (eval 
        `(defconstant ,',name
          ,(if (boundp ',name)
            (symbol-value ',name)
            ,value-form)
          ,@',(when documentation (list documentation)))))))

; --- misc ---

(defun aan (s)
  (cond
    ((string= s "") "")
    ((member (aref s 0) '#.(coerce "aeiouh" 'list) :test #'char-equal)
       (concatenate 'string "an " s))
    (t (concatenate 'string "a " s))))

(defmacro named-lambda (name &rest def)
  "Equivalent to the cl:lambda macro, except that it attempts to have 'name' show up in backtraces and function printing if possible. 'name' is not evaluated and must be a function name suitable for FLET/LABELS."
  `(flet ((,name ,@def)) #',name))  

#+sbcl (setf (fdefinition 'serve-event) #'sb-sys:serve-event)
#+cmu  (setf (fdefinition 'serve-event) #'system:serve-event)

(unless (fboundp 'serve-event)
  (defun serve-event (&optional timeout)
    "Portable fake serve-event if we don't have a real one."
    (when timeout (sleep timeout))
    nil))

; --- RUN-PROGRAM and friends ---

; Our run-program was originally just SBCL's run-program, so we imitate its interface on implementations that don't have a sufficiently similar function.

; xxx Allegro has something called run-shell-command, but I haven't investigated what its interface is.

#+(or lispworks clisp)
(defclass external-process ()
  ((input-stream :initarg :input-stream
                 :reader external-process-input-stream)
   (output-stream :initarg :output-stream
                  :reader external-process-output-stream))
  (:documentation "For Lisps whose RUN-PROGRAM-or-similar does not return an object/structure."))

#-(or lispworks clisp)
(defun run-program (&rest args) 
  (apply
    #+ccl    #'ccl:run-program
    #+sbcl   #'sb-ext:run-program
    #+cmu    #'extensions:run-program
    #-(or sbcl ccl cmu)
      (error "Don't know where to find RUN-PROGRAM")
    
    args))

#+clisp
  (defun squote (string)
    (format nil "'~A'"
      (elib:e-call-dispatch "'\\''" :|rjoin/1|
        (elib:e-call-dispatch string :|split/1| "'"))))

#+clisp
  (defun run-program (program arguments &rest key-args &key input output wait &allow-other-keys)
    
    ;; clisp's RUN-PROGRAM appears to be broken for this use, but MAKE-PIPE-IO-STREAM not. kpreid 2005-07-18
    (if (and (eql input :stream)
             (eql output :stream)
             (eql wait nil))
      (multiple-value-bind (io i o)
          (ext:make-pipe-io-stream
            (elib:e-call-dispatch " " :|rjoin/1| (map 'vector #'squote (cons program arguments)))
            :element-type 'character
            :external-format e.extern:+standard-external-format-common-name+
            :buffered nil)
        (make-instance 'external-process
          :input-stream o
          :output-stream i))
      
      (multiple-value-bind (v1 v2 v3)
          (apply #'ext:run-program
            program 
            :arguments arguments
            (let ((key-args (copy-seq key-args)))
              (remf key-args :error)
              key-args))
        (cond
          ((and (eql input :stream)
                (eql output :stream))
            ; (close v1) ; bidirectional stream
            (make-instance 'external-process
              :input-stream v3
              :output-stream v2))
          ((eql input :stream)
            (make-instance 'external-process
              :input-stream v1))
          ((eql output :stream)
            (make-instance 'external-process
              :output-stream v1))
          (t
            (make-instance 'external-process))))))

#+lispworks (progn
  ; xxx not the full set of args, but enough for our purposes
  (defun run-program (command args &key input output error (wait t))
    (labels ((pass ()
               (system:run-shell-command 
                 (cons command args)
                 :input input
                 :output output
                 :error-output (if (eql error 't) nil error)
                 :wait wait)))
      ; This condition written based on 
      ; http://www.lispworks.com/documentation/lw44/LWRM/html/lwref-630.htm#pgfId-1011601
      (cond
        (wait
          (pass)
          (make-instance 'external-process))
        ((not (member ':stream (list input output error)))
          (pass)
          (make-instance 'external-process))
        (t
          (multiple-value-bind (io-stream error-stream pid) (pass)
            (declare (ignore error-stream pid))
            (make-instance 'external-process 
              :input-stream io-stream
              :output-stream io-stream))))))
  )

#-(or lispworks clisp) (progn
  (defun external-process-input-stream (&rest args)
    (apply 
      #+ccl  #'ccl:external-process-input-stream
      #+sbcl #'sb-ext:process-input
      #+cmu  #'extensions:process-input
      #-(or sbcl ccl cmu)
        (error "Don't know where to find external-process-input-stream")
      args))
  
  (defun external-process-output-stream (&rest args)
    (apply 
      #+ccl  #'ccl:external-process-output-stream
      #+sbcl #'sb-ext:process-output
      #+cmu  #'extensions:process-output
      #-(or sbcl ccl cmu)
        (error "Don't know where to find external-process-output-stream")
      args)))

; --- misc functions ---

(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  ; Regarding the SBCL conditionals below: I want this code fast, but there seems to be *no* way to get SBCL not to output warnings on the parts which it can't optimize *at every point at which it is inlined*.

  (declaim (inline mangle-verb)
           (ftype (function (string (integer 0)) keyword) mangle-verb))
  (defun mangle-verb (verb arity)
    (declare (optimize (speed #-sbcl 3 #+sbcl 2) (safety 3) (space 2)))
    (intern (format nil "~A/~A" verb arity) "KEYWORD"))
  
  (declaim (inline unmangle-verb)
           (ftype (function (keyword) (values string (integer 0))) unmangle-verb))
  (defun unmangle-verb (mverb
      &aux (mv-string (symbol-name mverb))
           (slash     (position #\/ mv-string)))
    (declare 
             (optimize (speed #-sbcl 3 #+sbcl 1) (safety 3) (space 2)))
    (assert slash)
    (values
      (subseq mv-string 0 slash)
      (parse-integer mv-string :start (1+ slash)))))
    
; XXX not tested 
(declaim (inline without-suffix)
         (ftype (function (string string) (or null string)) without-suffix))
(defun without-suffix (string suffix
    &aux (without-len (- (length string) (length suffix))))
  "Given a string and a suffix, return the string without the suffix, or NIL if the string does not have that suffix."
  (if (and (>= without-len 0)
           (string= suffix string :start2 without-len))
    (subseq string 0 without-len)))
  
; XXX not tested 
(declaim (inline without-prefix)
         (ftype (function (string string) (or null string)) without-prefix))
(defun without-prefix (string prefix 
    &aux (split-index (length prefix)))
  "Given a string and a prefix, return the string without the prefix, or NIL if the string does not have that prefix."
  (if (and (>= (length string) split-index)
           (string= prefix string :end2 split-index))
    (subseq string split-index)))

(defmacro popping-equal-case (list-sym &rest cases &aux (arg-sym (gensym)))
  "for use in implementing command-line parsing"
  `(let ((,arg-sym (first ,list-sym)))
    (cond
      ,@(loop for (keys . body) in cases collect
        (if (eql keys 'otherwise)
          `(t 
            ,@body)
          `((member ,arg-sym ',(if (atom keys) (list keys) keys) :test #'equal)
            (pop ,list-sym)
            ,@body))))))

; --- floating-point rules ---

; I was going to use this to get NaN/Infinity for OpenMCL, but when I tried:
; 2 > (/ 1d0 0d0)
; Unhandled exception 11 at 0x00004774, context->regs at #xf0135458

;#+ccl (defmacro with-appropriate-floating-point-rules (&body forms
;    &aux (save (gensym "fpu")))
;  `(let ((,save (ccl:get-fpu-mode)))
;    (ccl:set-fpu-mode :rounding-mode :nearest
;                      :overflow nil
;                      :underflow nil
;                      :division-by-zero nil
;                      :invalid nil
;                      :inexact nil)
;    (unwind-protect
;      (progn ,@forms)
;      (apply #'ccl:set-fpu-mode ,save))))

#+(and sbcl x86)
(defun %with-appropriate-floating-point-rules (f)
  (let ((save (sb-int:get-floating-point-modes)))
    (unwind-protect
      (progn
        (sb-int:set-floating-point-modes
          :traps '())
        (funcall f))
      (apply #'sb-int:set-floating-point-modes save))))

#-(or (and sbcl x86))
(defun %with-appropriate-floating-point-rules (f)
  (funcall f))

(declaim (inline %with-appropriate-floating-point-rules))

(defmacro with-appropriate-floating-point-rules (&body forms)
  `(%with-appropriate-floating-point-rules (lambda () ,@forms)))

; --- lambda lists ---

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require :sb-introspect))

(defun function-lambda-list (function)
  "Return the lambda list of the given function, or '(&rest <unknown-lambda-list>) if it cannot be determined."
  (first
    (or
      #+sbcl
        (list (sb-introspect:function-arglist function))
      #+allegro ; XXX appropriate feature test?
        (list (nth-value 0 (excl:arglist function)))
      #+(or abcl clisp)
        (list (ext:arglist function))
      #+ccl ; XXX appropriate feature test? should this be restricted to #+openmcl?
        (list (ccl:arglist function))
      #+cmu
        (list
          (let ((*package* (find-package :cl-user)))
            (read-from-string
              (kernel:%function-arglist function)))) ; XXX is this a supported interface? probably not
      (cdr (function-lambda-expression function))
      '((&rest <unknown-lambda-list>)))))

(defun lambda-list-arguments-range (lambda-list)
  "Given a lambda list, returns two integers specifying an inclusive range such that a number of arguments outside that range is an error.

&rest and &body result in the maximum being cl:call-arguments-limit. XXX there should be an option to return nil.

XXX &key and &allow-other-keys are not yet supported, and will result in a too-low maximum."
  (loop 
    with maxsofar = 0
    with minsofar = 0
    with optionally = nil
    for elem in lambda-list
    do (cond
        ((eq elem '&optional)
          (setf optionally t))
        ((member elem '(&rest &body))
          (setf maxsofar call-arguments-limit)
          (loop-finish))
        ((member elem lambda-list-keywords)
          ; XXX make an explicit termination case on (&aux &allow-other-keys &environment &whole) and make encountering an unknown keyword an error
          (loop-finish))
        (t
          (incf maxsofar)
          (unless optionally
            (incf minsofar))))
    finally (return (values minsofar maxsofar))))

(defun function-arguments-range (function)
  (lambda-list-arguments-range (function-lambda-list function)))

(defun function-responds-to (function arity)
  (multiple-value-bind (min max) (function-arguments-range function)
    (<= min arity max)))

; XXX delete this log fragment once all approaches mentioned have been investigated/implemented
; [11:00] kpreid: Are there any semi-portable interfaces to get the argument and return types/names/arity of a function?
; [11:01] dan_b: kp: there's some stuff in clocc, i think.  but iirc it's pretty poor
; [11:02] piso: kpreid: you might look at sb-introspect.lisp, in sbcl/src/contrib/sb-introspect
; [11:03] dan_b: although it's a long way from "semi-portable"
; [11:03] piso: it would be more semi-portable if other implementations implemented it :)
; [11:04] piso: kpreid: allegro and abcl have ARGLIST
; [11:05] rtoy_: function-lambda-expression might work, but it's not required to return anything useful.

; --- backtrace ---

(defun backtrace-value ()
  "Returns a value describing the stack in unspecified detail, suitable for printing."
  (or
    #+sbcl 
      (sb-debug:backtrace-as-list)
    #+abcl ; xxx untested
      (ext:backtrace-as-list)
    #+cmu ; xxx untested
      (with-output-to-string (*debug-io*)
        (debug:backtrace))
    ;; xxx this makes OpenMCL 0.14.2-p1 die reliably: 
    ;; Can't find active area, -> kernel debugger
    ;; #+openmcl
    ;;   (with-output-to-string (*debug-io*)
    ;;     ;; Unfortunately, this has two detail levels: "not enough", and "too much". We'll take "too much".
    ;;     (ccl:print-call-history))
    nil))
    
;; From IRC, some time ago:
;;   <kpreid> Are there any semi-portable interfaces to capture a stack trace, as a debugger might do interactively?
;;   <tbmoore> kpreid: The swank support, perhaps.
;; So we could perhaps avoid reinventing some platform layering by using SWANK - but it'd be another dependency, and a rather odd one. Hm. -- kpreid 2005-06-19
