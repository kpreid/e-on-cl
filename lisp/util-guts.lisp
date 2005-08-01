; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.util)

;;; --- serve-event ---

#+sbcl (setf (fdefinition 'serve-event) #'sb-sys:serve-event)
#+cmu  (setf (fdefinition 'serve-event) #'system:serve-event)

(unless (fboundp 'serve-event)
  (defun serve-event (&optional timeout)
    "Portable fake serve-event if we don't have a real one."
    (when timeout (sleep timeout))
    nil))

;;; --- run-program ---

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
        (declare (ignore io))
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

;;; --- backtrace ---

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

;;; --- MOP ---

#+#.(cl:if (cl:eql (cl:symbol-package 'e.util:class-precedence-list) 
                   (cl:find-package :e.util)) :t :nil)
  (defun class-precedence-list (&rest args)
    (error "No MOP package known for this implementation, to retrieve class-precedence-list from."))
