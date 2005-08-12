; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.util)

;;; --- serve-event ---

;; Simple passthrough

#+sbcl 
(progn
  (setf (fdefinition 'serve-event)       #'sb-sys:serve-event)
  (setf (fdefinition 'add-io-handler)    #'sb-sys:add-fd-handler)
  (setf (fdefinition 'remove-io-handler) #'sb-sys:remove-fd-handler))
#+cmu
(progn
  (setf (fdefinition 'serve-event)       #'system:serve-event)
  (setf (fdefinition 'add-io-handler)    #'system:add-fd-handler)
  (setf (fdefinition 'remove-io-handler) #'system:remove-fd-handler))

;; Implement our own

#+(or clisp) (progn
  ;; XXX threading: for our purposes, this would better be represented as a vat/runner slot
  (defvar *serve-event-handlers* (make-hash-table))
  
  (defclass handler ()
    ((direction :initarg :direction
                :type '(member :input :output)
                :accessor handler-direction)
     (function  :initarg :function
                :accessor handler-function)))

  (defclass zombie-handler () ())
  
  
  (defun add-io-handler (target direction function)
    (let ((handler (make-instance 'handler :direction direction :function function)))
      (setf (gethash handler *serve-event-handlers*) target)
      handler))
  
  (defun remove-io-handler (handler)
    (coerce handler 'handler)
    (remhash handler *serve-event-handlers*)
    (change-class handler 'zombie-handler)
    (values))
  
  (defun serve-event (&optional timeout)
    #+e.serve-event-trace (format *trace-output* "~&; entering e serve-event ~A with ~A handlers~%" timeout (hash-table-count *serve-event-handlers*))
    (let* ((status-input
             (map-from-hash
               'list
               (lambda (handler stream)
                 `(,stream ,(handler-direction handler) . ,handler))
               *serve-event-handlers*))
           (statuses
            (if status-input
              (progn
                #+e.serve-event-trace (format *trace-output* "~&; entering socket-status")
                (socket:socket-status
                  (copy-tree status-input)
                  timeout))
              (if timeout
                (progn
                  #+e.serve-event-trace (format *trace-output* "~&; sleeping (no handlers)~%")
                  (sleep timeout))
                (break "Sleeping indefinitely: serve-event with no handlers and no timeout")))))
      (loop for (stream nil . handler) in status-input
            for (nil    nil . status)  in statuses
            do #+e.serve-event-trace (format *trace-output* "~&; serve-event: calling handler ~S function ~S for ~S~%" handler (handler-function handler) stream)
               (funcall (handler-function handler) stream)))
  #+e.serve-event-trace (format *trace-output* "~&; exiting e serve-event~%")))

;; Stub

(unless (fboundp 'serve-event)
  (defun serve-event (&optional timeout)
    "Portable fake serve-event if we don't have a real one."
    (when timeout (sleep timeout))
    nil))

;;; --- non-reentrant serve-event handler layer ---

(defclass io-handler-exclusion-group () 
  ((active   :initform nil :accessor  exclusion-group-active)
   (excluded :initform '() :accessor exclusion-group-excluded)))

(defun add-exclusive-io-handler (group target direction function)
  "Add a SERVE-EVENT handler which will not be invoked by recursive serve-event in the dynamic scope of another handler in its \"exclusion group\"."
  (labels ((install (&aux handler) 
             (setf handler
               (add-io-handler target 
                 direction
                 (lambda (target)
                   (if (exclusion-group-active group)
                     (progn
                       (push #'install (exclusion-group-excluded group))
                       (remove-io-handler (the (not null) handler)))
                     (unwind-protect
                       (progn
                         (setf (exclusion-group-active group) handler)
                         (funcall function target))
                       (setf (exclusion-group-active group) nil)
                       (loop for reinstaller = (pop (exclusion-group-excluded group))
                             while reinstaller
                             do (funcall reinstaller)))))))))
    (install)))

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

