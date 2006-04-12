; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %define-if-available-parse-mapping (nd)
    (let* ((name (if (listp nd)
                   (string (second nd))
                   (string nd)))
           (here-symbol (if (listp nd)
                          (first nd)
                          (intern name #.*package*))))
      (values here-symbol name))))

(defun %define-if-available (packages symbols copier)
  (dolist (nd symbols)
    (multiple-value-bind (here-symbol name) (%define-if-available-parse-mapping nd)
      (dolist (package (remove-if-not #'find-package packages))
        (multiple-value-bind (other-symbol found) (find-symbol name package)
          (when (eql found :external)
            (funcall copier here-symbol other-symbol)))))))

(defmacro define-if-available (packages symbols &key (accessor 'fdefinition))
  `(progn
     ,(when (member accessor '(fdefinition symbol-function))
        `(declaim (ftype function ,@(mapcar #'%define-if-available-parse-mapping symbols))))
     (%define-if-available ',packages ',symbols 
       (lambda (.out. .in.)
         (setf (,accessor .out.) (,accessor .in.))))))

(defun default-alias (alias original)
  (unless (fboundp alias)
    (setf (fdefinition alias)
          (fdefinition original))))

;;; --- serve-event ---

;; Simple passthrough

(define-if-available 
  (#+sbcl :sb-sys
   #+cmu  :system)
  (:serve-event     
   (add-io-handler :add-fd-handler)
   (remove-io-handler :remove-fd-handler)))

;; Implement our own

#-(or sbcl cmu) (progn
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
    (values)))

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

(defclass exclusive-io-handler ()
  ((group :initarg :group :reader %eih-group)
   (current-base :initform nil :accessor %eih-base)
   (installer :initarg :installer :accessor %eih-installer)))

(defun call-with-io-handler-exclusion (body group &optional (marker t))
  (assert marker)
  (if (exclusion-group-active group)
    (error "attempted to activate already-active IO handler exclusion group ~S" group)
    (unwind-protect
      (progn
        (setf (exclusion-group-active group) marker)
        (funcall body))
      (setf (exclusion-group-active group) nil)
      (loop for reinstaller = (pop (exclusion-group-excluded group))
            while reinstaller
            do (funcall reinstaller)))))

(defmacro with-io-handler-exclusion ((group marker) &body body)
  `(call-with-io-handler-exclusion (lambda () ,@body) ,group ,marker))

(defun add-exclusive-io-handler (group target direction function)
  "Add a SERVE-EVENT handler which will not be invoked by recursive serve-event in the dynamic scope of another handler in its \"exclusion group\"."
  (let ((wrap-handler (make-instance 'exclusive-io-handler 
                        :group group)))
    (with-slots ((current-base-handler current-base)) wrap-handler
      (let (installer)
        (labels ((install ()
                   (if (null (%eih-installer wrap-handler))
                     (warn "attempted to reinstall ~S for ~S ~S" wrap-handler target direction)
                     (setf current-base-handler
                       (add-io-handler target 
                         direction
                         (lambda (target)
                           (if (exclusion-group-active group)
                             (progn
                               (push installer (exclusion-group-excluded group))
                               (remove-io-handler (the (not null) current-base-handler))
                               (setf current-base-handler nil))
                             (with-io-handler-exclusion (group wrap-handler)
                               (funcall function target)))))))))
          (setf installer #'install)
          (setf (%eih-installer wrap-handler) installer)
          (install)
          wrap-handler)))))

(defun remove-exclusive-io-handler (handler)
  (let ((base-handler (slot-value handler 'current-base)))
    (when base-handler
      (e.util:remove-io-handler base-handler)))
  (setf (exclusion-group-excluded (%eih-group handler))
        (remove
          (%eih-installer handler)
          (exclusion-group-excluded (%eih-group handler))))
  (setf (%eih-installer handler) nil)
  (values))

;;; --- pathname handling ---

(define-if-available 
  (#+sbcl :sb-ext) 
  ("NATIVE-PATHNAME" "NATIVE-NAMESTRING"))

(default-alias 'native-pathname 'pathname)
(default-alias 'native-namestring 'namestring)

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

(define-if-available
  (#+ccl :ccl
   #+sbcl :sb-ext
   #+cmu :extensions)
  (:run-program))

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
      args))

  (defun external-process-error-stream (&rest args)
    (apply 
      #+ccl  #'ccl:external-process-error-stream
      #+sbcl #'sb-ext:process-error
      #+cmu  #'extensions:process-error
      #-(or sbcl ccl cmu)
        (error "Don't know where to find external-process-error-stream")
      args)))

(unless (fboundp 'run-program)
  (defun run-program (&rest args)
    (declare (ignore args))
    (error "Don't know where to find RUN-PROGRAM")))

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

;;; --- miscellaneous ---

(defun system-symbol (name package system)
  "Return the symbol with name NAME in PACKAGE, after ensuring that asdf system SYSTEM is loaded."
  (asdf:operate 'asdf:load-op system)
  (multiple-value-bind (symbol found)
      (find-symbol name package)
    (assert found () "The symbol ~A::~A was not found after loading ~A."
                     package name system)
    symbol))

;;; --- end ---

#+sbcl (sb-ext:lock-package #.*package*)
