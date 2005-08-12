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

(declaim (inline map-from-hash)
         (ftype (function (t function hash-table) (or null vector list)) map-from-hash))
(defun map-from-hash (result-type function hash-table)
  "Like cl:map, but iterating over hash tables."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (cond
    ((subtypep result-type 'nil)
      (maphash function hash-table))
    ((subtypep result-type 'list)
      (loop for key being each hash-key of hash-table using (hash-value value)
            collect (funcall function key value)))
    ((subtypep result-type 'vector)
      (loop with v = (make-array (hash-table-count hash-table)
                       :element-type
                         (cond
                           ((or (eql result-type 'vector)
                                (and (consp result-type)
                                     (eql (first result-type) 'vector)
                                     (eql (second result-type) '*)))
                             't)
                           ((and (consp result-type)
                                 (eql (first result-type) 'vector))
                             (second result-type))
                           (t
                             (error "unsupported vector result type ~S in map-from-hash" result-type))))
            for key being each hash-key of hash-table using (hash-value value)
            for i from 0
            do (setf (aref v i) (funcall function key value))
            finally (return v)))
    (t
      (error "unsupported result type ~S in map-from-hash" result-type))))


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
    
(declaim (inline without-suffix)
         (ftype (function (string string) (or null string)) without-suffix))
(defun without-suffix (string suffix
    &aux (without-len (- (length string) (length suffix))))
  "Given a string and a suffix, return the string without the suffix, or NIL if the string does not have that suffix."
  (if (and (>= without-len 0)
           (string= suffix string :start2 without-len))
    (subseq string 0 without-len)))
  
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

; --- function declarations ---

(declaim (ftype function
  add-exclusive-io-handler

  run-program
  external-process-input-stream
  external-process-output-stream))
