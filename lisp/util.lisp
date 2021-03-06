; Copyright 2005-2008 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:in-package :e.util)

;; sb-introspect does not have an asdf system
#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require :sb-introspect))

; --- Globals ---

;; The definition of DEFGLOBALS is in packages.lisp.

(defmacro defglobal (&rest args)
  #+sbcl `(defconstantonce ,@args) ; use this only on implementations which don't die on non-externalizable constants
  #-sbcl `(defvar ,@args))

(defmacro defconstantonce (name value-form &optional documentation)
  `(progn
    (eval-when (:compile-toplevel)
      (declaim (type t ,name)))
    (eval-when (:load-toplevel :execute)
      (%defconstantonce ',name ,value-form ',documentation))))

(defun %defconstantonce (name value documentation)
  (if (boundp name)
    (warn "Not recomputing ~A variable ~A." 'defconstantonce name)
    ;; sbcl emits bogus-or-irrelevant "redefining special as constant" warnings due to our declaim
    (handler-bind ((warning #'muffle-warning))
      (eval `(defconstant ,name ',value
               ,@(when documentation (list documentation))))))
  name)

; --- misc ---

(defglobal +the-asdf-system+
  (asdf:find-system :e-on-cl))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +arity-limit+ call-arguments-limit))
(deftype arity () `(integer 0 ,+arity-limit+))

(declaim (inline mangle-verb)
         (ftype (function (string arity) keyword) mangle-verb))
(defun mangle-verb (verb arity)
  (declare (string verb)
           (type arity arity)
           (optimize (safety 3)))
  (intern (let* ((sep (length verb))
                 (digits (max (loop for n of-type arity = arity
                                                   then (floor n 10)
                                    while (> n 0)
                                    count t)
                              1))
                 (mv-string (make-array (+ sep 1 digits)
                              :element-type 'character
                              :fill-pointer nil
                              :adjustable nil)))
            (replace mv-string verb :end1 sep)
            (setf (aref mv-string sep) #\/)
            (loop for pos from (1- (length mv-string)) above sep
                  for n of-type arity = arity then (floor n 10)
                  do (setf (aref mv-string pos)
                             (digit-char (mod n 10) 10)))
            mv-string)
          "KEYWORD"))

(declaim (inline unmangle-verb)
         (ftype (function (keyword) (values string (integer 0))) unmangle-verb))
(defun unmangle-verb (mverb
    &aux (mv-string (symbol-name mverb))
         (slash     (position #\/ mv-string :from-end t)))
  (declare (optimize (safety 3)))
  (assert slash)
  (values
    (subseq mv-string 0 slash)
    (parse-integer mv-string :start (1+ slash))))

(declaim (inline mverb-verb)
         (ftype (function (keyword string) t) mverb-verb))
(defun mverb-verb= (mverb verb)
  "Return whether the verb of MVERB is equal to VERB."
  (let ((mv-string (symbol-name mverb)))
    (string= mv-string
             verb
             :end1 (position #\/ mv-string :from-end t))))

(defun convention-capitalize (string)
  (string-upcase string :end (min 1 (length string))))

(defun convention-uncapitalize (string)
  (if (and (plusp (length string))
           (string/= string (string-upcase string)))
    (concatenate 'string (string (char-downcase (aref string 0)))
                         (subseq string 1))
    string))

(defun guess-lowercase-string (string)
  (if (notany #'lower-case-p string)
    (string-downcase string)
    string))

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

(define-modify-macro nreverse-here ()
  nreverse
  "In-place updating version of cl:nreverse.")

(defmacro handler-case-with-backtrace (form &rest clauses
    &aux (backtrace-var (gensym)))
  "Like HANDLER-CASE, but the handlers get second arguments which are as much backtrace information as the Lisp provides."
  `(let (,backtrace-var)
    (handler-case
      (handler-bind (((or ,@(mapcar #'first clauses))
                      #'(lambda (c) (declare (ignore c)) (setf ,backtrace-var (backtrace-value)))))
        ,form)
      ,@(loop for (type . lambda) in clauses collect
         `(,type (,(caar lambda))
            ((lambda ,@lambda) ,(caar lambda) ,backtrace-var))))))


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

(defun function-lambda-list (function)
  "Return the lambda list of the given function, or '(&rest <unknown-lambda-list>) if it cannot be determined."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (first
    (or
      #+sbcl
        (list (sb-introspect:function-lambda-list function))
      #+excl ; xx is this the most appropriate feature test?
        (list (nth-value 0 (excl:arglist function)))
      #+(or abcl clisp)
        (list (ext:arglist function))
      #+ccl ; xxx appropriate feature test? should this be restricted to #+openmcl?
        (list (ccl:arglist function))
      #+cmu
        (list
          (let ((*package* (find-package :cl-user)))
            (read-from-string
              (kernel:%function-arglist function)))) ; XXX is this a supported interface? probably not
      (second (function-lambda-expression function))
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

; --- function declarations ---

(declaim (ftype function
  serve-event
  
  native-pathname
  native-namestring
  
  run-program
  external-process-status
  external-process-exit-code
  external-process-input-stream
  external-process-output-stream
  external-process-error-stream
  
  system-symbol
  
  backtrace-value))
