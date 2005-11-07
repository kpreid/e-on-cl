; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang.compiler)

(defvar *trace-loading* nil)

(defmacro during ((fmt &rest fa) &body body)
  `(prog2
     (when *trace-loading*
       (format *trace-output* ,(concatenate 'string "~&; begin " fmt "~%") ,@fa))
     (progn ,@body)
     (when *trace-loading*
       (format *trace-output* ,(concatenate 'string "~&; end " fmt "~%") ,@fa))))

;; XXX stale?
(defun extract-outer-scope (layout 
    &aux (table-sym (gensym))
         (bindings (scope-layout-bindings layout)))
  "Given a scope layout, return a form which evaluates to the scope for that scope layout in its lexical environment."
  ; XXX call something on the Scope maker, don't make one directly
  `(locally (declare (notinline make-instance))
    (make-instance 'e.knot:scope
      :slot-table
        (let ((,table-sym (make-hash-table :test #'equal :size ',(length bindings))))
          ,@(loop
            with seen = (make-hash-table :test #'equal) 
            for (k . v) in (scope-layout-bindings layout)
            unless (gethash k seen)
              do (setf (gethash k seen) t)
              and collect `(setf (gethash ',k ,table-sym) ,(binding-get-slot-code v)))
          ,table-sym)
      :fqn-prefix ',(scope-layout-fqn-prefix layout))))

(defun delta-extract-outer-scope (final-layout e-node initial-scope-form
    &aux (rebound (e-coerce (e. (e. (e. e-node |staticScope|) |outNames|) |getKeys|) 'vector))
         (table-sym (gensym)))
  "Return a form which, when evaluated in the appropriate context for 'final-layout', returns the outer scope resulting from the evaluation of 'e-node', assuming that it has already been evaluated, and 'final-layout' is the resulting scope layout, and 'initial-scope' is the outer scope in which it was evaluated."
  `(e.
    ; XXX call something on the Scope maker, don't make one directly
    (locally (declare (notinline make-instance))
      (make-instance 'e.knot:scope
        :slot-table
          (let ((,table-sym (make-hash-table :test #'equal
                                             :size ',(length rebound))))
            ,@(loop for noun across rebound collect
              `(setf (gethash ',noun ,table-sym)
                     ,(binding-get-slot-code
                       (scope-layout-noun-binding final-layout noun))))
            ,table-sym)
        :fqn-prefix ',(scope-layout-fqn-prefix final-layout)))
    |or|
    ,initial-scope-form))

(defun outer-scope-to-layout (outer-scope)
  (make-instance 'prefix-scope-layout :fqn-prefix (e. outer-scope |getFQNPrefix|) :rest
    (scope-layout-nest ; XXX this is wrong; scopes should have scope-box marks just as scope layouts do (see Scope#nestOuter)
      ; XXX decide what the official interface to getting slots from a Scope is
      (let (layout)
        (e. outer-scope |iterate| (efun (k v)
          (assert (char= #\& (char k 0)))
          (push (cons (subseq k 1) (binding-for-slot v)) layout)))
        (nreverse layout)))))

(defun e-to-cl (expr outer-scope)
  (e-coercef outer-scope 'e.knot:scope)
  (during ("e-to-cl")
    (funcall (if *trace-loading* #'print #'identity)
      `(locally
        (declare (optimize (compilation-speed 3)
                           (debug 1)
                           (safety 3)
                           (space 1)
                           (speed 1))
                 #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
        ,(e.elang.compiler.seq:sequence-e-to-cl 
           expr
           (outer-scope-to-layout outer-scope)
           `',outer-scope)))))

;; XXX support COMPILEing of the CL code when appropriate - SBCL isn't the world

(defun cl-to-lambda (form &key (name (gensym "cl-to-lambda-")))
  "this is a separate function to show up distinctly in profiles"
  (during ("CL eval")
    (eval `(e.util:named-lambda ,name () ,form))))

; --- ---

; Wrapper so that in profiling, time spent executing E code is separated from the time to compile it into CL
(defun eval-e (tree scope)
  (let ((elib::*allow-unexternalizable-optimization* t))
    (funcall (cl-to-lambda (e-to-cl tree scope)
                           ; using KEYWORD package because using something else might? trigger the sbcl leak
                           :name (intern (format nil "e-eval in scope ~A" (e. scope |getFQNPrefix|)) "KEYWORD")))))

(defun get-translation (expr)
  ; XXX document this - * means use |outer-&foo| for outer vars
  ; XXX poking at compiler internals
  (e.elang.compiler.seq::naive-sequence-to-form
    (e.elang.compiler.seq::sequence-expr expr '* 'return-value)
    'return-value))

;;; --- Cached compilation ---

(defvar *efasl-program*)
(defvar *efasl-result*)

(defun compile-e-to-file (expr output-file fqn-prefix)
  "Compile an EEXpr into a compiled Lisp file. The file, when loaded, will set *efasl-result* to a list containing the nouns used by the expression and a function which, when called with an OuterScope object followed by slots for each of the nouns, will return as EVAL-E would."
  (let* ((nouns (coerce (e-coerce (e. (e. (e. expr |staticScope|) |namesUsed|) |getKeys|) 'vector) 'list))
         (syms (mapcar #'make-symbol nouns))
         (initial-scope-var (gensym "INITIAL-SCOPE"))
         (layout
           (make-instance 'prefix-scope-layout :fqn-prefix fqn-prefix
                                               :rest
             (scope-layout-nest
               (mapcar #'cons nouns syms))))
         (*efasl-program*
           `(setf *efasl-result*
                  (list
                    ',nouns
                    (lambda (,initial-scope-var ,@syms)
                      ;; XXX we shouldn't need to reference the compiler
                      ;; implementation here
                      ,(e.elang.compiler.seq:sequence-e-to-cl
                         expr 
                         layout
                         initial-scope-var))))))
    ;; XXX instead of having to do this to handle nested cases,
    ;; we should implement a-u-o with the macrolet/macroexpand trick
    (let ((elib::*allow-unexternalizable-optimization* nil))
      (multiple-value-bind (truename warnings-p failure-p)
          (compile-file (merge-pathnames
                          #p"lisp/universal.lisp"
                          (asdf:component-pathname 
                            (asdf:find-system +the-asdf-system+)))
                        :output-file output-file)
        (declare (ignore truename warnings-p))
        (assert (not failure-p) () "Compilation for ~A failed." output-file)))))

(defun load-compiled-e (file scope)
  (let ((*efasl-result* nil))
    (during ("CL load ~A" (file-namestring file))
      (load file :verbose nil :print nil))
    (during ("execute ~A" (file-namestring file))
      (destructuring-bind (names function) *efasl-result*
        (apply function scope (mapcar (lambda (n) (e. scope |getSlot| n)) names))))))
