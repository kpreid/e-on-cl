; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
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

;; XXX review these two routines; they generate code which is put into compiled emakers, and as such should avoid as much current-implementation-dependency as they can

#+(or) ;; believed to be stale
(defun extract-outer-scope (layout 
    &aux (table-sym (gensym))
         (bindings (scope-layout-bindings layout)))
  "Given a scope layout, return a form which evaluates to the scope for that scope layout in its lexical environment."
  ; XXX call something on the Scope maker instead of make-scope
  ;; XXX pass local-definitions information
  `(locally (declare (notinline make-instance))
    (e.knot:make-scope
      (list
        ',(scope-layout-fqn-prefix layout)
        ,@(loop
          with seen = (make-hash-table :test #'equal) 
          for (k . v) in (scope-layout-bindings layout)
          unless (gethash k seen)
            do (setf (gethash k seen) t)
            and collect `(list ',(format nil "&~A" k) ,(binding-get-slot-code v)))))))

(defun delta-extract-outer-scope (final-layout e-node initial-scope-form
    &aux (rebound (map 'vector #'ref-shorten (e-coerce (e. (e. (e. e-node |staticScope|) |outNames|) |getKeys|) 'vector))))
  "Return a form which, when evaluated in the appropriate context for 'final-layout', returns the outer scope resulting from the evaluation of 'e-node', assuming that it has already been evaluated, and 'final-layout' is the resulting scope layout, and 'initial-scope' is the outer scope in which it was evaluated."
  `(e.
    ; XXX call something on the Scope maker, don't make one directly
    ;; XX pass local-definitions information
    (locally (declare (notinline make-instance))
      (e.knot:make-scope
        ',(scope-layout-fqn-prefix final-layout)
        (list
          ,@(loop for noun across rebound collect
            `(list ',(format nil "&~A" noun)
                   ,(binding-get-slot-code
                     (scope-layout-noun-binding final-layout noun)))))))
    |or|
    ,initial-scope-form))

(defun outer-scope-to-layout (outer-scope)
  (make-instance 'prefix-scope-layout :fqn-prefix (e. outer-scope |getFQNPrefix|) :rest
    (scope-layout-nest ; XXX this is wrong; scopes should have scope-box marks just as scope layouts do (see Scope#nestOuter)
      ; XXX decide what the official interface to getting slots from a Scope is
      (let (layout)
        (e. outer-scope |iterate| (efun (k v)
          (setf k (ref-shorten k))
          (assert (char= #\& (char k 0)))
          (push (cons (subseq k 1) (binding-for-slot v)) layout)))
        (nreverse layout)))))

(defun e-to-cl (expr outer-scope)
  (e-coercef outer-scope 'e.knot:scope)
  (e-coercef expr 'e.elang.vm-node::|ENode|)
  (require-kernel-e expr nil)
  (e.knot:require-node-fits-scope expr outer-scope nil)
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
    (eval `(with-unexternalizable-optimization
             (e.util:named-lambda ,name () ,form)))))

; --- ---

; Wrapper so that in profiling, time spent executing E code is separated from the time to compile it into CL
(defun eval-e (tree scope)
  (funcall (cl-to-lambda (e-to-cl tree scope)
                         ; using KEYWORD package because using something else might? trigger the sbcl leak
                         :name (intern (format nil "e-eval in scope ~A" (e. scope |getFQNPrefix|)) "KEYWORD"))))

(defun get-translation (expr)
  ; XXX document this - * means use |outer-&foo| for outer vars
  ; XXX poking at compiler internals
  (e.elang.compiler.seq::naive-sequence-to-form
    (e.elang.compiler.seq::sequence-expr expr '* 'return-value)
    'return-value))

(defglobal +the-evaluator+ (e-lambda "$evaluator" (:stamped +deep-frozen-stamp+)
  ; XXX eval, evalToSingleton, matchBind...
  (:|getKernelNodes| ()
    (let ((map (e. +the-make-const-map+ |fromIteratable|
                 (e-lambda "$kernelNodesIterator" () (:|iterate| (f)
                   (do-symbols (symbol :e.kernel)
                     (block nil
                       (efuncall f (symbol-name symbol) (or (get symbol 'static-maker)
                                                         (return)))))
                   nil))
                 +e-true+)))
      (e. map |sortKeys|)))))

;;; --- Cached compilation ---

(defvar *efasl-program*)
(defvar *efasl-result*)

(deftype externalizable-for-efasl ()
  '(or bit-vector
       character
       cl-type-guard
       keyword
       null
       number
       pathname
       string))

(defun compile-e-to-file (expr output-file fqn-prefix opt-scope)
  "Compile an EExpr into a compiled Lisp file. The file, when loaded, will set *efasl-result* to a list containing the nouns used by the expression and a function which, when called with an OuterScope object followed by slots for each of the nouns, will return as EVAL-E would. If opt-scope is provided, some of the nouns in the expression may be compiled into literal occurrences of their values in that scope."
  (require-kernel-e expr nil)
  (when opt-scope
    ;; XXX this is wrongish: we should execute the check *always*
    (e.knot:require-node-fits-scope expr opt-scope nil))
  (let* ((all-nouns (map 'list #'ref-shorten (e-coerce (e. (e. (e. expr |staticScope|) |namesUsed|) |getKeys|) 'vector)))
         (needed-nouns '())
         (needed-syms '())
         (initial-scope-var (gensym "INITIAL-SCOPE"))
         (layout
           (make-instance 'prefix-scope-layout :fqn-prefix fqn-prefix
                                               :rest
             (scope-layout-nest
               (mapcar
                 (lambda (noun)
                   (escape-bind (fail)
                       (let* ((slot (if opt-scope
                                      (e. opt-scope |getSlot| noun)
                                      (efuncall fail)))
                              (value (progn
                                       (unless (typep slot 'e-simple-slot)
                                         (efuncall fail))
                                       (ref-shorten (e. slot |getValue|)))))
                         (unless (typep value 'externalizable-for-efasl)
                           ;; xxx in principle, this can be extended to any externalizable value 
                           (efuncall fail))
                         (cons noun (make-instance 'value-binding :value value)))
                     (-unused-)
                       (declare (ignore -unused-))
                       (push noun needed-nouns)
                       (cons noun (first (push (make-symbol noun) needed-syms)))))
                 all-nouns))))
         (*efasl-program*
           `(setf *efasl-result*
                  (list
                    ',needed-nouns
                    (lambda (,initial-scope-var ,@needed-syms)
                      ;; XXX we shouldn't need to reference the compiler
                      ;; implementation here
                      ,(e.elang.compiler.seq:sequence-e-to-cl
                         expr 
                         layout
                         initial-scope-var))))))
    (e. e.knot:+sys-trace+ |doing|
      (format nil "compiling ~A" output-file)
      (efun ()
        (multiple-value-bind (truename warnings-p failure-p)
            (compile-file (merge-pathnames
                            #p"lisp/universal.lisp"
                            (asdf:component-pathname 
                              (asdf:find-system +the-asdf-system+)))
                          :output-file output-file
                          :verbose nil
                          :print nil)
          (declare (ignore truename warnings-p))
          (assert (not failure-p) () "Compilation for ~A failed." output-file))))))

(defun load-compiled-e (file scope)
  (let ((*efasl-result* nil))
    (during ("CL load ~A" (file-namestring file))
      (load file :verbose nil :print nil))
    (during ("execute ~A" (file-namestring file))
      (destructuring-bind (names function) *efasl-result*
        (apply function scope (mapcar (lambda (n) (e. scope |getSlot| n)) names))))))

;;; --- ASDF-integrated compilation ---

(defglobal +core-scope-additions+ (e.knot:make-scope "__core$"
  `(("ConstList"       ,(type-specifier-to-guard 'vector)) ;; XXX twines are lists
    ("getSafeScope"    ,(efun () (vat-safe-scope *vat*)))
    ("lisp"            ,e.knot:+lisp+)
    ("DeepFrozenStamp" ,elib:+deep-frozen-stamp+))))

(defglobal +core-scope+
   (e. +core-scope-additions+ |or| e.knot:+shared-safe-scope+))

(defmethod asdf:perform ((o asdf:compile-op) (c e.system:e-core-source-file))
  (loop for input-file in (asdf:input-files o c)
        for output-file in (asdf:output-files o c)
        do
    (compile-e-to-file 
      (e.syntax:parse-to-kernel 
        (e. (e.extern:pathname-to-file input-file) |getTwine|))
      output-file
      (format nil "org.cubik.cle.core.~A$"
        (pathname-name input-file))
      +core-scope+)))

(defmethod asdf:perform ((o asdf:load-op) (c e.system:e-core-source-file))
  (dolist (file (asdf:input-files o c))
    (load-compiled-e file +core-scope+)))

