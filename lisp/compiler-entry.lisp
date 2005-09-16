; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang)

(defun extract-outer-scope (layout 
    &aux (table-sym (gensym))
         (bindings (scope-layout-bindings layout)))
  "Given a scope layout, return a form which evaluates to the scope for that scope layout in its lexical environment."
  ; XXX call something on the Scope maker, don't make one directly
  `(make-instance 'e.knot:scope
    :slot-table
      (let ((,table-sym (make-hash-table :test #'equal :size ',(length bindings))))
        ,@(loop
          with seen = (make-hash-table :test #'equal) 
          for (k . v) in (scope-layout-bindings layout)
          unless (gethash k seen)
            do (setf (gethash k seen) t)
            and collect `(setf (gethash ',k ,table-sym) ,(binding-get-slot-code v)))
        ,table-sym)
    :fqn-prefix ',(scope-layout-fqn-prefix layout)))

(defun delta-extract-outer-scope (final-layout e-node initial-scope
    &aux (rebound (e-coerce (e. (e. (e. e-node |staticScope|) |outNames|) |getKeys|) 'vector))
         (table-sym (gensym)))
  "Return a form which, when evaluated in the appropriate context for 'final-layout', returns the outer scope resulting from the evaluation of 'e-node', assuming that it has already been evaluated, and 'final-layout' is the resulting scope layout, and 'initial-scope' is the outer scope in which it was evaluated."
  `(e.
    ; XXX call something on the Scope maker, don't make one directly
    (make-instance 'e.knot:scope
      :slot-table
        (let ((,table-sym (make-hash-table :test #'equal
                                           :size ',(length rebound))))
          ,@(loop for noun across rebound collect
            `(setf (gethash ',noun ,table-sym)
                   ,(binding-get-slot-code
                     (scope-layout-noun-binding final-layout noun))))
          ,table-sym)
      :fqn-prefix ',(scope-layout-fqn-prefix final-layout))
    |or|
    ',initial-scope))

(defun outer-scope-to-layout (outer-scope)
  (make-instance 'prefix-scope-layout :fqn-prefix (e. outer-scope |getFQNPrefix|) :rest
    (scope-layout-nest ; XXX this is wrong; scopes should have scope-box marks just as scope layouts do (see Scope#nestOuter)
      ; XXX decide what the official interface to getting slots from a Scope is
      (let (layout)
        (e. outer-scope |iterate| (efun (k v)
          (assert (char= #\& (char k 0)))
          (push (cons (subseq k 1) (binding-for-slot v)) layout)))
        (nreverse layout)))))

; XXX support COMPILEing of the CL code when appropriate - SBCL isn't the world
#-(and)
(defun e-to-cl (tree outer-scope)
  (e-coercef outer-scope 'e.knot:scope)
  (let ((outer-layout (outer-scope-to-layout)))
    (multiple-value-bind (local-vars end-layout form) (transform tree outer-layout)
      `(locally
        (declare (optimize (compilation-speed 3)
                           (debug 1)
                           (safety 3)
                           (space 1)
                           (speed 1))
                 #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
        ,(make-lets local-vars `
          (values 
            ,form
            ,(delta-extract-outer-scope end-layout tree outer-scope)))))))

#+(and)
(defun e-to-cl (expr outer-scope)
  (e-coercef outer-scope 'e.knot:scope)
  `(locally
    (declare (optimize (compilation-speed 3)
                       (debug 1)
                       (safety 3)
                       (space 1)
                       (speed 1))
             #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
    ,(sequence-e-to-cl expr outer-scope)))

(defun cl-to-lambda (form &key (name (gensym "cl-to-lambda-")))
  "to show up in profiles"
  (eval `(e.util:named-lambda ,name () ,form)))

; --- ---

; Wrapper so that in profiling, time spent executing E code is separated from the time to compile it into CL
(defun eval-e (tree scope)
  (let ((elib::*allow-unexternalizable-optimization* t))
    (funcall (cl-to-lambda (e-to-cl tree scope)
                           ; using KEYWORD package because using something else might? trigger the sbcl leak
                           :name (intern (format nil "e-eval in scope ~A" (e. scope |getFQNPrefix|)) "KEYWORD")))))

(defun get-translation (tree)
  ; XXX not deterministic - uses *gensym-counter*
  ; XXX document this - * means use |outer-&foo| for outer vars
  (multiple-value-bind (vars layout form) (scope-box-transform '* tree)
    (assert (null vars))
    (assert (eq '* layout))
    form))

