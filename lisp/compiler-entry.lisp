; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elang)

(defun extract-outer-scope (inner-scope 
    &aux (table-sym (gensym))
         (bindings (inner-scope-bindings inner-scope)))
  "Given an inner scope, return a form which generates an equivalent outer scope."
  ; XXX call something on the Scope maker, don't make one directly
  `(make-instance 'e.knot:scope
    :slot-table
      (let ((,table-sym (make-hash-table :test #'equal :size ',(length bindings))))
        ,@(loop
          with seen = (make-hash-table :test #'equal) 
          for (k . v) in (inner-scope-bindings inner-scope)
          unless (gethash k seen)
            do (setf (gethash k seen) t)
            and collect `(setf (gethash ',k ,table-sym) ,(binding-get-slot-code v)))
        ,table-sym)
    :fqn-prefix ',(inner-scope-fqn-prefix inner-scope)))

(defun delta-extract-outer-scope (final-inner e-node initial-scope
    &aux (rebound (e-coerce (e. (e. (e. e-node |staticScope|) |outNames|) |getKeys|) 'vector))
         (table-sym (gensym)))
  "Return a form which, when evaluated in the appropriate context for 'final-inner', returns the outer scope resulting from the evaluation of 'e-node', assuming that it has already been evaluated, and 'final-inner' is the resulting inner scope, and 'initial-scope' is the outer scope in which it was evaluated."
  `(e.
    ; XXX call something on the Scope maker, don't make one directly
    (make-instance 'e.knot:scope
      :slot-table
        (let ((,table-sym (make-hash-table :test #'equal
                                           :size ',(length rebound))))
          ,@(loop for noun across rebound collect
            `(setf (gethash ',noun ,table-sym)
                   ,(binding-get-slot-code
                     (inner-scope-noun-binding final-inner noun))))
          ,table-sym)
      :fqn-prefix ',(inner-scope-fqn-prefix final-inner))
    |or|
    ',initial-scope))

; XXX support COMPILEing of the CL code when appropriate - SBCL isn't the world
(defun e-to-cl (tree outer-scope)
  (e-coercef outer-scope 'e.knot:scope)
  (let* ((outer-name-map
           (make-instance 'prefix-inner-scope :fqn-prefix (e. outer-scope |getFQNPrefix|) :rest
             (inner-scope-nest ; XXX this is wrong; outer scopes should have scope-box marks just as inner scopes do (see Scope#nestOuter)
               ; XXX decide what the official interface to getting slots from a Scope is
               (let (inner-scope)
                 (e. outer-scope |iterate| (e-lambda (:|run/2| (k v)
                   (assert (char= #\& (char k 0)))
                   (push (cons (subseq k 1) (binding-for-slot v)) inner-scope))))
                 (nreverse inner-scope))))))
    (multiple-value-bind (local-vars end-name-map form) (transform tree outer-name-map)
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
            ,(delta-extract-outer-scope end-name-map tree outer-scope)))))))

(defun cl-to-lambda (form &key (name (gensym "cl-to-lambda-")))
  "to show up in profiles"
  (eval `(e.util:named-lambda ,name () ,form)))

; --- ---

; Wrapper so that in profiling, time spent executing E code is separated from the time to compile it into CL
(defun eval-e (tree scope)
  (funcall (cl-to-lambda (e-to-cl tree scope)
                         ; using KEYWORD package because using something else might? trigger the sbcl leak
                         :name (intern (format nil "e-eval in scope ~A" (e. scope |getFQNPrefix|)) "KEYWORD"))))

(defun get-translation (tree)
  ; XXX not deterministic - uses *gensym-counter*
  ; XXX document this - * means use |outer-&foo| for outer vars
  (multiple-value-bind (vars name-map form) (scope-box-transform '* tree)
    (assert (null vars))
    (assert (eq '* name-map))
    form))

