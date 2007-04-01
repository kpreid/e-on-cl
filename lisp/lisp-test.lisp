; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.lisp-test
  (:use :cl 
        #+e.sb-rt :sb-rt #-e.sb-rt :rt
        :e.elib)
  (:export :system-test
           
           :collect-mismatches))
(cl:in-package :e.lisp-test)
(export 'deftest)

(defun test-once (system)
  (rem-all-tests)
  (let ((*package* #.*package*))
    (map nil
       #'load
       (directory
         (merge-pathnames
           (make-pathname :name :wild 
                          :type "tlisp" 
                          :directory '(:relative "ltests"))
           (asdf:component-pathname system)))))
  (do-tests))

(defun system-test (op system)
  "Invoked by an implementation of asdf:test-op."
  (declare (ignore op))
  (with-simple-restart (continue "Proceed as if the lisp-side tests passed.")
    (loop
      (with-simple-restart (:retry "Retry the lisp-side tests.")
        (if (test-once system)
          (return)
          (error "Some tests failed.")))))
  (fresh-line)
  (rem-all-tests))

;;; --- Utilities for tests ---

(defun collect-mismatches (test key-1 key-2 inputs)
  "For each element of INPUTS, apply KEY-1 and KEY-2 to the element. Return a list of triples of element from INPUTS, result of KEY-1, and result of KEY-2, for each element where TEST yields false given the results of KEY-1 and KEY-2."
  (loop for input #-sbcl #-sbcl in (coerce inputs 'list)
                  #+sbcl #+sbcl #+sbcl #+sbcl #+sbcl being the elements of inputs
        for a = (funcall key-1 input)
        for b = (funcall key-2 input)
        unless (funcall test a b)
        collect (list input a b)))
