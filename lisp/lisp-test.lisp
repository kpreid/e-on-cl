; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.lisp-test
  (:use :cl 
        #+sbcl :sb-rt #-sbcl :rt
        :e.elib)
  (:export :system-test))
  
(cl:in-package :e.lisp-test)

(defun system-test (op system)
  "Invoked by an implementation of asdf:test-op."
  (declare (ignore op))
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
  (with-simple-restart (continue "Proceed as if the lisp-side tests passed.")
    (loop
      (with-simple-restart (:retry "Retry the lisp-side tests.")
        (if (do-tests)
          (return)
          (error "Some tests failed.")))))
  (fresh-line)
  (rem-all-tests))
