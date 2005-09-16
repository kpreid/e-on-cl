; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.lisp-test
  (:use :cl 
        #+e.sb-rt :sb-rt #-e.sb-rt :rt
        :e.elib)
  (:export :system-test))
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
