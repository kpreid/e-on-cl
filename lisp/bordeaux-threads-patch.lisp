; Copyright 2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.util)

(defglobal +sole-thread+ '#:sole-thread)

#+#.(cl:handler-case
      (cl:progn (bordeaux-threads:current-thread) '(:or)) 
      (cl:error () '(:and)))
(defmethod bordeaux-threads:current-thread ()
  "Patch by E-on-CL."
  +sole-thread+)



#+#.(cl:handler-case
      (cl:progn (bordeaux-threads:make-condition-variable) '(:or)) 
      (cl:error () '(:and)))
(progn
  (defclass pseudo-condition-variable () ())
  (defmethod bordeaux-threads:make-condition-variable ()
    "Patch by E-on-CL."
    (make-instance 'pseudo-condition-variable)))
