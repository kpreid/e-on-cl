;;; Used for file compilation of E code without writing to a temporary .lisp file.

(in-package :e.elang.compiler)

(macrolet ((it () 
             *efasl-program*)) 
  (it))