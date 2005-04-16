; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elang)

; XXX this is the leftovers from being split into other files

(def-vtable unbound-variable
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (let ((e-var-name (slot-symbol-var-name (cell-error-name this))))
      (if (and e-var-name *java-e-compatible*)
        (e. tw |print| "problem: undefined variable: " e-var-name)
        (call-next-method)))))
