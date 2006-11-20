; Copyright 2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.knot)

(let ((process (run-program "rune" '("-show") :search t :wait nil :output :stream)))
  (loop for line = (read-line (external-process-output-stream process) nil)
        while line
        do
    (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "^\s*-De.home=(.*)$" line))))
      (when (plusp (length match))
        (e.knot:found-e-on-java-home (native-pathname (elt match 0)))
        (loop-finish)))))

