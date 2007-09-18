; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :cl-user)

;(pushnew :e.mutable *features*)
;(pushnew :e.immutable *features*)

;(pushnew :e.instrument.ref-shorten-uses *features*)

;; The presence of this feature causes method bodies to be wrapped in (apply (named-lambda "fqn#method" (...) ...) args) instead of being direct CASE clause forms. This may be helpful for backtraces, but may have a speed/space penalty.
;; Default policy is to use this under OpenMCL, since OpenMCL is poor at displaying function arguments in backtraces.
#+openmcl (pushnew :e.method-lambdas *features*)

;; The presence of this feature indicates that SXHASH is known to produce inadequately varying results for FUNCTIONs. SBCL currently does this.
(when (= (sxhash (lambda ())) 42)
  (pushnew :e.function-sxhash-inadequate *features*))