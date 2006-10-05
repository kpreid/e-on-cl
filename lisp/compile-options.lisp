; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :cl-user)

;(pushnew :e.mutable *features*)
;(pushnew :e.immutable *features*)

; The non-MOP modes are buggy and/or unfinished. I'm leaving them in for fixing up in case we need to support an implementation not having MOP.
(pushnew :e.vtable-collect.use-mop *features*)
;(pushnew :e.vtable-collect.use-example *features*)
;(pushnew :e.vtable-collect.use-typelist *features*)

;(pushnew :e.instrument.ref-shorten-uses *features*)

;; The presence of this feature causes method bodies to be wrapped in (apply (named-lambda "fqn#method" (...) ...) args) instead of being direct CASE clause forms. This may be helpful for backtraces, but may have a speed/space penalty.
;; Default policy is to use this under OpenMCL, since OpenMCL is poor at displaying function arguments in backtraces.
#+openmcl (pushnew :e.method-lambdas *features*)