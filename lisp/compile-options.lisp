; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :cl-user)

; The non-MOP modes are buggy and/or unfinished. I'm leaving them in for fixing up in case we need to support an implementation not having MOP.
(pushnew :e.vtable-collect.use-mop *features*)
;(pushnew :e.vtable-collect.use-example *features*)
;(pushnew :e.vtable-collect.use-typelist *features*)

;(pushnew :e.instrument.ref-shorten-uses *features*)