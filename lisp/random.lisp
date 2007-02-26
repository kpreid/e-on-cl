; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defconstant +swiss-size-in-octets+ 20)
(defconstant +swiss-upper-bound+ (expt 2 (* 8 +swiss-size-in-octets+)))

(def-vtable random-state
  (:|__printOn| (this out)
    (e-coercef out +the-text-writer-guard+)
    (with-standard-io-syntax
      (e. out |write| (prin1-to-string this))))
  (:|nextSwiss| (this)
    (random +swiss-upper-bound+ this))
  (:|diverge| (this)
    (make-random-state this)))