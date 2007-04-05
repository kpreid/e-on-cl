; Copyright 2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(def-shorten-methods vr-add-io-handler 4)
(def-shorten-methods vr-remove-io-handler 1)

(defmethod make-runner-for-this-thread ()
  (or #+(or sbcl cmu) (make-instance 'serve-event-runner)
      (make-instance 'runner)))
