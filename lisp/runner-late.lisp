; Copyright 2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(def-shorten-methods vr-add-io-handler 4)
(def-shorten-methods vr-remove-io-handler 1)

(defmethod make-runner-for-this-thread (&rest initargs &key &allow-other-keys)
  (locally ; sb-pcl is confused otherwise
    (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
    (apply #'make-instance
      (or #+(or sbcl cmu) 'serve-event-runner
          'queue-blocking-runner)
      initargs)))
