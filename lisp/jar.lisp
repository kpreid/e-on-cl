; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.jar
  (:use :cl :elib :zip)
  (:documentation "Access to jar/zip files, so that we can borrow Java E's emakers.")
  (:export))

(in-package :e.jar)

(defun open-jar (pathname)
  (let ((zipfile (open-zipfile pathname)))
    ; XXX finalizer to close zip file
    (e-lambda "org.cubik.cle.prim.open-jar$jar" ()
      (:|getOpt| (subpath)
        "Return a file from the jar file, or null if it does not exist. Does not yet support getting directories."
        (e-coercef subpath 'string)
        (let ((zip-entry (get-zipfile-entry subpath zipfile)))
          (when zip-entry
            (e-lambda |entry| ()
              (:|_clFileWriteDate| ()
                "XXX this interface to be revised.
                 The modification date of an item in a jar is that of the jar itself, for our purposes."
                (file-write-date pathname))
              (:|getText| ()
                ;; XXX merge this with whatever our overall encoding strategy turns out to be
                (let ((data (zipfile-entry-contents zip-entry)))
                  #+sbcl (sb-ext:octets-to-string data :external-format :ascii)
                  #-sbcl (zip::octets-to-string data :default)))
              (:|getTwine| ()
                ;; XXX actual twine support
                (e. |entry| |getText|)))))))))