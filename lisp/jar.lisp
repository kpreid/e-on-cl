; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.jar
  (:use :cl :e.elib :zip)
  (:documentation "Access to jar/zip files, so that we can borrow Java E's emakers.")
  (:export))

(in-package :e.jar)

(defun open-jar (pathname)
  (let ((zipfile (open-zipfile pathname)))
    ; XXX finalizer to close zip file
    (e-lambda "org.cubik.cle.prim.open-jar$jar" ()
      (:|getOpt| ((subpath 'string))
        "Return a file from the jar file, or null if it does not exist. Does not yet support getting directories."
        (let ((zip-entry (get-zipfile-entry subpath zipfile)))
          (when zip-entry
            (e-lambda |entry| ()
              (:|_clFileWriteDate| ()
                "XXX this interface to be revised.
                 The modification date of an item in a jar is that of the jar itself, for our purposes."
                (file-write-date pathname))
              (:|getText| ()
                (let ((data (zipfile-entry-contents zip-entry)))
                  ;; use sbcl's if it's available, because it's exported,
                  ;; otherwise borrow zip's internals
                  ;; XXX use flexi-streams directly instead
                  #+sbcl (sb-ext:octets-to-string data :external-format :ascii)
                  #-sbcl (zip::octets-to-string data :us-ascii)))
              (:|getTwine| ()
                ;; XXX imitating EoJ jar uris; review
                (e. (e. |entry| |getText|)
                    |asFrom|
                    (format nil "jar:~A!~A" pathname subpath))))))))))