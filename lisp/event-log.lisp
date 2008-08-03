; Copyright 2008 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; --- Tracing stuff ---

(defvar *turn-log-id*)
(defvar *turn-subserial*)

(declaim (type (or null stream) *causality-output*))
(defvar *causality-output* nil)
(declaim (string *causality-output-comma*))
(defvar *causality-output-comma* "")

;;; --- Stack cleanup ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sym-or-gen (name package-des)
    "Return the symbol in the package if both exist, otherwise a gensym. For robustness against internal symbols going away in future versions (or running on some other lisp)."
    (let ((package (find-package package-des)))
      (or (when package (find-symbol name package))
          (gensym)))))

(defparameter *omit-functions*
  (let ((h (make-hash-table :test #'equal)))
    (loop for f in '(#.(sym-or-gen "MAP-BACKTRACE" "SB-DEBUG")
                     #.(sym-or-gen "BACKTRACE-AS-LIST" "SB-DEBUG")
                     backtrace-value
                     backtrace-prettily
                     event-fields
                     %log-event
                     take-turn-serial
                     (lambda ()))
          do (setf (gethash f h) t))
    h))

(defparameter *stop-functions*
  (let ((h (make-hash-table :test #'equal)))
    (loop for f in '(call-with-turn
                     runner-loop)
          do (setf (gethash f h) t))
    h))

(defun backtrace-prettily ()
  (loop for frame in (e.util:backtrace-value)
        for fname = (first frame)
        for source-path =
          (e.util:native-namestring
            (translate-logical-pathname
              (or #+sbcl (ignore-errors
                           (sb-introspect:definition-source-pathname
                             (sb-introspect:find-definition-source 
                               (fdefinition fname)))) 
                  #p"")))
        for name = 
          (cond
            ((gethash fname *omit-functions*)
             nil)
            ((mangled-verb-p (second frame))
             (format nil "~S.~A(~{~S~^, ~})" 
                     (first frame)
                     (unmangle-verb (second frame))
                     (cddr frame)))
            ((typep frame '(cons (cons (eql #.(sym-or-gen "FAST-METHOD" "SB-PCL"))
                                       (cons (member e-call-dispatch
                                                     e-send-dispatch 
                                                     e-send-only-dispatch)))))
              (destructuring-bind ((fm op (rec-specializer mverb-specializer)) j1 j2 rec mverb &rest args) frame
                (declare (ignore fm mverb-specializer j1 j2))
                (format nil "~s ~S~A~A(~{~S~^, ~})" 
                         rec-specializer
                         rec
                         (ecase op
                           (e-call-dispatch ".")
                           (e-send-dispatch " <- ")
                           (e-send-only-dispatch " <only- "))
                         mverb
                         args)))
            (t
             (write-to-string frame :pretty t :lines 1 :length 20 :level 3 :right-margin nil :miser-width nil :escape t :readably nil)))
        when (gethash fname *stop-functions*)
          do (loop-finish)
        when name
          collect `(("name" . ,name)
                    ("source" . ,source-path)
                    #|XXX extract source-spans from E objects, also |#)))

;;; --- Log ID management ---

(defun log-unique-id ()
  (write-to-string (random 10000000000000000000000000000000000000) :base 36))

(defun make-vat-log-id (label)
  ;; XXX uri-encode label part
  (format nil "tag:cle.cubik.org,2008:vat:~A:~A" (log-unique-id) label))

(defun make-turn-log-id (vat)
  `(("loop" . ,(vat-log-id vat))
    ("number" . ,(incf (turn-serial-counter vat)))))

(defun take-turn-serial (thunk)
  (let ((*turn-log-id* (make-turn-log-id *vat*))
        (*turn-subserial* 0))
    (funcall thunk)))

;;; --- Log output ---

(defun event-fields (classes)
  (let ((stack (e.util:backtrace-value #| XXX not always list |#)))
    `(("$" . ,(coerce classes 'vector))
      ("anchor" .
        (("number" . ,(incf *turn-subserial*))
         ("turn" . ,*turn-log-id*)))
      ("trace" .
       (("calls" . ,(coerce (backtrace-prettily) 'vector)))))))

(defun %log-event (classes alist)
  (princ *causality-output-comma* *causality-output*)
  (setf *causality-output-comma* (format nil ",~%"))
  (json:encode-json
    (append alist
            (event-fields classes))
    *causality-output*))

(defmacro log-event (classes alist)
  `(when *causality-output*
     (%log-event ,classes ,alist)))