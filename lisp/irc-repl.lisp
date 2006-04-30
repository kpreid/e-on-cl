;; This file is derived from an example supplied with the cl-irc package, whose LICENSE is:
;; 
;; Copyright (c) 2002 Jochen Schmidt
;; Copyright (c) 2003 Erik Enge and Brian Mastenbrook
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;; THIS SOFTWARE IS PROVIDED "AS IS" AND THERE ARE NEITHER EXPRESSED NOR
;; IMPLIED WARRANTIES - THIS INCLUDES, BUT IS NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.IN
;; NO WAY ARE THE AUTHORS LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION)
;; 
;; For further details contact the authors of this software.
;; 
;;   Erik Enge, erik@nittin.net
;;   Brian Mastenbrook, bmastenb@indiana.edu

(defpackage :e.irc-repl
  (:use :common-lisp :irc)
  (:export
    :start-ceel
    :shut-up
    :un-shut-up))
(in-package :e.irc-repl)

(defvar *connection*)
(defvar *nickname* "")
(defvar *scope*)

(defun shut-up ()
  (setf (irc:client-stream *connection*) (make-broadcast-stream)))

(defun un-shut-up ()
  (setf (irc:client-stream *connection*) *trace-output*))

(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
         ,else))))

(defun valid-message (string prefix &key space-allowed)
  (if (eql (search prefix string :test #'char-equal) 0)
      (and (or space-allowed
               (not (find #\space string :start (length prefix))))
           (length prefix))
      nil))

(defun strip-address (string &key (address *nickname*) (final nil))
  (loop for i in (list (format nil "~A " address)
                       (format nil "~A: " address)
                       (format nil "~A:" address)
                       (format nil "~A, " address))
        do (aif (valid-message string i :space-allowed t)
                (return-from strip-address (subseq string it))))
  (and (not final) string))

(defun msg-hook (message)
  (let ((destination (if (string-equal (first (arguments message)) *nickname*)
                         (source message)
                         (first (arguments message))))
        (cmd (strip-address (trailing-argument message) :final t)))
    (if cmd
      (let ((source
              (handler-case
                (e.syntax:parse-to-kernel cmd)
                (error (condition)
                  (declare (ignore condition))
                  (privmsg *connection* destination (format nil "syntax error"))
                  (return-from msg-hook)))))
        (handler-case
          (multiple-value-bind (result new-scope)
              (elang:eval-e source *scope*)
            (privmsg *connection* destination (format nil "# value: ~A" (elib:e-quote result)))
            (setf *scope* new-scope))
          (error (condition)
            (privmsg *connection* destination (format nil "# ~A" (elib:e-quote condition))))))
    
    
      )))

(defun start-irc-repl (nick server &rest channels)
  ; xxx have a better definition of our initial scope; some description of what things are safely included
  (setf *scope* (elib:e. (elib:vat-safe-scope elib:*vat*) |with| "timer" e.extern:+the-timer+))
  (setf *nickname* nick)
  (setf *connection* (connect :nickname *nickname* :server server))
  (loop for channel in channels do (join *connection* channel))
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  #+(or sbcl
        openmcl)
  (start-background-message-handler *connection*)
  #-(or sbcl
        openmcl)
  (read-message-loop *connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *connection* 'irc::irc-privmsg-message)
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook))
