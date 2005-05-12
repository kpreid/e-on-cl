; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.sockets
  (:use :cl :elib #+sbcl :sb-bsd-sockets)
  (:documentation "Taming the Lisp's socket interface, if it has one.")
  (:export))

(in-package :e.sockets)

;;; XXX there are no tests for this code beyond org.cubik.cle.IPAuthor's use of it

(defun coerce-typed-vector (vector type)
  "Coerce 'vector' to a specialized vector of type 'type', obeying E reference transparency rules."
  (e-coercef vector 'vector)
  (handler-case
    (coerce vector type)
    (type-error ()
      (map type #'(lambda (v) (e-coerce v (cadr type))) vector))))

#+sbcl
(def-vtable sb-bsd-sockets:socket
  (:|connect/3| (this host port opt-ejector)
    (setf host (coerce-typed-vector host '(vector (unsigned-byte 8) 4)))
    (e-coercef port '(unsigned-byte 16))
    (handler-case
        (socket-connect this host port)
      (socket-error (condition)
        (eject-or-ethrow opt-ejector condition))))
  
  ; XXX implement vtable macros then write one for this get-and-setf template
  (:|getNonBlocking/0| (this) (as-e-boolean (non-blocking-mode this)))
  (:|setNonBlocking/1| (this new) (setf (non-blocking-mode this) (e-is-true new)))
  
  (:|getSockoptSendBuffer/0| #'sockopt-send-buffer)
  (:|setSockoptSendBuffer/1| (this new) (setf (sockopt-send-buffer this) new))
  (:|getSockoptReceiveBuffer/0| #'sockopt-receive-buffer)
  (:|setSockoptReceiveBuffer/1| (this new) (setf (sockopt-receive-buffer this) new))
  
  ; XXX this should be on a general fd-ref object
  ; XXX be able to avoid allocating the buffer
  (:|read/3| (this max-octets error-ejector eof-ejector)
    "Read up to 'max-octets' currently available octets from the socket, and return them as a ConstList."
    ; thanks to nyef on irc://irc.freenode.net/lisp for this code --
    ;   http://paste.lisp.org/display/7891
    ;   http://meme.b9.com/cview.html?utime=3324058742&channel=lisp&start=3324055147&end=3324062347#utime_requested
    (let ((buf (make-array max-octets :element-type '(unsigned-byte 8)
                                      :fill-pointer 0
                                      :adjustable nil)))
      (multiple-value-bind (n-read errno)
          ; XXX SBCL internal package. Show me something more appropriate and I'll use it
          (sb-unix:unix-read (socket-file-descriptor this)
                             (sb-sys:vector-sap (sb-impl::%array-data-vector buf))
                             max-octets)
        (case n-read
          ((nil)
            (case errno
              ((#.sb-unix:ewouldblock)
                buf)
              (otherwise
                ; XXX typed error with errno slot
                (eject-or-ethrow error-ejector (format nil "file descriptor read error: ~A (~A)" errno (sb-int:strerror errno))))))
          ((0)
            (eject-or-ethrow eof-ejector "socket EOF"))
          (otherwise
            (setf (fill-pointer buf) n-read)
            buf)))))
  )