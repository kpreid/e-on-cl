; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.sockets
  (:use :cl :e.util :elib 
        #+sbcl :sb-bsd-sockets
        #+clisp :socket)
  (:documentation "Taming the Lisp's socket interface, if it has one.")
  (:export
    :foo-write
    :foo-connect-tcp
    :get-addr-info))

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
  ;; This vtable imitates a fd-ref as well as being a socket, because a SOCKET handles closing upon GC and so we need to keep it around.

  (:|connect| (this host port opt-ejector)
    (setf host (coerce-typed-vector host '(vector (unsigned-byte 8) 4)))
    (e-coercef port '(unsigned-byte 16))
    (handler-case
        (socket-connect this host port)
      (socket-error (condition)
        (eject-or-ethrow opt-ejector condition))))
  
  ; XXX implement vtable macros then write one for this get-and-setf template
  (:|getNonBlocking| (this) (as-e-boolean (non-blocking-mode this)))
  (:|setNonBlocking| (this new) (setf (non-blocking-mode this) (e-is-true new)))
  
  (:|getSockoptSendBuffer/0| 'sockopt-send-buffer)
  (:|setSockoptSendBuffer| (this new) (setf (sockopt-send-buffer this) new))
  (:|getSockoptReceiveBuffer/0| 'sockopt-receive-buffer)
  (:|setSockoptReceiveBuffer| (this new) (setf (sockopt-receive-buffer this) new))
  
  (:|read| (this max-octets error-ejector eof-ejector)
    "Read up to 'max-octets' currently available octets from the socket, and return them as a ConstList."
    (e. (make-fd-ref (socket-file-descriptor this)) |read| max-octets error-ejector eof-ejector))
  
  (:|getFD| (this) (socket-file-descriptor this))
  )

#+sbcl
(defun errno-to-condition (errno)
  (make-instance 'simple-error
    :format-control "error ~A (~A)"
    :format-arguments (list errno (sb-impl::strerror errno))))

#+sbcl
(defun foo-write (target vector error-ejector)
  "half-baked function used from IPAuthor"
  (setf target (ref-shorten target))
  (e-coercef vector '(vector (unsigned-byte 8)))
  (multiple-value-bind (n errno)
      (sb-unix::unix-write (socket-file-descriptor target) vector 0 (length vector))
    (if (= 0 errno)
      n
      (eject-or-ethrow error-ejector (errno-to-condition errno)))))

#+clisp
(defun foo-write (target vector error-ejector)
  (e-coercef vector '(vector (unsigned-byte 8)))
  (multiple-value-bind (x n)
      (ext:write-byte-sequence vector target :no-hang t)
    (declare (ignore x))
    n))

#-(or sbcl clisp)
(defun foo-write (target vector error-ejector)
  (error "foo-write not implemented"))

#+sbcl
(defun in-progress-socket-error-p (error)
  "xxx:sb-bsd-sockets fails to provide a condition class for EINPROGRESS, so we implement it ourselves using internal stuff"
  (and (typep error 'socket-error)
       (eql (sb-bsd-sockets::socket-error-errno error) 
            #.sb-posix:einprogress)))

#+sbcl
(defun foo-connect-tcp (addr-info opt-ejector)
  (e-coercef addr-info 'pseudo-addr-info)
  (let ((socket (make-instance 'inet-socket 
                  :type :stream
                  :protocol :tcp)))
    (setf (non-blocking-mode socket) t)
    (handler-case
        (socket-connect socket
                        (host-ent-address (addr-info-host-ent addr-info)) 
                        (addr-info-port addr-info))
      ((satisfies in-progress-socket-error-p) (condition)
        (declare (ignore condition)))
      (socket-error (condition)
        (eject-or-ethrow opt-ejector condition)))
    socket))

#+clisp
(defun foo-connect-tcp (addr-info opt-ejector)
  (e-coercef addr-info 'pseudo-addr-info)
  (socket-connect (addr-info-port addr-info)
                  (addr-info-unresolved-name addr-info)
                  :element-type '(unsigned-byte 8) 
                  :buffered nil 
                  :timeout 0))

; --- ---

#+sbcl (defun %convert-handler-target (target)
         (socket-file-descriptor target))
#+clisp (defun %convert-handler-target (target)
          target)

(defun foo-add-receive-handler (target e-function)
  ;; vat-add-io-handler takes care of establishing the turn
  (vat-add-io-handler
    *vat*
    (%convert-handler-target target)
    :input
    (named-lambda e-receive-handler (x)
      (declare (ignore x)) 
      (e. e-function |run|))))

(defun foo-remove-receive-handler (handler)
  (vat-remove-io-handler handler))

; --- ---

#+(or sbcl clisp)
(defclass pseudo-addr-info ()
  (#+sbcl
   (host-ent :initarg :host-ent :accessor addr-info-host-ent :type host-ent)
   #+clisp
   (unresolved-name :initarg :unresolved-name :accessor addr-info-unresolved-name :type string)
   (port :initarg :port :accessor addr-info-port :type (unsigned-byte 16))))

(defun get-addr-info (host service hints)
  (e-coercef host '(or null string))
  (e-coercef service '(or null string))
  (e-coercef hints 'null)
  (e<- (efun () 
	 (or 
	  #+sbcl (make-instance 'pseudo-addr-info
				:host-ent (get-host-by-name host)
				:port (if service (parse-integer service) 0))
                 
	  #+clisp (make-instance 'pseudo-addr-info
				 :unresolved-name host
				 :port (if service (parse-integer service) 0))
	  (error "~A not implemented for this Lisp" 'get-addr-info)))
       |run|))

(def-vtable pseudo-addr-info
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    #+sbcl
    (with-slots (host-ent port) this
      (e. tw |write| "<network address ")
      (e. tw |quote| (host-ent-name host-ent))
      (loop for address in (host-ent-addresses host-ent) do
        (e. tw |write| " ")
        (e. tw |print| (e. "." |rjoin| (map 'vector #'princ-to-string address))))
      (e. tw |write| ":")
      (e. tw |print| port)
      (e. tw |write| ">"))
    #+clisp
    (with-slots (unresolved-name port) this
      (e. tw |write| "<network address ")
      (e. tw |print| unresolved-name)
      (e. tw |write| ":")
      (e. tw |print| port)
      (e. tw |write| ">"))))

; --- ---

; XXX this vtable should be in the main system: streams might be used for non-sockets purposes.
(def-vtable stream
  #+clisp
  (:|read/3| (stream max-elements error-ejector eof-ejector)
    "Read up to 'max-octets' currently available octets from the FD, and return them as a ConstList. Blocks if read(2) would block."
    (let ((buf (make-array max-elements :element-type '(unsigned-byte 8) :fill-pointer t)))
      (setf (fill-pointer buf)
        (ext:read-byte-sequence buf stream :no-hang t))
      (if (and (= 0 (fill-pointer buf))
               (eq (socket-status (cons stream :input) 0) :eof))
        (eject-or-ethrow eof-ejector "stream EOF")
        buf)))
  #+clisp
  (:|getSockoptSendBuffer/0| (stream)
    (socket-options stream :so-sndbuf))
  #+clisp
  (:|getSockoptReceiveBuffer/0| (stream)
    (socket-options stream :so-rcvbuf)))

; --- ---

; XXX this should not be in sockets but in something more general
(defun make-fd-ref (fd)
  (e-lambda "org.cubik.cle.io.FDRef" ()
    (:|getFD| () fd)

    #+sbcl
    (:|read| (max-octets error-ejector eof-ejector)
    "Read up to 'max-octets' currently available octets from the FD, and return them as a ConstList. Blocks if read(2) would block."
      ; XXX be able to avoid allocating the buffer
      ; thanks to nyef on irc://irc.freenode.net/lisp for this code --
      ;   http://paste.lisp.org/display/7891
      ;   http://meme.b9.com/cview.html?utime=3324058742&channel=lisp&start=3324055147&end=3324062347#utime_requested
      (let ((buf (make-array max-octets :element-type '(unsigned-byte 8)
                                        :fill-pointer 0
                                        :adjustable nil)))
        (multiple-value-bind (n-read errno)
            ; XXX SBCL internal package. Show me something more appropriate and I'll use it
            (sb-unix:unix-read fd
                               (sb-sys:vector-sap (sb-impl::%array-data-vector buf))
                               max-octets)
          (case n-read
            ((nil)
              (case errno
                ((#.sb-posix:ewouldblock)
                  buf)
                (otherwise
                  ; XXX typed error with errno slot
                  (eject-or-ethrow error-ejector (format nil "file descriptor read error: ~A (~A)" errno (sb-int:strerror errno))))))
            ((0)
              (eject-or-ethrow eof-ejector "socket EOF"))
            (otherwise
              (setf (fill-pointer buf) n-read)
              buf)))))))
