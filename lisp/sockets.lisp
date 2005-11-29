; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(defpackage :e.sockets
  (:use :cl :e.util :elib 
        #+sbcl :sb-bsd-sockets
        #+clisp :socket)
  #+openmcl (:import-from :openmcl-socket :socket-error)
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

  (:|getFD/0| 'socket-file-descriptor)

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
(defun foo-write (target vector error-ejector &key (start 0) (end (length vector)))
  "half-baked function used from IPAuthor"
  (setf target (ref-shorten target))
  (e-coercef vector '(vector (unsigned-byte 8)))
  (multiple-value-bind (n errno)
      (sb-unix::unix-write (socket-file-descriptor target) vector start end)
    (if (zerop errno)
      n
      (eject-or-ethrow error-ejector (errno-to-condition errno)))))

#+clisp
(defun foo-write (target vector error-ejector &key (start 0) (end (length vector)))
  (e-coercef vector '(vector (unsigned-byte 8)))
  (when (typep target 'deferred-socket)
    (setf target (deferred-socket-socket target)))
  (handler-case
    (multiple-value-bind (x n)
        (ext:write-byte-sequence vector target :no-hang t :start start :end end)
      (declare (ignore x))
      n)
    (error (c)
      (eject-or-ethrow error-ejector c))))

#-(or sbcl clisp)
(defun foo-write (target vector error-ejector)
  (error "foo-write not implemented"))

#+sbcl
(defun in-progress-socket-error-p (error)
  "xxx:sb-bsd-sockets fails to provide a condition class for EINPROGRESS, so we implement it ourselves using internal stuff"
  (and (typep error 'socket-error)
       (eql (sb-bsd-sockets::socket-error-errno error) 
            #.sb-posix:einprogress)))

#+openmcl
(defun in-progress-socket-error-p (error)
  (declare (ignore error))
  nil)


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

(defgeneric socket-shorten (socket))

(defmethod socket-shorten ((socket t))
  socket)
  
#+sbcl
(defun foo-make-socket (domain type)
  (check-type domain (member :internet))
  (check-type type (member :stream))
  (make-instance 'inet-socket
    :type     (ref-shorten type)
    :protocol 0))

#+(or clisp openmcl) (progn
  (defclass deferred-socket ()
    ((domain :initarg :domain
             :reader foo-socket-domain
             :type keyword)
     (type :initarg :type
           :reader foo-socket-type
           :type keyword)
     (real-socket :initform nil
                  :accessor deferred-socket-socket))
    (:documentation "For socket interfaces which don't offer unbound/unconnected socket objects."))

  (defmethod socket-shorten ((socket deferred-socket))
    (or (deferred-socket-socket socket)
        (error "~S is not yet a native socket" socket))))

#+clisp    
(defun foo-make-socket (domain type)
  (check-type domain (member :internet))
  (check-type type (member :stream))
  (make-instance 'deferred-socket :domain domain :type type))

#+openmcl 
(defun foo-make-socket (domain type)
  (check-type domain (member :internet))
  (check-type type (member :stream))
  (make-instance 'deferred-socket :domain domain :type type))

#+sbcl
(defun foo-bind-socket (socket addr-info)
  (socket-bind socket
               (host-ent-address (addr-info-host-ent addr-info))
               (addr-info-port addr-info)))

#+sbcl
(defun foo-connect-socket (socket addr-info)
  (socket-connect socket
                  (host-ent-address (addr-info-host-ent addr-info))
                  (addr-info-port addr-info)))

#+clisp
(defun foo-connect-socket (socket addr-info)
  (assert (not (deferred-socket-socket socket)))
  (setf (deferred-socket-socket socket)
        (socket-connect (addr-info-port addr-info)
                        (addr-info-unresolved-name addr-info)
                        :element-type '(unsigned-byte 8) 
                        :buffered nil 
                        :timeout 0))
  (values))

#+openmcl 
(defun foo-connect-socket (socket addr-info)
  (setf (deferred-socket-socket socket)
        ;; OpenMCL happens to use the same keywords we do
        (openmcl-socket:make-socket
          :address-family (foo-socket-domain socket)
          :type (foo-socket-type socket)
          :connect :active
          :remote-host (addr-info-ip-number addr-info)
          :remote-port (addr-info-port addr-info))))

#+sbcl
(defun foo-listen (socket opt-backlog)
  ;; XXX proper backlog value
  ;; XXX errors?
  (socket-listen socket (or opt-backlog 64)))

; --- ---

#+sbcl 
(defun %convert-handler-target (target)
  (e. target |getFD|))
#+clisp (defun %convert-handler-target (target)
          (socket-shorten target))

#+openmcl (defun %convert-handler-target (target)
            (socket-shorten target))

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

(defmacro form-or (&rest forms)
  (first forms))

(defun foo-sockopt-receive-buffer (impl-socket)
  (form-or
    #+sbcl (sockopt-receive-buffer impl-socket)
    #+clisp (socket-options (socket-shorten impl-socket) :so-rcvbuf)
    #+openmcl 512 ;; XXX not available? "surely it's at least this big"
    (error "foo-sockopt-receive-buffer not implemented")))

(defun foo-sockopt-send-buffer (impl-socket)
  (form-or
    #+sbcl (sockopt-send-buffer impl-socket)
    #+clisp (socket-options (socket-shorten impl-socket) :so-sndbuf)
    #+openmcl 512 ;; XXX not available? "surely it's at least this big"
    (error "foo-sockopt-send-buffer not implemented")))

;; XXX name/peername need to return their results in terms of sockaddr-oid E-structures so they're not ipv4-tied

#+clisp
(defun convert-socket-address-return (fn socket)
  (multiple-value-bind (host port) (funcall fn socket nil)
    (values (coerce (split-sequence:split-sequence #\. (first (split-sequence:split-sequence #\Space host :count 1)) :count 4) 'vector)
            port)))

(defun foo-socket-name (socket)
  (setf socket (socket-shorten socket))
  (form-or 
    #+sbcl (socket-name socket)
    #+openmcl (values (ip4-number-to-vector
                        (openmcl-socket:local-host socket))
                      (openmcl-socket:local-port socket))
    #+clisp (convert-socket-address-return #'socket-stream-local socket)
    (error "foo-socket-name not implemented")))

(defun foo-socket-peername (socket)
  (setf socket (socket-shorten socket))
  (form-or 
    #+sbcl (socket-peername socket)
    #+openmcl (values (ip4-number-to-vector
                        (openmcl-socket:remote-host socket))
                      (openmcl-socket:remote-port socket))
    #+clisp (convert-socket-address-return #'socket-stream-peer socket)
    (error "foo-socket-peername not implemented")))


; --- ---

(defun ip4-number-to-vector (ip4-number)
  (coerce 
    (nreverse 
      (loop for x = ip4-number then (ash x -8) 
            collect (logand x #xFF))) 
    'vector))

#+(or sbcl clisp openmcl)
(progn 
  (defclass pseudo-addr-info ()
    (#+sbcl
     (host-ent :initarg :host-ent :accessor addr-info-host-ent :type host-ent)
     #+openmcl
     (ip-number :initarg :ip-number :accessor addr-info-ip-number :type (unsigned-byte 32))
     #+clisp
     (unresolved-name :initarg :unresolved-name :accessor addr-info-unresolved-name :type string)
     (port :initarg :port :accessor addr-info-port :type (unsigned-byte 16))))
  
  (def-vtable pseudo-addr-info
    (:|__printOn| (this tw)
      (e-coercef tw +the-text-writer-guard+)
      (e. tw |write| "<network address")
      #+sbcl
      (with-slots (host-ent port) this
        (e. tw |write| " ")
        (e. tw |quote| (host-ent-name host-ent))
        (loop for address in (host-ent-addresses host-ent) do
          (e. tw |write| " ")
          (e. tw |print| (e. "." |rjoin| (map 'vector #'princ-to-string address))))
        (e. tw |write| ":")
        (e. tw |print| port))
      #+openmcl
      (with-slots (ip-number port) this
        (e. tw |write| " ")
        (e. tw |print| (e. "." |rjoin| (ip4-number-to-vector ip-number)))
        (e. tw |write| ":")
        (e. tw |print| port))
      #+clisp
      (with-slots (unresolved-name port) this
        (e. tw |write| "<network address ")
        (e. tw |print| unresolved-name)
        (e. tw |write| ":")
        (e. tw |print| port))
      (e. tw |write| ">"))))

(defun get-addr-info (host service hints)
  ;; XXX needs a failure ejector
  (e-coercef host '(or null string))
  (e-coercef service '(or null string))
  (e-coercef hints 'null)
  (e<- (efun () 
         (or 
          #+sbcl (make-instance 'pseudo-addr-info
                   :host-ent (if host
                               (get-host-by-name host)
                               ;; XXX this is probably all internal stuff
                               (make-instance 'sb-bsd-sockets:host-ent
                                 :name nil
                                 :type sockint::af-inet ;; XXX internal
                                 :aliases nil
                                 :addresses (list #(0 0 0 0))))
                   :port (if service (parse-integer service) 0))
                             
          #+clisp (make-instance 'pseudo-addr-info
                    :unresolved-name host
                    :port (if service (parse-integer service) 0))
          
          #+openmcl 
          (make-instance 'pseudo-addr-info
            :ip-number (if host (openmcl-socket:lookup-hostname host) nil)
            ;; XXX hints should be supported to choose "udp" instead
            :port (if (null service) 0
                    (handler-case
                        (parse-integer service)
                      (parse-error ()
                        (openmcl-socket:lookup-port service "tcp")))))
          
          (error "~A not implemented for this Lisp" 'get-addr-info)))
       |run|))

; --- ---

; XXX this vtable should be in the main system: streams might be used for non-sockets purposes.
(def-vtable stream
  #+clisp
  (:|read/3| (stream max-elements error-ejector eof-ejector)
    "Read up to 'max-octets' currently available octets from the FD, and return them as a ConstList. Blocks if read(2) would block."
    (let ((buf (make-array max-elements :element-type '(unsigned-byte 8) :fill-pointer t)))
      (setf (fill-pointer buf)
        (ext:read-byte-sequence buf stream :no-hang t))
      (if (and (zerop (fill-pointer buf))
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

(defgeneric stream-to-fd-ref (stream))

(defmethod stream-to-fd-ref ((stream synonym-stream))
  (stream-to-fd-ref (symbol-value (synonym-stream-symbol stream))))

#+sbcl
(defmethod stream-to-fd-ref ((stream sb-sys:fd-stream))
  (make-fd-ref (sb-sys:fd-stream-fd stream)))
  
#+clisp
(defmethod stream-to-fd-ref ((stream stream))
  stream)

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
