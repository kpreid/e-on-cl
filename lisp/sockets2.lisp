; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.streams)

;;; Socket interface: second try.

(defobject +local-ref-guard+ "org.cubik.cle.socket.SocketLocalRef" ()
  (:|coerce| (specimen opt-ejector)
    (if (e-is-true (efuncall +the-audit-checker+ +local-ref-guard+ specimen))
      specimen
      (eject-or-ethrow opt-ejector (make-e-type-error specimen +local-ref-guard+)))))

(defobject +peer-ref-guard+ "org.cubik.cle.socket.SocketPeerRef" ()
  (:|coerce| (specimen opt-ejector)
    (if (e-is-true (efuncall +the-audit-checker+ +peer-ref-guard+ specimen))
      specimen
      (eject-or-ethrow opt-ejector (make-e-type-error specimen +peer-ref-guard+)))))

(defun make-socket-out-stream (our-socket impl-socket)
  (make-fd-out-stream our-socket
                      impl-socket #|XXX is this correct for non-sbcl?|#))

(defun eventual-write-simple-array (fd-ref array start end done closed failed)
  ;; XXX rewrite this in E
  (with-result-promise (handler)
    (vr-add-io-handler *vat* (%convert-handler-target fd-ref) :output (lambda (target)
      (declare (ignore target))
      (escape-bind (write-error-ej)
          (let ((n (e. fd-ref |write| array write-error-ej start end)))
            (cond
              ((>= n (- end start))
                (vr-remove-io-handler handler)
                (funcall done))
              ((zerop n)
                (vr-remove-io-handler handler)
                (funcall closed))
              (t
                (incf start n))))
        (error)
          (vr-remove-io-handler handler)
          (funcall failed error))))))

(defun make-fd-out-stream (name fd-ref)
  ;; XXX rewrite this in E (as in makeFDInStreamAuthor.emaker)
  ;; XXX review this for synchronous-information leaks
  (let* ((is-waiting nil)
         (reservations (make-instance 'queue))
         (t2 (multiple-value-list (make-promise)))
         (terminator (first t2))
         (terminator-resolver (second t2)))
    (labels ((shutdown ()
               (warn "got to shutdown") ;; debug
               (escape (ignore)
                 (e. fd-ref |shutdown| :output
                   (efun (condition)
                     (invoke-debugger condition) ;; debug
                     (setf condition (ref-shorten condition))
                     (typecase condition
                       ((or #+sbcl sb-bsd-sockets:not-connected-error)
                        nil)
                       (t 
                        (e. e.knot:+trace+ |run| (format nil "Error from attempted shutdown of ~A for writing at end of stream: ~A" (e-quote fd-ref) condition))))
                     (e. ignore |run|)))))
             (done-chunk ()
               (warn "~S done-chunk" name)
               (setf is-waiting nil)
               (check-reservations))
             (handle-chunk (chunk)
               (setf chunk (ref-shorten chunk))
               (warn "~S ~S handling chunk ~S" name fd-ref (type-of chunk))
               (etypecase chunk
                 (vector
                   (setf is-waiting t)
                   (eventual-write-simple-array
                      fd-ref
                      (coerce chunk '(simple-array (unsigned-byte 8) (*)))
                      0 (length chunk)
                      #'done-chunk
                      (lambda ()
                        (e. terminator-resolver |resolve| nil)
                        (done-chunk))
                      (lambda (error)
                        (e. terminator-resolver |resolve| (make-unconnected-ref error))
                        (done-chunk))))
                 (null
                   (e. terminator-resolver |resolve| nil)
                   (shutdown)
                   (done-chunk))
                 ((satisfies ref-opt-problem)
                   (e. terminator-resolver |resolve| chunk)
                   (shutdown)
                   (done-chunk))))
             (check-reservations ()
               (warn "~S check-reservations" name)
               (cond
                 (is-waiting)
                 ((ref-is-resolved terminator)
                   (loop for resolver = (dequeue reservations)
                         while resolver
                         do (e. resolver |resolve| terminator)))
                 (t 
                   (unless is-waiting
                     (let ((resolver (dequeue reservations)))
                       (when resolver
                         (setf is-waiting t)
                         (e. resolver |resolve|
                           (multiple-value-bind (chunk chunk-resolver) (make-promise)
                             (when-resolved (chunk) chunk
                               (handle-chunk chunk))
                             chunk-resolver)))))))))
      (e-lambda "$fdOutStream" ()
        (:|__printOn| ((out +the-text-writer-guard+))
          (e. out |write| "->")
          (e. out |printSame| name))
        (:|getChunkType| () (type-specifier-to-guard '(vector (unsigned-byte 8))))
        (:|flush| () nil)
        (:|terminates| () terminator)
        (:|reserve| ()
          (multiple-value-bind (reservation resolver-resolver) (make-promise)
            (enqueue reservations resolver-resolver)
            (check-reservations)
            reservation))))))

(defun make-socket-in-stream (our-socket impl-socket)
  (let ((make-fd-in-stream (efuncall (e-import "org.cubik.cle.io.makeFDInStreamAuthor") e.knot::+lisp+)))
    (efuncall make-fd-in-stream our-socket (socket-shorten impl-socket) (foo-sockopt-receive-buffer (socket-shorten impl-socket)))))

(defun make-retriable-lazy-slot (maker &aux value-box)
  "For lazy making that can fail and be retried later."
  (e-lambda "$retriableLazyApplySlot"
      ()
    (:|getValue| ()
      (first (or value-box (setf value-box (list (funcall maker))))))))

(defun make-socket-wrapper (impl-socket domain type &key did-bind-visible did-connect-visible)
  (with-result-promise (our-socket)
    (let* (listen-ok
           cleanups
           (out-stream-slot
             (make-retriable-lazy-slot (lambda ()
               (make-socket-out-stream our-socket impl-socket))))
           (in-stream-slot
             (make-retriable-lazy-slot (lambda ()
               (make-socket-in-stream our-socket impl-socket)))))

      ;; XXX if the socket creation fails, it must be a vat-killing event unless authorized, as it is nondeterministic

      #+sbcl (setf (non-blocking-mode impl-socket) t)

      (e-lambda "org.cubik.cle.socket.socket" ()
        (:|__printOn| ((tw +the-text-writer-guard+))
          (e. tw |write| "<")
          (e. tw |print| (string-downcase (symbol-name domain)))
          (e. tw |write| " ")
          (e. tw |print| (string-downcase (symbol-name type)))
          (e. tw |write| " socket")
          (flet ((x (visible label reader)
                   (when visible
                     (e. tw |write| label)
                     (handler-case
                       (multiple-value-bind (addr4 port)
                           (funcall reader impl-socket)
                          (e. tw |print| (e. "." |rjoin| (map 'vector #'princ-to-string addr4)))
                          (e. tw |write| ":")
                          (e. tw |print| port))
                       (socket-error (c)
                         ;; to be improved later
                         (e. tw |print| c))))))
            (x did-bind-visible " on " #'foo-socket-name)
            (x did-connect-visible " connected to " #'foo-socket-peername))
          (e. tw |write| ">"))
        
        (:|bind| ((local-ref +local-ref-guard+))
          (when-resolved (addr-info)
              (get-addr-info (e. local-ref |getHostName|)
                             (e. local-ref |getServiceName|)
                             nil)
            (setf addr-info (ref-shorten addr-info))
            ;; XXX handle non-resolved addr-info
            (setf did-bind-visible t
                  listen-ok t)
            (handler-case
              (progn
                (foo-bind-socket impl-socket addr-info)
                nil)
              (socket-error (c) 
                (make-unconnected-ref c)))))
        
        (:|close| (ejector)
          (loop while cleanups do (funcall (pop cleanups)))
          (e. impl-socket |close| ejector))
                
        (:|connect| ((peer-ref +peer-ref-guard+))
          "connect() this socket eventually. Resolves to nil for no error, true for EINPROGRESS, and a broken reference (XXX specify errno access) for all other errors."
          ;; XXX correct handling of EINTR
          (when-resolved (addr-info)
              (get-addr-info (e. peer-ref |getHostName|)
                             (e. peer-ref |getServiceName|)
                             nil)
            (setf addr-info (ref-shorten addr-info))
            ;; XXX handle non-resolved addr-info
            (setf did-connect-visible t) ; XXX details experimental
            (handler-case
              (progn
                (foo-connect-socket impl-socket addr-info)
                nil)
              ((satisfies in-progress-socket-error-p) (condition)
                (declare (ignore condition))
                +e-true+)
              (socket-error (c) 
                (make-unconnected-ref c)))))
        
        (:|listen| ((opt-backlog '(or null integer)) handler)
          (assert listen-ok)
          (foo-listen impl-socket opt-backlog)
          (let ((handler (foo-add-receive-handler impl-socket (efun ()
            (efuncall handler (make-socket-wrapper 
                                (foo-accept impl-socket nil)
                                domain
                                type
                                :did-bind-visible t
                                :did-connect-visible t))))))
            (push (lambda () (foo-remove-receive-handler handler)) cleanups))
          nil)
        
        (:|getOut| () (e. out-stream-slot |getValue|))
        (:|getIn|  () (e. in-stream-slot  |getValue|))
        
        (:|setSockoptSendBuffer| ((new '(integer 0)))
          (setf (foo-sockopt-send-buffer impl-socket) new))
        (:|setSockoptReceiveBuffer| ((new '(integer 0))) 
          (setf (foo-sockopt-receive-buffer impl-socket) new))
        
        (:|getSockoptSendBuffer/0| ()
          (foo-sockopt-send-buffer impl-socket))
        (:|getSockoptReceiveBuffer/0| ()
          (foo-sockopt-receive-buffer impl-socket))))))

(defobject +the-make-socket+ "org.cubik.cle.socket.makeSocket" (:stamped +deep-frozen-stamp+)
  (:|getInternet/0| (constantly ':internet))
  (:|getStream/0|   (constantly ':stream))
  (:|run| ((domain '(member :internet))
           (type '(member :stream)))
    (make-socket-wrapper (foo-make-socket domain type) domain type)))

(defun socket-address-ref-maker (stamp label)
  (e-lambda "org.cubik.cle.socket.getSocketAddressRef" ()
    (:|run| ((host-spec '(or null string))
             (service-spec '(or null string)))
      (e-lambda "$socketPeerRef" (:stamped stamp)
        (:|__printOn| ((tw +the-text-writer-guard+))
          (e. tw |write| "<socket ")
          (e. tw |print| label)
          (e. tw |write| " ")
          (e. tw |print| host-spec)
          (e. tw |write| ":")
          (e. tw |print| service-spec)
          (e. tw |write| ">"))
        (:|getHostName| () host-spec)
        (:|getServiceName| () service-spec)))))

(defglobal +the-get-socket-local-ref+ 
  (socket-address-ref-maker +local-ref-guard+ "local"))
(defglobal +the-get-socket-peer-ref+ 
  (socket-address-ref-maker +peer-ref-guard+ "peer"))

(defmacro with-ejection ((ejector) &body body &aux (condition (gensym)))
  ;; XXX review for more general use of this macro
  "Expose any CL:ERROR signaled in BODY via EJECTOR."
  `(handler-case
     (progn ,@body)
     (error (,condition) (eject-or-ethrow ,ejector ,condition))))

(defconstant +converted-stream-buffer-size+ 4096 #| XXX arbitrary |#)

;; NOTE: used by e.extern:+spawn+
(defun cl-to-eio-in-stream (stream name)
  (fd-ref-to-eio-in-stream (stream-to-fd-ref stream :input)
                           name
                           +converted-stream-buffer-size+))
                        

;; NOTE: used by e.extern:+spawn+
(defun cl-to-eio-out-stream (stream name)
  (make-fd-out-stream name
                      (stream-to-fd-ref stream :output)))
                      

(defun fd-ref-to-eio-in-stream (fd-ref name buffer)
  ;; XXX avoid reimporting the author
  (efuncall (efuncall (e-import "org.cubik.cle.io.makeFDInStreamAuthor")
                      e.knot::+lisp+) 
    name
    fd-ref
    buffer))

#+sbcl
(defun set-fd-non-blocking-mode (fd non-blocking-p)
  ;; XXX borrowed from sb-bsd-sockets misc.lisp, 2007-05-12. I'd rather use
  ;; something higher-level, but sb-bsd-sockets' code is restricted to the fds
  ;; it manages
  (declare (optimize (speed 3)))
  (let* ((arg1 (the (signed-byte 32) (sb-posix::fcntl fd sb-posix::f-getfl 0)))
         (arg2
          (if non-blocking-p
              (logior arg1 sb-posix::o-nonblock)
            (logand (lognot sb-posix::o-nonblock) arg1))))
    (when (= (the (signed-byte 32) -1)
             (the (signed-byte 32)
               (sb-posix::fcntl fd sb-posix::f-setfl arg2)))
      (socket-error "fcntl"))
    non-blocking-p))

(defobject +the-make-pipe+ "$makePipe" ()
  (:|run| (ejector)
    #-sbcl (error "makePipe not yet implemented for ~A" (lisp-implementation-type))
    (multiple-value-bind (read write)
        (with-ejection (ejector) 
          #+sbcl (sb-posix:pipe)
          #-sbcl nil)
      (set-fd-non-blocking-mode read t)
      (set-fd-non-blocking-mode write t)
      (vector
        (fd-ref-to-eio-in-stream (make-fd-ref read :close t) 
                                 (e-lambda "system pipe" ())
                                 4096)
        (make-fd-out-stream (e-lambda "system pipe" ())
                            (make-fd-ref write :close t))))))
