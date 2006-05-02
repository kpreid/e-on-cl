; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.sockets)

;;; Socket interface: second try.

(defglobal +local-ref-guard+ (e-lambda "org.cubik.cle.socket.SocketLocalRef" ()
  (:|coerce| (specimen opt-ejector)
    (if (e-is-true (e. +the-audit-checker+ |run| +local-ref-guard+ specimen))
      specimen
      (eject-or-ethrow opt-ejector (make-e-type-error specimen +local-ref-guard+))))))

(defglobal +peer-ref-guard+ (e-lambda "org.cubik.cle.socket.SocketPeerRef" ()
  (:|coerce| (specimen opt-ejector)
    (if (e-is-true (e. +the-audit-checker+ |run| +peer-ref-guard+ specimen))
      specimen
      (eject-or-ethrow opt-ejector (make-e-type-error specimen +peer-ref-guard+))))))

(defun make-socket-out-stream (our-socket impl-socket)
  (make-fd-out-stream our-socket
                      impl-socket ;; XXX is this correct for non-sbcl?
                      (foo-sockopt-receive-buffer impl-socket)))

(defun make-fd-out-stream (name fd-ref buffer-size)
  (with-result-promise (stream)
    (multiple-value-bind (backend backend-resolver) (make-promise)
      (let* ((buffer (make-array 
                       buffer-size 
                       :element-type '(unsigned-byte 8)))
             ;; UNIX-WRITE doesn't like non-simple arrays
             (end-of-buffer 0)
             (should-shutdown nil)
             (is-waiting))
        (labels ((wait-for-available ()
                   (unless is-waiting
                     (with-result-promise (handler)
                       (setf is-waiting t)
                       (vr-add-io-handler *vat* (%convert-handler-target fd-ref) :output (lambda (target)
                         (declare (ignore target))
                         (vr-remove-io-handler handler)
                         (setf is-waiting nil)
                         (e<- backend |setAvailable| (- (array-dimension buffer 0) end-of-buffer))
                         (when should-shutdown
                           ;; doing this at a time when we don't have a
                           ;; handler installed
                           ;; XXX this looks wrong - not giving the buffer a
                           ;; chance to be emptied - but we haven't proven it
                           ;; wrong yet
                           (e. fd-ref |shutdown| :output nil))
                         (when (> end-of-buffer 0)
                           (flush))))))
                   (values))
                 (flush ()
                   (escape-bind (write-error-ej)
                         (let ((n (e. fd-ref |write|
                                    buffer 
                                    write-error-ej 
                                    0 end-of-buffer)))
                           
                           (replace buffer buffer :start2 n)
                           (decf end-of-buffer n)
                           (wait-for-available))
                       (error)
                         #+(or) (e. e.knot:+trace+ |run| error)
                         (unless (ref-is-resolved (e. stream |terminates|))
                           (e. stream |fail| error)))))
          (let ((impl
                 (e-lambda "$outImpl" ()
                   (:|__printOn| (tw)
                     (e-coercef tw +the-text-writer-guard+)
                     (e. tw |print| name))
                   (:|write| (elements)
                     (e-coercef elements 'vector)
                     (let ((mark end-of-buffer))
                       (assert (<= (length elements) (- (array-dimension buffer 0) mark)))
                       (incf end-of-buffer (length elements))
                       (replace buffer elements :start1 mark)))
                   (:|flush| ()
                     (flush)
                     nil)
                   (:|terminate| (terminator)
                     (declare (ignore terminator))
                     (setf should-shutdown t)
                     (flush)))))
            (wait-for-available)
            (e. (e-import "org.erights.e.elib.eio.makeOutStreamShell")
                |run|
                '(unsigned-byte 8)
                backend-resolver
                impl)))))))

(defun make-socket-in-stream (our-socket impl-socket)
  (let ((make-fd-in-stream (e. (e-import "org.cubik.cle.io.makeFDInStreamAuthor") |run| e.knot::+lisp+)))
    (e. make-fd-in-stream |run| our-socket (socket-shorten impl-socket) (foo-sockopt-receive-buffer (socket-shorten impl-socket)))))

(defun make-retriable-lazy-slot (maker &aux value-box)
  "For lazy making that can fail and be retried later."
  (e-lambda "$retriableLazyApplySlot"
      ()
    (:|getValue| ()
      (first (or value-box (setf value-box (list (funcall maker))))))))

(defun make-socket-wrapper (impl-socket domain type &key did-bind-visible did-connect-visible)
  (with-result-promise (our-socket)
    (let* (listen-ok
           (out-stream-slot
             (make-retriable-lazy-slot (lambda ()
               (make-socket-out-stream our-socket impl-socket))))
           (in-stream-slot
             (make-retriable-lazy-slot (lambda ()
               (make-socket-in-stream our-socket impl-socket)))))

      ;; XXX if the socket creation fails, it must be a vat-killing event unless authorized, as it is nondeterministic

      #+sbcl (setf (non-blocking-mode impl-socket) t)

      (e-lambda "org.cubik.cle.socket.socket" ()
        (:|__printOn| (tw)
          (e-coercef tw +the-text-writer-guard+)
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
        
        (:|bind| (local-ref)
          (e-coercef local-ref +local-ref-guard+)
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
        
        (:|connect| (peer-ref)
          "connect() this socket eventually. Resolves to nil for no error, true for EINPROGRESS, and a broken reference (XXX specify errno access) for all other errors."
          (e-coercef peer-ref +peer-ref-guard+)
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
        
        (:|listen| (opt-backlog handler)
          (assert listen-ok)
          (e-coercef opt-backlog '(or null integer))
          (foo-listen impl-socket opt-backlog)
          (foo-add-receive-handler impl-socket (efun ()
            (e. handler |run| (make-socket-wrapper (foo-accept impl-socket nil)
                                                   domain
                                                   type
                                                   :did-bind-visible t
                                                   :did-connect-visible t))))
          nil)
        
        (:|getOut| () (e. out-stream-slot |getValue|))
        (:|getIn|  () (e. in-stream-slot  |getValue|))
        
        (:|setSockoptSendBuffer| (new)
          (e-coercef new '(integer 0))
          (setf (foo-sockopt-send-buffer impl-socket) new))
        (:|setSockoptReceiveBuffer| (new) 
          (e-coercef new '(integer 0))
          (setf (foo-sockopt-receive-buffer impl-socket) new))
        
        (:|getSockoptSendBuffer/0| ()
          (foo-sockopt-send-buffer impl-socket))
        (:|getSockoptReceiveBuffer/0| ()
          (foo-sockopt-receive-buffer impl-socket))))))

(defglobal +the-make-socket+ (e-lambda "org.cubik.cle.socket.makeSocket" (:stamped +deep-frozen-stamp+)
  (:|getInternet/0| (constantly ':internet))
  (:|getStream/0|   (constantly ':stream))
  (:|run| (domain type)
    (e-coercef domain '(member :internet))
    (e-coercef type '(member :stream))
    (make-socket-wrapper (foo-make-socket domain type) domain type))))

(defun socket-address-ref-maker (stamp label)
  (e-lambda "org.cubik.cle.socket.getSocketAddressRef" ()
    (:|run| (host-spec service-spec)
      (e-coercef host-spec '(or null string))
      (e-coercef service-spec '(or null string))
      (e-lambda "$socketPeerRef" (:stamped stamp)
        (:|__printOn| (tw)
          (e-coercef tw +the-text-writer-guard+)
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

;; NOTE: used by e.extern:+spawn+
(defun cl-to-eio-in-stream (stream name)
  (fd-to-eio-in-stream (sb-sys:fd-stream-fd stream) name 4096 #| XXX arbitrary buffer size |#))

;; NOTE: used by e.extern:+spawn+
(defun cl-to-eio-out-stream (stream name)
  (make-fd-out-stream name (make-fd-ref (sb-sys:fd-stream-fd stream)) 4096))

(defun fd-to-eio-in-stream (fd name buffer)
  (e. (e. (e-import "org.cubik.cle.io.makeFDInStreamAuthor")
          |run|
          e.knot::+lisp+) 
      |run| 
      name
      (make-fd-ref fd)
      buffer))

;; XXX this is not really about sockets
(defglobal +the-make-pipe+ (e-lambda "$makePipe" ()
  (:|run| (ejector)
    #-sbcl (error "makePipe not yet implemented for ~A" (lisp-implementation-type))
    (multiple-value-bind (read write)
        (with-ejection (ejector) 
          #+sbcl (sb-posix:pipe)
          #-sbcl nil)
      ;; XXX arrange for finalization of streams to close the fds
      ;; XXX set nonblocking
      (vector
        (fd-to-eio-in-stream read (e-lambda "system pipe" ()) 4096)
        (make-fd-out-stream (e-lambda "system pipe" ())
                            (make-fd-ref write)
                            4096))))))
