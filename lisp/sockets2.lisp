; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.sockets)

;;; Socket interface: second try.

(defglobal +peer-ref-guard+ (e-lambda "org.cubik.cle.socket.SocketPeerRef" ()
  (:|coerce| (specimen opt-ejector)
    (if (e-is-true (e. +the-audit-checker+ |run| +peer-ref-guard+ specimen))
      specimen
      (eject-or-ethrow opt-ejector (make-e-type-error specimen +peer-ref-guard+))))))

(defun make-socket-out-stream (our-socket impl-socket)
  (with-result-promise (stream)
    (multiple-value-bind (backend backend-resolver) (make-promise)
      (let* ((buffer (make-array 
                       (foo-sockopt-receive-buffer impl-socket) 
                       :element-type '(unsigned-byte 8)))
             ;; UNIX-WRITE doesn't like non-simple arrays
             (end-of-buffer 0))
        (flet ((wait-for-available ()
                 (with-result-promise (handler)
                   (vat-add-io-handler *vat* (%convert-handler-target impl-socket) :output (lambda (target)
                     (declare (ignore target))
                     (e. backend |setAvailable| (- (array-dimension buffer 0) end-of-buffer))
                     (vat-remove-io-handler handler)
                     (when (> end-of-buffer 0)
                       (e. stream |flush|)))))
                 (values)))
          (let ((impl
                 (e-lambda "$outImpl" ()
                   (:|__printOn| (tw)
                     (e-coercef tw +the-text-writer-guard+)
                     (e. tw |print| our-socket))
                   (:|write| (elements)
                     (e-coercef elements 'vector)
                     (let ((mark end-of-buffer))
                       (assert (< (length elements) (- (array-dimension buffer 0) mark)))
                       (incf end-of-buffer (length elements))
                       (replace buffer elements :start1 mark)))
                   (:|flush| ()
                     (escape-bind (write-error-ej)
                         (let ((n (foo-write impl-socket 
                                             buffer 
                                             write-error-ej 
                                             :end end-of-buffer)))
                           
                           (replace buffer buffer :start2 n)
                           (decf end-of-buffer n)
                           (wait-for-available))
                       (error)
                         (e. e.knot:+traceln+ |run| error)
                         (e. stream |fail| error))
                     nil)
                   (:|terminate| (terminator)
                     ;; XXX to do this right we need shutdown(2)
                     ;; for now we'll hope GC will clean up after us
                     nil))))
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

(defglobal +the-make-socket+ (e-lambda "org.cubik.cle.socket.makeSocket" (:stamped +deep-frozen-stamp+)
  (:|getInternet/0| (constantly ':internet))
  (:|getStream/0|   (constantly ':stream))
  (:|run| (domain type)
    (e-coercef domain '(member :internet))
    (e-coercef type '(member :stream))
    (with-result-promise (our-socket)
      (let* (did-bind-visible
             did-connect-visible
             (impl-socket (foo-make-socket domain type))
             (out-stream-slot
               (make-retriable-lazy-slot (lambda ()
                 (make-socket-out-stream our-socket impl-socket))))
             (in-stream-slot
               (make-retriable-lazy-slot (lambda ()
                 (make-socket-in-stream our-socket impl-socket)))))

        ;; XXX if the socket creation fails, it must be a vat-killing event unless authorized, as it is nondeterministic

        #+sbcl (setf (non-blocking-mode impl-socket) t)

        (e-lambda "$socket" ()
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
                  (make-unconnected-ref c)))
))
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
            (foo-sockopt-receive-buffer impl-socket))))))))

(defglobal +the-get-socket-peer-ref+ (e-lambda "org.cubik.cle.socket.getSocketPeerRef" ()
  (:|run| (host-spec service-spec)
    (e-coercef host-spec 'string)
    (e-coercef service-spec 'string)
    (e-lambda "$socketPeerRef" (:stamped +peer-ref-guard+)
      (:|__printOn| (tw)
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |write| "<socket peer ")
        (e. tw |print| host-spec)
        (e. tw |write| ":")
        (e. tw |print| service-spec)
        (e. tw |write| ">"))
      (:|getHostName| () host-spec)
      (:|getServiceName| () service-spec)))))
