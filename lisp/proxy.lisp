; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defclass handler-ref (ref)
  ((handler        :initarg :handler
                   :reader %proxy-ref-handler)
   (resolution-box :initarg :resolution-box
                   :reader %proxy-ref-resolution-box))
  (:documentation "A reference which has a handler and resolution promise, but doesn't necessarily use them for anything."))

(defclass resolved-handler-ref (handler-ref) 
  ((resolution-identity :reader %resolution-identity
                        :writer set-resolution-identity
                        :type traversal-key))
  (:documentation "A reference which gets its identity from a handler and resolution promise's key; i.e. a far ref or disconnected ref."))

(defmethod initialize-instance :after ((ref resolved-handler-ref) &rest initargs)
  (declare (ignore initargs))
  (set-resolution-identity
     (make-traversal-key (%proxy-ref-resolution-box ref))
     ref))

;; NOTE: this is magic in that proxies are considered Selfless whereas no other kinds of eventual references are; implemented in elib::selflessp.
(def-atomic-sameness resolved-handler-ref
  (lambda (a b) (and (eql (class-of a)
                          (class-of b))
                     (samep (%proxy-ref-handler a)
                            (%proxy-ref-handler b))
                     (samep (%resolution-identity a)
                            (%resolution-identity b))))
  (lambda (a) (same-hash (%proxy-ref-handler a))))

(defclass disconnected-ref (broken-ref resolved-handler-ref)
  ())

(defclass proxy-ref (handler-ref)
  ()
  (:documentation "A Ref backed by a ProxyHandler."))

(defun bogus-resolution (&rest spec)
  (let ((condition (apply #'to-condition 'simple-error spec)))
    (e. e.knot:+sys-trace+ |run| condition)
    (make-unconnected-ref (transform-condition-for-e-catch condition))))

(defun verify-resolution-box (ref box)
  (handler-case 
    (progn
      (setf box (ref-shorten box))
      (check-type box e-simple-slot)
      (e. box |get|))
    (error ()
      (bogus-resolution "Resolution promise of a proxy handled by ~A didn't resolve to a simple slot, but ~A."
        (e-quote (%proxy-ref-handler ref))
        (e-quote box)))))

(defun verify-broken-resolution (ref resolution-ref)
  (if (eql (ref-state resolution-ref) 'broken)
    resolution-ref
    (bogus-resolution "Attempt to resolve a Far ref handled by ~A to another identity (~A)." 
      (e-quote (%proxy-ref-handler ref)) 
      (e-quote resolution-ref))))

(defgeneric %resolve-proxy (proxy resolution))

(defun %maybe-resolve-proxy (ref)
  "Convert this proxy-ref into its resolution, if it has resolved further. Returns whether it did anything."
  (with-ref-transition-invariants (ref)
    (check-type ref proxy-ref)
    (let ((resolution-box (%proxy-ref-resolution-box ref)))
      (when (ref-is-resolved resolution-box)
        ;; If the resolution-box is resolved, it contains the resolution of this reference.
        (%resolve-proxy ref (verify-resolution-box ref resolution-box))
        ;; Inform the caller of the need to re-dispatch.
        t))))

(defmethod %ref-shorten ((ref proxy-ref))
  (if (%maybe-resolve-proxy ref)
    (ref-shorten ref)
    ref))

(defmethod ref-state :around ((ref proxy-ref))
  (if (%maybe-resolve-proxy ref)
    (ref-state ref)
    (call-next-method)))

(defmethod ref-state ((ref proxy-ref))
  (error "ref-state not overridden for a proxy-ref"))

(defmethod ref-opt-sealed-dispatch ((ref proxy-ref) brand)
  (if (%maybe-resolve-proxy ref)
    (apply #'ref-opt-sealed-dispatch ref brand)
    (e. (%proxy-ref-handler ref) |handleOptSealedDispatch| brand)))

(defmethod e-call-dispatch ((ref proxy-ref) mverb &rest args)
  (if (%maybe-resolve-proxy ref)
    (apply #'e-call-dispatch ref mverb args)
    (error 'synchronous-call-error :recipient ref :mverb mverb :args args)))

(defmethod e-send-dispatch ((ref proxy-ref) mverb &rest args)
  (if (%maybe-resolve-proxy ref)
    (apply #'e-send-dispatch ref mverb args)
    (e<- (%proxy-ref-handler ref) |handleSend| (e-util:unmangle-verb mverb) (coerce args 'vector))))

(defmethod e-send-only-dispatch ((ref proxy-ref) mverb &rest args)
  (if (%maybe-resolve-proxy ref)
    (apply #'e-send-only-dispatch ref mverb args)
    (e-send-only-dispatch (%proxy-ref-handler ref) :|handleSendOnly/2| (e-util:unmangle-verb mverb) (coerce args 'vector)))
  nil)

(defmethod weak-when-more-resolved ((ref proxy-ref) weak-reactor action)
  (declare (ignore weak-reactor action))
  ;; XXX implement this to invoke the handler. until then, multivat 
  ;; tail-recursive loops may leak resolved-refs.
  (values))


(defclass far-ref (proxy-ref resolved-handler-ref)
  ())

(defmethod %resolve-proxy ((proxy far-ref) resolution)
  "When resolving a far-ref, its identity must be preserved, and it can only become disconnected (broken); attempting to do otherwise is an error."
  (change-class proxy 'disconnected-ref
    :problem (ref-opt-problem (verify-broken-resolution proxy resolution))))

(defmethod ref-state ((ref far-ref))
  (values 'eventual t))

;; XXX remote promises *should* have atomic sameness based on their handler and resolution, but the equalizer can't handle that yet. Not doing so merely results in remote promises not being same when they should be.
(defclass remote-promise (proxy-ref)
  ())

(defmethod ref-state ((ref remote-promise))
  (values 'eventual nil))

(defmethod %resolve-proxy ((proxy remote-promise) resolution)
  "If we're a remote promise, then we can just become a forwarder."
  (change-class proxy 'resolved-ref :target resolution))


(defmethod ref-opt-sealed-dispatch ((ref disconnected-ref) brand)
  nil)


(defobject +the-make-proxy+ "org.erights.e.elib.ref.makeProxy"
    (:stamped +deep-frozen-stamp+)
  (:|run| (handler resolution-box resolved)
    (unless (settledp handler)
      (error 'not-settled-error :name "proxy handler" :value handler))
    (make-instance (if (e-is-true resolved) 'far-ref 'remote-promise)
        :handler handler
        :resolution-box resolution-box)))
