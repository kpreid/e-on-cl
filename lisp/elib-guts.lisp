; This Common Lisp code is Copyright 2005 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; Currently, this file is just defined as 'stuff moved from elib.lisp that doesn't actually need to be loaded before other files, so as to reduce dependencies requiring recompiling'.

; --- native-type guards ---

(def-vtable cl-type-guard
  (:|coerce/2| (guard specimen opt-ejector)
    (e-coerce-native specimen (cl-type-specifier guard) opt-ejector guard))
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (cl-type-simple-expr (cl-type-specifier this))))
  (:|getFQName/0| (this)
    (cl-type-fq-name (cl-type-specifier this)))
  (:|getTheTrivialValue/0| (this)
    (with-slots (ts trivial-box) this
      (car (or trivial-box
           (setf trivial-box (list (e-coerce
             (cond ; should we defmacro subtypecase?
               ((subtypep 'null ts) nil)
               ((subtypep ts 'integer) 0)
               ((subtypep ts 'float64) 0d0)
               ((subtypep ts 'float32) 0.0)
               ((subtypep ts 'e-boolean) +e-false+)
               ; should there be an ejector?
               (t (error "No trivial value available")))
             this)))))))
  (:|getDocComment/0| (this)
    (with-slots (ts) this
      (let ((documentation (documentation ts 'type)))
        ; The CONS case is a workaround for an apparent bug in OpenMCL 0.14.2-p1.
        ; If not for that, this would be (or documentation "").
        (typecase documentation
          (string documentation)
          (cons   (first documentation))
          (null   "")))))
  (:|getSupers/0| (this) 
    "Supertype information is not currently available for primitive types."
    (declare (ignore this))
    #())
  (:|getAuditors/0| (this)
    (declare (ignore this))
    #())
  (:|getMessageTypes/0| (this)
    ; xxx this is a bit inefficient
    (with-slots (ts) this
      (message-pairs-to-map-including-miranda-messages (vtable-message-types ts)))))

(def-atomic-sameness cl-type-guard
  (lambda (a b) (equal (cl-type-specifier a)
                       (cl-type-specifier b)))
  (lambda (a)   (sxhash (cl-type-specifier a))))

(defmethod e-call-match ((rec cl-type-guard) mverb &rest args)
  (with-slots (ts super) rec
    ; XXX we could make super initialized more lazily
    ; XXX super needs to be per-thread
    (unless super
      (setf super (e. (e. (vat-safe-scope *vat*) |get| "__makeGuard") |run| rec)))
    (if (eql ts 't) ; XXX bad OO
      (cond
        ((and (not (eql mverb :|get/0|))
              (eql mverb (e-util:mangle-verb "get" (length args))))
          (e. (e. super |_getMakeUnionGuard|) |run| (coerce args 'vector)))
        ((and (eql mverb :|__respondsTo/2|) 
              (string= (aref args 0) "get"))
          +e-true+)
        (t
          (apply #'e-call-dispatch super mverb args)))
      (apply #'e-call-dispatch super mverb args))))

(defmethod e-audit-check-dispatch ((auditor (eql +deep-frozen-stamp+)) (specimen cl-type-guard))
  ; NOTE: assuming that super behaves deep-frozen
  t)


; --- Auditing ---

(defmethod e-audit-check-dispatch ((auditor t) (specimen t))
  nil)

(defvar +the-audit-checker+ (e-named-lambda "org.erights.e.elib.slot.auditChecker"
  :stamped +deep-frozen-stamp+
  (:|__printOn/1| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "__auditedBy")) ; XXX move to e.syntax?
  (:|run/2| (auditor specimen)
    (as-e-boolean (e-audit-check-dispatch auditor specimen)))))

; --- local resolver ---

(declaim (inline promise-ref-resolve))
(defun promise-ref-resolve (ref new-target
    &aux (buffer (slot-value ref 'buffer)))
  (declare (type promise-ref ref)
           (type (vector list) buffer))
  (setf new-target (ref-shorten new-target))
  (with-ref-transition-invariants (ref)
    (if (eq new-target ref)
      (change-class ref 'unconnected-ref :problem (make-condition 'vicious-cycle-error))
      (change-class ref 'forwarding-ref :target new-target)))
  
  ; after change-class, the buffer has been dropped by the ref
  ; we could optimize the case of just forwarding many messages to the target, for when the target is another promise, but don't yet
  (loop for (resolver mverb args) across buffer do
    (e. resolver |resolve| (apply #'e-send-dispatch new-target mverb args))))

(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defun resolve-race (this target)
    "Internal function used by the local-resolver vtable."
    (with-slots (ref) this
      (if (null ref)
        +e-false+
        (progn
          (promise-ref-resolve ref target)
          (setf ref nil)
          +e-true+)))))

(def-vtable local-resolver
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (if (slot-value this 'ref)
                     "<Resolver>"
                     "<Closed Resolver>")))
  ; XXX resolve/2
  (:|resolveRace/1| #'resolve-race)
  (:|resolve/1| (this target)
    (unless (e-is-true (resolve-race this target))
      (error "already resolved")))
  (:|smash/1| (this problem)
    "Equivalent to this.resolve(Ref.broken(problem))."
    ; XXX the doc comment is accurate, and specifies the appropriate behavior, but it does so by performing the same operation as Ref.broken()'s implementation in a separate implementation. Both occurrences should probably be routed through the exception-semantics section in base.lisp.
    (unless (resolve-race this (elib:make-unconnected-ref (e-coerce problem 'condition)))
      (error "Already resolved")))
  (:|isDone/0| (this)
    "Returns whether this resolver's promise has been resolved already."
    (as-e-boolean (not (slot-value this 'ref)))))

; --- e-boolean ---

(def-vtable e-boolean
  (:|__printOn/1| (this tw) ; XXX move to e.syntax?
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (if (e-is-true this)
                     "true"
                     "false")))
  (:|and/1| (this other)
    "Boolean and."
    (e-coercef other 'e-boolean)
    (if (e-is-true this) other +e-false+))
  (:|not/0| (this)
    "Boolean negation."
    (if (eql this +e-false+) +e-true+ +e-false+))
  (:|or/1| (this other)
    "Boolean or."
    (e-coercef other 'e-boolean)
    (if (e-is-true this) +e-true+ other))
  (:|pick/2| (this true-value false-value)
    "Return the first argument if this is true, otherwise return the second argument."
    (if (e-is-true this) true-value false-value))
  (:|xor/1| (this other)
    "Boolean exclusive or."
    (e-coercef other 'e-boolean)
    (if (e-is-true this) (e. other |not|) other)))

(defmethod e-audit-check-dispatch ((auditor (eql +deep-frozen-stamp+)) (specimen e-boolean))
  t)

; --- Ejector ---

(flet ((ejector-throw (this &optional value)
         (ejector-prethrow (list 'ejector this) value)
         (handler-case (throw (slot-value this 'catch-tag) value)
           ; xxx should define vtable for cant-throw-error instead of making a new condition?
           ;     (of course, we must make a new condition if the implementation doesn't have such a distinct condition type)
           (#+ccl ccl::cant-throw-error #-ccl t
             ()
             (error "ejector ~S no longer in scope" (slot-value this 'label))))))
             
  (def-vtable ejector
    (:|__printOn/1| (this tw) 
      (e-coercef tw +the-text-writer-guard+)
      (with-slots (label) this
        (e. tw |print| "<" label " ejector>")))
    (:|run/0| #'ejector-throw)
    (:|run/1| #'ejector-throw)))

; --- Equalizer ---

(defconstant +eeq-hash-depth+ 5)

(defun eeq-same-yet-hash (target &optional fringe)
  "If fringe is nil (the default value), requires a settled target."
  (let ((result (eeq-internal-sameness-hash target +eeq-hash-depth+ fringe)))
    (when fringe
      (loop for prom being each hash-key of fringe do
        (setf result (logxor result (eeq-identity-hash prom)))))
    result))

(defun eeq-identity-hash (target) (sxhash target))

(defun eeq-sameness-fringe (original opt-fringe &optional (sofar (make-hash-table)))
  "returns cl:boolean"
  ; XXX translation of Java
  ;(declare (optimize (speed 3) (safety 3)))
  (when (nth-value 1 (gethash original sofar))
    ;(format t "original in sofar: ~S" original)
    (return-from eeq-sameness-fringe t))
  (let ((obj (ref-shorten original)))
    (when (nth-value 1 (gethash obj sofar))
      ;(format t "shortened in sofar: ~S" obj)
      (return-from eeq-sameness-fringe t))
    (when (eeq-is-transparent-selfless obj)
      ;(format t "selfless: ~S" obj)
      (setf (gethash original sofar) nil)
      (return-from eeq-sameness-fringe
        (loop
          with result = t
          for elem across (spread-uncall obj)
          do (setf result (and result (eeq-sameness-fringe elem opt-fringe sofar)))
             (when (and (not result) (null opt-fringe))
               (return nil))
          finally (return result))))
    (if (ref-is-resolved obj)
      (progn
        ;(format t "resolved: ~S" obj)
        t)
      (progn
        ;(format t "fallthru: ~S" obj)
        (when opt-fringe
          (setf (gethash obj opt-fringe) nil))
        nil))))


; XXX consider replacing all eeq-is-transparent-selfless methods with audit stamps.
(defmethod eeq-is-transparent-selfless ((a t))
  (e-audit-check-dispatch +selfless-stamp+ a))

(defmethod eeq-is-transparent-selfless ((a vector))
  (declare (ignore a)) t)

(defmethod eeq-is-transparent-selfless ((a string))
  (declare (ignore a)) nil)

(defun spread-uncall (a)
  (let ((unspread (e. a |__optUncall|)))
    (assert unspread)
    (concatenate 'vector `#(,(aref unspread 0) ,(aref unspread 1)) (aref unspread 2))))

(defmethod eeq-same-dispatch (left right)
  (assert (not (eeq-is-transparent-selfless left)))
  (assert (not (eeq-is-transparent-selfless right)))
  (assert (not (eql left right)))
  nil)

(defun eeq-internal-sameness-hash (target hash-depth opt-fringe)
  ;(declare (optimize (speed 3) (safety 3)))
  (setf target (ref-shorten target))
  (if (<= hash-depth 0)
    (return-from eeq-internal-sameness-hash (cond
      ((eeq-sameness-fringe target opt-fringe)
        -1)
      ((null opt-fringe)
        (error "Must be settled"))
      (t
        -1))))
  (if (eeq-is-transparent-selfless target)
    (reduce #'logxor 
      (loop for a across (spread-uncall target)
            collect (eeq-internal-sameness-hash a (1- hash-depth) opt-fringe)))
    (let ((hash (eeq-hash-dispatch target)))
      (cond
        (hash
          hash)
        ((ref-is-resolved target)
          ; Selfless objects were caught earlier. Any settled non-Selfless reference has the identity of its underlying object.
          (eeq-identity-hash target))
        ((null opt-fringe)
          (error "Must be settled"))
        (t
          ; target is an unresolved promise. Our caller will take its hash into account via opt-fringe.
          (setf (gethash target opt-fringe) +e-false+)
          -1)))))

(defmethod eeq-hash-dispatch ((a null))
  (declare (ignore a))
  0)
  
(defmethod eeq-hash-dispatch (a)
  (declare (ignore a))
  nil)

#-(and) (declaim (inline equalizer-trace))
#-(and) (defun equalizer-trace (&rest args)
  ; XXX we should have a better, centralized trace system
  (format *trace-output* "~&; equalizer:")
  (apply #'format *trace-output* args)
  (fresh-line))

#+(and) (defmacro equalizer-trace (&rest arg-forms)
  (declare (ignore arg-forms))
  '(progn))

(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defun make-equalizer (
      &aux (lefts  (make-array 30 :element-type t :adjustable t :fill-pointer 0)) 
           (rights (make-array 30 :element-type t :adjustable t :fill-pointer 0))
           (equalizer-busy nil)
           (next-equalizer nil)) 
    (declare (optimize (speed 3) (safety 3) (debug 0))
             (type (and (vector t) (not simple-array) (not simple-vector)) lefts rights))
    (labels  ((clear () 
                (fill lefts  nil)
                (fill rights nil)
                (setf (fill-pointer lefts)  0
                      (fill-pointer rights) 0))
                
              (push-sofar (left right sofar)
                (declare (fixnum sofar))
                (when (< (sxhash right) 
                         (sxhash left))
                  (setf left  right
                        right left))
                (if (>= sofar (fill-pointer lefts))
                  (progn
                    (assert (= sofar (fill-pointer lefts)))
                    (vector-push-extend left  lefts)
                    (vector-push-extend right rights))
                  (setf (aref lefts sofar) left
                        (aref rights sofar) right))
                (let ((sofarther (1+ sofar)))
                  (declare (fixnum sofarther))
                  sofarther))
              
              (find-sofar (left right sofar)
                (declare (fixnum sofar))
                (when (< (sxhash right)
                         (sxhash left))
                  (setf left  right
                        right left))
                (loop for i below sofar
                      when (and (eql left  (aref lefts i))
                                (eql right (aref rights i)))
                        do (return-from find-sofar t))
                nil)
  
              (opt-same-spread (left right sofar)
                "returns e-boolean or nil"
                (declare (fixnum sofar))
                (equalizer-trace "enter opt-same-spread ~S ~S ~S" left right sofar)
                (let ((leftv  (spread-uncall left))
                      (rightv (spread-uncall right))
                      (not-different-result +e-true+))
                  (declare (simple-vector leftv rightv))
                  (when (/= (length leftv) (length rightv))
                    ; Early exit, and precondition for the following loop.
                    (equalizer-trace "exit opt-same-spread lengths ~S ~S" (length leftv) (right leftv))
                    (return-from opt-same-spread +e-false+))
                  (loop with sofarther = (push-sofar left right sofar)
                        for i below (length leftv)
                        for new-left = (aref leftv i)
                        for new-right = (aref rightv i)
                        for opt-result = (opt-same new-left new-right sofarther)
                        do (cond 
                             ; If we encountered an unsettled reference,
                             ; then we can never be sure of sameness
                             ; (return true), but we might later find out
                             ; it's different (return false), so instead
                             ; of returning nil now, we remember that 
                             ; our result should not be true.
                             ((null opt-result) 
                               (equalizer-trace "tagging as not-same at ~S ~S ~S" i new-left new-right)
                               (setf not-different-result nil))
                             ; Two same-index elements of the uncalls are
                             ; different, so the objects must be 
                             ; different.
                             ((eq opt-result +e-false+)
                               (equalizer-trace "exit opt-same-spread different at ~S ~S ~S" i new-left new-right)
                               (return-from opt-same-spread +e-false+))
                             (t
                               (equalizer-trace "opt-same-spread match at ~S ~S ~S" i new-left new-right))))
                  ; If we reach here, then no definite differences have
                  ; been found, so we return true or nil depending on
                  ; whether we encountered an unsettled reference.
                  (equalizer-trace "exit opt-same-spread with accumulated ~S" not-different-result)
                  not-different-result))
                  
              (opt-same (left right sofar)
                "returns e-boolean or nil"
                (declare (fixnum sofar))
                (equalizer-trace "enter opt-same ~S ~S ~S" left right sofar)
                ; XXX translation of Java - should be cleaned up to be less sequential
                (block nil
                  (when (eql left right)
                    ; Early exit: primitive reference equality.
                    (equalizer-trace "exit opt-same eql")
                    (return +e-true+))
                  (setf left (ref-shorten left))
                  (setf right (ref-shorten right))
                  (when (eql left right)
                    ; Equality past resolved forwarding refs. At this
                    ; point, we have caught all same Selfish objects.
                    (equalizer-trace "exit opt-same ref-eql")
                    (return +e-true+))
                  (unless (and (ref-is-resolved left) (ref-is-resolved right))
                    ; left and right are unresolved and not primitive-eql,
                    ; so they must be a promise and something else which
                    ; is not the same (possibly another promise).
                    (equalizer-trace "exit opt-same unresolved")
                    (return nil))
                  (when (find-sofar left right sofar)
                    ; This breaks cycles in equality comparison. Any
                    ; pair found by find-sofar has been checked at a
                    ; smaller recursion depth, and can be considered same
                    ; at this depth.
                    (equalizer-trace "exit opt-same sofar loop")
                    (return +e-true+))
                  (let ((left-selfless (eeq-is-transparent-selfless left))
                        (right-selfless (eeq-is-transparent-selfless right)))
                    (cond
                      ((and left-selfless right-selfless)
                        (opt-same-spread left right sofar))
                      ((or  left-selfless right-selfless)
                        ; Early exit: if one but not both are selfless,
                        ; they can't be the same.
                        (equalizer-trace "exit opt-same selflessness mismatch ~S ~S" left-selfless right-selfless)
                        +e-false+)
                      (t
                        ; this handles what in Java-E are HONORARY selfless
                        ; objects
                        (equalizer-trace "exit opt-same via eeq-same-dispatch to come")
                        (as-e-boolean (eeq-same-dispatch left right))))))))
      
      (with-result-promise (equalizer)
        (e-named-lambda "org.erights.e.elib.tables.makeEqualizer$equalizer"
          :stamped +deep-frozen-stamp+
          (:|sameEver| (left right)
            (let ((result (e. equalizer |optSame| left right)))
              (if result
                result
                (error 'insufficiently-settled-error :values (list left right)))))
        
          (:|sameYet| (left right)
            (or (e. equalizer |optSame| left right) +e-false+))
        
          (:|optSame| (left right)
            (if equalizer-busy
              (funcall (or next-equalizer
                         (setf next-equalizer (make-equalizer)))
                       :|optSame/2| left right)
              (progn
                (setf equalizer-busy t)
                (unwind-protect
                  (opt-same left right 0)
                  (clear)
                  (setf equalizer-busy nil))))))))))

; --- Type/FQN mapping miscellaneous ---

; - observable-type-of - 

(defmethod observable-type-of ((specimen ref)
    &aux (short (ref-shorten specimen)))
  "Ref-shortening case."
  (if (eql short specimen)
    't
    (observable-type-of short)))

(defmethod observable-type-of ((specimen t))
  (typecase specimen
    ; XXX we can't just specialize on float64/float32 because they aren't class names
    (float64 'float64)
    (float32 'float32)
    
    (otherwise (class-name (class-of specimen)))))

(defmethod observable-type-of ((a function))
  (declare (ignore a)) 
  't)

; - fqn/parameters base cases - 

(defmethod cl-type-fq-name ((type-sym t))
  (with-standard-io-syntax
    (format nil "org.cubik.cle.native.~A.~A" 
      (string-downcase (package-name (symbol-package type-sym)))
      (string-downcase (symbol-name type-sym)))))

(defmethod cl-type-parameters ((type-sym t) parameters)
  (if parameters
    (e-quote (coerce parameters 'vector))
    ""))

(defmethod cl-type-fq-name ((type cons))
  (cl-type-fq-name (car type)))

(defmethod cl-type-parameters ((type cons) non-parameters)
  (assert (null non-parameters))
  (cl-type-parameters (first type) (rest type)))

; - misc fqns - 

; XXX move these to near the relevant def-vtable, when possible
(def-fqn t "org.cubik.cle.prim.any")
(def-fqn nil "org.cubik.cle.prim.none")

; XXX this is rather odd, but we do want the 'any' print
(def-fqn and "org.cubik.cle.prim.types.all")
(def-fqn or "org.cubik.cle.prim.types.any")

(def-fqn vicious-cycle-error "org.erights.e.elib.ref.ViciousCycleException")
(def-fqn insufficiently-settled-error "org.erights.e.elib.tables.InsufficientlySettledException")
(def-fqn not-settled-error "org.erights.e.elib.tables.NotSettledException")

(def-fqn flex-list "org.erights.e.elib.tables.FlexList")
(def-fqn string "org.erights.e.elib.tables.String")
(def-fqn float64 "org.cubik.cle.prim.float64")
(def-fqn float32 "org.cubik.cle.prim.float32")
(def-fqn condition "org.cubik.cle.native.Throwable") ; xxx Java name
(def-fqn e-boolean "org.cubik.cle.native.boolean")
(def-fqn null "org.cubik.cle.prim.Null")
(def-fqn character "org.cubik.cle.prim.char")
(def-fqn vector "org.erights.e.elib.tables.ConstList")
(def-fqn type-desc "org.erights.e.elib.base.TypeDesc")
(def-fqn message-desc "org.erights.e.elib.base.MessageDesc")
(def-fqn param-desc "org.erights.e.elib.base.ParamDesc")
(def-fqn e-simple-slot "org.erights.e.elib.slot.FinalSlot") ; xxx 'Final'?
(def-fqn local-throw-sealed-box "org.cubik.cle.prim.LocalThrowSealedBox")

(loop for group-type in '(and or) do
  (defmethod cl-type-parameters ((type (eql group-type)) parameters)
    (with-output-to-string (stream)
      (format stream "[" #|]|#)
      (loop for sep = "" then ", "
            for x in parameters
            do (format stream "~A~A" sep (cl-type-simple-expr x)))
      (format stream #|[|# "]"))))

(defmethod cl-type-parameters ((type (eql 'vector)) parameters)
  (destructuring-bind (&optional (element-type 't) (length '*)) parameters
    (setf element-type 
      (upgraded-array-element-type
        (if (eql element-type '*) 
          't
          element-type)))
    (cond
      ((not (eql length '*))
        (format nil "[~A, ~A]" (cl-type-simple-expr element-type) length))
      ((not (eql element-type 't))
        (format nil "[~A]" (cl-type-simple-expr element-type)))
      (t 
        ""))))

(defun cl-type-simple-expr (type)
  (concatenate 'string (simplify-fq-name (cl-type-fq-name type)) (cl-type-parameters type nil)))
  
(defun cl-type-fq-expr (type)
  (concatenate 'string (cl-type-fq-name type) (cl-type-parameters type nil)))
  
; --- far refs/proxies ---

(defclass proxy-ref (ref)
  ((handler :initarg :handler))
  (:documentation "A Ref backed by a ProxyHandler."))

(defmethod ref-shorten ((ref proxy-ref))
  ref)

(defmethod ref-state ((ref proxy-ref))
  (error "ref-state not overridden for a proxy-ref"))

(defmethod e-call-dispatch ((ref proxy-ref) mverb &rest args)
  (declare (ignore mverb args))
  (error 'synchronous-call-error))

(defmethod e-send-dispatch ((ref proxy-ref) mverb &rest args)
  (with-slots (handler) ref
    (e<- handler |handleSendAll| (e-util:unmangle-verb mverb) (coerce args 'vector))))


(defclass far-ref (proxy-ref identified-ref)
  ())

(defmethod ref-state ((ref far-ref))
  (values 'eventual t))


(defclass remote-promise (proxy-ref) 
  ())

(defmethod ref-state ((ref remote-promise))
  (values 'eventual nil))


(defvar +the-make-proxy-resolver+ (e-named-lambda "org.erights.e.elib.ref.makeProxyResolver"
  (:|run/2| (opt-handler opt-identity &aux ref-slot)
    ; XXX ref-slot will eventually be a weak reference
    (unless opt-handler
      (error "null is not allowed as the handler"))
    (unless (eeq-is-settled opt-identity)
      (error 'not-settled-error :name "optIdentity" :value opt-identity))
    (e-named-lambda "org.erights.e.elib.ref.makeProxyResolver$proxyResolver"
      (:|__printOn/1| (tw)
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |print| "<Proxy Resolver>"))
      (:|getProxy/0| ()
        "Return the Ref for this resolver, creating a new one if necessary."
        (unless (and ref-slot (e. ref-slot |getValue|))
          (setf ref-slot (make-instance 'e-simple-slot :value
            (if opt-handler
              (if opt-identity
                (make-instance 'far-ref
                    :handler opt-handler
                    :identity opt-identity)
                (make-instance 'remote-promise
                    :handler opt-handler))
              (error "this ProxyResolver is resolved and therefore its proxy is not available")))))
        (e. ref-slot |getValue|))
      (:|smash/1| (problem)
        (unless opt-handler
          (error "already resolved"))
        (let ((ref (and ref-slot (e. ref-slot |getValue|))))
          (when ref
            (with-ref-transition-invariants (ref)
              (change-class ref
                (if opt-identity
                  'disconnected-ref
                  'unconnected-ref)
                :problem problem))))
        (let ((handler opt-handler))
          (setf opt-handler nil
                ref-slot nil)
          ; XXX should we catch any problems arising here?
          (e. handler |handleResolution| (make-unconnected-ref problem)))
        nil)))))

; --- sorted queue ---

(defmethod sorted-queue-peek ((q sorted-queue) absent-thunk)
  (with-slots (elements) q
    (if elements
      (first elements)
      (funcall absent-thunk))))

(defmethod sorted-queue-pop ((q sorted-queue))
  (with-slots (elements) q
    (if elements
      (pop elements)
      (error "empty queue"))))
      
(defmethod sorted-queue-snapshot ((q sorted-queue))
  (with-slots (elements) q
    (copy-list elements)))
    
(defmethod sorted-queue-put ((q sorted-queue) key value)
  (declare (integer key))
  (with-slots (elements) q
    ;XXX more efficient than linear?
    (if (or (null elements) (< key (car (first elements))))
      (push (cons key value) elements)
      (loop for prev = elements then (rest prev)
            while prev
            do (when (or (null (rest prev))
                         (< key (car (second prev))))
                 (push (cons key value) (rest prev))
                 (return))
            finally (error "fell off end of queue")))
    nil))

(defvar +the-make-sorted-queue+ (e-named-lambda "org.cubik.cle.prim.makeSortedQueue"
  (:|run| ()
    (make-instance 'sorted-queue))))

(def-vtable sorted-queue
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<sorted queue of " (length (slot-value this 'elements)) ">"))
  (:|peek/1| (this absent-thunk)
    (block nil
      (let ((p (sorted-queue-peek this (lambda () (return (e. absent-thunk |run|))))))
        (vector (car p) (cdr p)))))
  (:|pop/0| (this)
    (let ((p (sorted-queue-pop this)))
      (vector (car p) (cdr p))))
  (:|put/2| (this key value)
    (e-coercef key 'integer)
    (sorted-queue-put this key value))
  (:|asList/0| (this)
    (map 'vector 
         #'(lambda (c) (vector (car c) (cdr c)))
         (sorted-queue-snapshot this))))
