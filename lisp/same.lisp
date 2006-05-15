; This Common Lisp code, insofar as it is original work, is
; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license.
; It is partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.elib.same-impl
  (:use :cl :e.elib))

(in-package :e.elib.same-impl)

(defconstant +hash-depth+ 5)

(defun identity-hash (target) (sxhash target))

(defun sameness-fringe (original opt-fringe &optional (sofar (make-hash-table)))
  "returns cl:boolean"
  ; XXX translation of Java
  ;(declare (optimize (speed 3) (safety 3)))
  (when (nth-value 1 (gethash original sofar))
    (return-from sameness-fringe t))
  (let ((obj (ref-shorten original)))
    (when (nth-value 1 (gethash obj sofar))
      (return-from sameness-fringe t))
    (when (transparent-selfless-p obj)
      (setf (gethash original sofar) nil)
      (return-from sameness-fringe
        (loop
          with result = t
          for elem across (spread-uncall obj)
          do (setf result (and result (sameness-fringe elem opt-fringe sofar)))
             (when (and (not result) (null opt-fringe))
               (return nil))
          finally (return result))))
    (if (ref-is-resolved obj)
      (progn
        t)
      (progn
        (when opt-fringe
          (setf (gethash obj opt-fringe) nil))
        nil))))

(declaim (inline transparent-selfless-p))
(defun transparent-selfless-p (a)
  (approvedp +selfless-stamp+ a))

(defun spread-uncall (a)
  "assumes the object produces a properly formed uncall"
  (let ((unspread (e. a |__optUncall|)))
    (e-coercef unspread 'vector)
    (concatenate 'vector `#(,(aref unspread 0) ,(aref unspread 1)) (aref unspread 2))))

(defmethod elib::samep-dispatch (left right)
  (assert (not (transparent-selfless-p left)))
  (assert (not (transparent-selfless-p right)))
  (assert (not (eql left right)))
  nil)

(defun %sameness-hash (target hash-depth opt-fringe)
  ;(declare (optimize (speed 3) (safety 3)))
  (setf target (ref-shorten target))
  (if (<= hash-depth 0)
    (return-from %sameness-hash (cond
      ((sameness-fringe target opt-fringe)
        -1)
      ((null opt-fringe)
        (error "Must be settled"))
      (t
        -1))))
  (if (transparent-selfless-p target)
    (reduce #'logxor 
      (loop for a across (spread-uncall target)
            collect (%sameness-hash a (1- hash-depth) opt-fringe)))
    (let ((hash (elib::same-hash-dispatch target)))
      (cond
        (hash
          hash)
        ((ref-is-resolved target)
          ; Selfless objects were caught earlier. Any settled non-Selfless reference has the identity of its underlying object.
          (identity-hash target))
        ((null opt-fringe)
          (error "Must be settled"))
        (t
          ; target is an unresolved promise. Our caller will take its hash into account via opt-fringe.
          (setf (gethash target opt-fringe) +e-false+)
          -1)))))

(defmethod elib::same-hash-dispatch ((a null))
  (declare (ignore a))
  0)
  
(defmethod elib::same-hash-dispatch (a)
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

(defmacro sort-two-by-hash (a b)
  "not multiple-evaluation safe"
  `(when (< (sxhash ,b) 
            (sxhash ,a))
     (setf ,a ,b 
           ,b ,a)))

;; xxx *Currently*, the equalizer being returned from a maker function is left over from when equalizers used buffer vectors. Future changes, such as parameterization, might make the maker useful again later so I haven't bothered to change it.

(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defun make-equalizer () 
    (declare (optimize (speed 3) (safety 3) (debug 0)))
    (labels  ((push-sofar (left right sofar)
                (declare (type (cons list list) sofar))
                (sort-two-by-hash left right)
                (cons (cons left  (car sofar))
                      (cons right (cdr sofar))))
              
              (find-sofar (left right sofar)
                (declare (type (cons list list) sofar))
                (sort-two-by-hash left right)
                (some (lambda (sofar-left sofar-right)
                        (and (eql left  sofar-left)
                             (eql right sofar-right)))
                      (car sofar)
                      (cdr sofar)))
  
              (opt-same-spread (left right sofar)
                "returns e-boolean or nil"
                (declare (type (cons list list) sofar))
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
                (declare (type (cons list list) sofar))
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
                  (let ((left-selfless (transparent-selfless-p left))
                        (right-selfless (transparent-selfless-p right)))
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
                        (equalizer-trace "exit opt-same via samep-dispatch to come")
                        (as-e-boolean (elib::samep-dispatch left right))))))))
      
      (nest-fq-name ("org.erights.e.elib.tables.makeEqualizer")
        (e-lambda |equalizer|
            (:stamped +deep-frozen-stamp+)
          (:|sameEver| (left right)
            (let ((result (e. |equalizer| |optSame| left right)))
              (if result
                result
                (error 'insufficiently-settled-error :values (list left right)))))
        
          (:|sameYet| (left right)
            (or (e. |equalizer| |optSame| left right) +e-false+))
        
          (:|optSame| (left right)
            (opt-same left right '(() . ())))
          
          (:|isSettled| (ref)
            (as-e-boolean (settledp ref)))
          
          (:|isSelfless| (ref)
            "Returns whether the reference is selfless; that is, whether it can be acquired by construction as well as by passing. Such references may or may not also be transparent."
            (as-e-boolean (typep ref
              '(or (satisfies transparent-selfless-p)
                   ;; XXX derive this list from the def-atomic-sameness
                   ;; definitions instead of hardcoding
                   null
                   string
                   character
                   integer
                   float64
                   elib:e-boolean))))
          
          (:|makeTraversalKey/1| 'make-traversal-key))))))

;;; --- TraversalKey ---

(defun same-yet-hash (target fringe)
  "Used by TraversalKey implementation. Returns a hash value which corresponds to the equality test ELIB:SAME-YET-P mixed with the fringe's hashes."
  (let ((result (%sameness-hash target +hash-depth+ fringe)))
    (loop for prom being each hash-key of fringe do
        (setf result (logxor result (identity-hash prom))))
    result))

(defclass traversal-key (vat-checking) 
  ((wrapped :initarg :wrapped
            :accessor tk-wrapped)
   (snap-hash :initarg :snap-hash
              :accessor tk-snap-hash
              :type fixnum)
   (fringe :initarg :fringe
           :accessor tk-fringe
           :type hash-table)))
   
(defun make-traversal-key (target)
  (let ((wrapped (ref-shorten target))
        (fringe (make-hash-table :test #'eql)))
    (make-instance 'traversal-key
      :wrapped wrapped
      :fringe fringe
      :snap-hash (same-yet-hash wrapped fringe))))

#+(or) ;; Unused now that the Equalizer supplies TraversalKeys.
(defglobal +the-make-traversal-key+ 
  (let ((traversal-key-guard (make-instance 'cl-type-guard 
                               :type-specifier 'traversal-key)))
    (e-lambda "org.cubik.cle.prim.makeTraversalKey" ()
      (:|asType| () traversal-key-guard)
      (:|run/1| 'make-traversal-key))))

(def-atomic-sameness traversal-key
  (lambda (a b)
    (and (eql (tk-snap-hash a)
              (tk-snap-hash b))
         (same-yet-p (tk-wrapped a)
                          (tk-wrapped b))
         (eql (hash-table-count (tk-fringe a))
              (hash-table-count (tk-fringe b)))
         (loop for aelem being each hash-key of (tk-fringe a)
               always (nth-value 1 (gethash aelem (tk-fringe b))))))
  tk-snap-hash)

(def-vtable traversal-key
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print|
      "<key:"
      (tk-wrapped this)
      ">")))

(defmethod print-object ((tk traversal-key) stream)
  "solely for debugging"
  (print-unreadable-object (tk stream :type t :identity nil)
    (format stream "on ~W" (tk-wrapped tk))))

;;; --- XXX name this section ---

(defvar *the-equalizer* (make-equalizer))

(defun same-yet-p  (a b)
  (e-is-true (e. *the-equalizer* |sameYet| a b)))
(defun samep (a b)
  (e-is-true (e. *the-equalizer* |sameEver| a b)))

(defun same-hash (target)
  "Return a hash value which corresponds to the equality test ELIB:SAMEP."
  (%sameness-hash target +hash-depth+ nil))

(defun settledp (a)
  (sameness-fringe a nil))
