; This Common Lisp code, insofar as it is original work, is
; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license.
; It is partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(cl:defpackage :e.elib.same-impl
  (:use :cl :e.elib)
  (:import-from :e.elib :selflessp))

(in-package :e.elib.same-impl)

(defconstant +hash-depth+ 5)

(declaim (inline selfish-hash))
(defun selfish-hash (target) 
  #-e.function-sxhash-inadequate (sxhash target)
  #+e.function-sxhash-inadequate
  (or (when (functionp target)
        (funcall target 'e.elib:selfish-hash-magic-verb))
      (sxhash target)))

(declaim (inline selflessp spread))

(defun spread (portrayal)
  "assumes a properly formed portrayal, or nil"
  (setf portrayal (ref-shorten portrayal))
  (when portrayal
    (e-coercef portrayal 'vector)
    (concatenate 'vector (list (aref portrayal 0) 
                               (aref portrayal 1))
                         (aref portrayal 2))))

(defun selflessp (a)
  ;; NOTE: This is the implementation of the test for the Selfless guard.
  (or (approvedp +selfless+ a)
      ;; NOTE: special case for proxies
      ;; XXX should this instead be implemented by some mechanism for approvedp to dispatch on nonnear refs?
      (typep a 'e.elib::resolved-handler-ref)))

;; this ought to be with the GF definition, but can't be because def-shorten-methods isn't available there
(def-shorten-methods semitransparent-result-box-contents 1)

(declaim (inline semitransparent-opt-uncall))
(defun semitransparent-opt-uncall (specimen)
  ;; in a function because it's used by DeepFrozen too
  (when (approvedp +semitransparent-stamp+ specimen)
    (semitransparent-result-box-contents
      (e. specimen |__optSealedDispatch|
          +semitransparent-result-box-brand+))))

(declaim (inline opt-spread-equalizer-uncall))
(defun opt-spread-equalizer-uncall (specimen)
  "Given a Selfless reference, return the spread portrayal to use for comparing it, or nil if none is found."
  (cond ((approvedp +transparent-stamp+ specimen)
         (spread (e. specimen |__optUncall|)))
        ((spread (semitransparent-opt-uncall specimen)))))

(declaim (inline %examine-reference))
(defun %examine-reference (original path opt-fringe
                           &key 
                           (before-shortening (constantly nil))
                           early-test
                           (selfless-early (constantly nil))
                           transparent-seed
                           transparent-op
                           (transparent-shortcut (constantly nil))
                           nontransparent-op
                           when-resolved-selfish
                           when-unresolved-selfish)
  (funcall before-shortening original)
  (let ((short (ref-shorten original)))
    (funcall early-test short)
    (if (selflessp short)
      (progn
        (funcall selfless-early)
        (let ((spread-uncall (opt-spread-equalizer-uncall short)))
          (if spread-uncall
            (loop with result = transparent-seed
                  for a across spread-uncall
                  for i from 0
                  do (setf result (funcall transparent-op result a
                                    (when opt-fringe (cons i path))))
                     (locally
                       (declare #+sbcl (sb-ext:muffle-conditions
                                        sb-ext:code-deletion-note))
                       (when (funcall transparent-shortcut result)
                         (return result)))
                  finally (return result))
            (funcall nontransparent-op short path))))
      (if (ref-is-resolved short)
        (funcall when-resolved-selfish short)
        (progn
          (when opt-fringe
            ; target is an unresolved promise. Our caller will take its hash into account via opt-fringe.
            (vector-push-extend (cons short path) opt-fringe))
          (funcall when-unresolved-selfish))))))

(defun sameness-fringe (original path opt-fringe &optional (sofar (make-hash-table)))
  "Returns a cl:boolean indicating whether the given reference is settled; fills OPT-FRINGE if provided."
  ;(declare (optimize (speed 3) (safety 3)))
  (flet ((stop-on-cycle (obj)
           (when (nth-value 1 (gethash obj sofar))
             (return-from sameness-fringe t))))
    (%examine-reference
      original path opt-fringe
      :before-shortening #'stop-on-cycle
      :early-test        #'stop-on-cycle
      :selfless-early (lambda () (setf (gethash original sofar) nil))
      :transparent-seed t
      :transparent-op (lambda (accum elem path-ext)
                        (let ((sub-settled (sameness-fringe elem path-ext opt-fringe sofar)))
                          (and accum sub-settled)))
      :transparent-shortcut (lambda (result)
                              (and (not result) (null opt-fringe)))
      :nontransparent-op (lambda (short path)
                           (if (e.elib::same-hash-dispatch short)
                             t
                             (progn
                               ;; XXX duplication here of the unresolved-selfish logic
                               (when opt-fringe
                                 (vector-push-extend (cons short path) opt-fringe))
                               nil)))
      :when-resolved-selfish (constantly t)
      :when-unresolved-selfish (constantly nil))))

(defun %sameness-hash (target hash-depth path opt-fringe)
  ;(declare (optimize (speed 3) (safety 3)))
  "Compute the hash of a reference. If the vector OPT-FRINGE is provided, fills it with unsettled leaves and their locations in the structure; otherwise, encountering an unsettled reference is an error."
  (flet ((unsettled ()
           (if opt-fringe
             -1
             (error 'insufficiently-settled-error :values (list target)))))
    (%examine-reference
      target path opt-fringe
      :early-test
        (lambda (target)
          (when (<= hash-depth 0)
            (return-from %sameness-hash
              (if (sameness-fringe target path opt-fringe)
                -1
                (unsettled)))))
      :transparent-seed 0
      :transparent-op (lambda (accum a path-ext)
                        (logxor accum (%sameness-hash a (1- hash-depth) path-ext opt-fringe)))
      :nontransparent-op (lambda (target path)
                           (declare (ignore path))
                           (or (e.elib::same-hash-dispatch target)
                               (unsettled)))
      :when-resolved-selfish #'selfish-hash
      :when-unresolved-selfish #'unsettled)))

(defmethod e.elib::same-hash-dispatch ((a null))
  (declare (ignore a))
  0)
  
(defmethod e.elib::same-hash-dispatch (a)
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
     (psetf ,a ,b 
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
  
              (opt-same-spread (left right sofar leftv rightv)
                "returns e-boolean or nil"
                (declare (type (cons list list) sofar)
                         (type vector leftv rightv))
                (equalizer-trace "enter opt-same-spread ~S ~S ~S ~S ~S" left right sofar leftv rightv)
                (when (/= (length leftv) (length rightv))
                  ; Early exit, and precondition for the following loop.
                  (equalizer-trace "exit opt-same-spread lengths ~S ~S" (length leftv) (length rightv))
                  (return-from opt-same-spread +e-false+))
                (let ((not-different-result +e-true+))
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
                    ; Equality past resolved refs. At this
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
                  (let ((left-selfless (selflessp left))
                        (right-selfless (selflessp right)))
                    (if (and left-selfless right-selfless)
                      (let ((spread-l (opt-spread-equalizer-uncall left))
                            (spread-r (opt-spread-equalizer-uncall right)))
                        ; xxx we could squeeze out a little bit more efficiency
                        ; by short-circuiting if spread-l is nil
                        (cond
                          ((and spread-l spread-r)
                            (opt-same-spread left right sofar 
                                             spread-l spread-r))
                          ((e.elib::opt-same-dispatch left right))
                          (t
                            ; missing sameness definition, consider unsettled
                            nil)))
                      (progn
                        ; Resolved, uneql, and not both selfless, so they
                        ; can't be the same.
                        (equalizer-trace "exit opt-same not selfless ~S ~S" left-selfless right-selfless)
                        +e-false+))))))
      
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
                    
          (:|makeTraversalKey/1| 'make-traversal-key))))))

;;; --- TraversalKey ---

(defun same-yet-hash (target fringe)
  "Used by TraversalKey implementation. Returns a hash value which corresponds to the equality test ELIB:SAME-YET-P mixed with the fringe's hashes."
  ;; selfish-hash is correct here as long as it descends into conses (ook)
  (reduce #'logxor fringe :key #'selfish-hash :initial-value
    (%sameness-hash target +hash-depth+ nil fringe)))

(defclass traversal-key (vat-checking) 
  ((wrapped :initarg :wrapped
            :accessor tk-wrapped)
   (snap-hash :initarg :snap-hash
              :accessor tk-snap-hash
              :type fixnum)
   (fringe :initarg :fringe
           :accessor tk-fringe
           :type (vector cons))))
   
(defun make-traversal-key (target)
  (let ((wrapped (ref-shorten target))
        (fringe (make-array 1 :element-type 'cons
                              :adjustable t 
                              :fill-pointer 0)))
    (make-instance 'traversal-key
      :wrapped wrapped
      :fringe fringe
      :snap-hash (same-yet-hash wrapped fringe))))

(def-atomic-sameness traversal-key
  (lambda (a b)
    (and (eql (tk-snap-hash a)
              (tk-snap-hash b))
         (same-yet-p (tk-wrapped a)
                     (tk-wrapped b))
         (let ((af (tk-fringe a))
               (bf (tk-fringe b)))
           (and (eql (length af)
                     (length bf))
                (loop for (a-promise . a-path) across af
                      for (b-promise . b-path) across bf
                      always (and (eql a-promise b-promise)
                                  (equal a-path b-path)))))))
  tk-snap-hash)

(def-vtable traversal-key
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (or (eql auditor +deep-frozen-stamp+)
        (eql auditor +selfless+)))
  (:|__printOn| (this (tw +the-text-writer-guard+))
    "Does not print the reference behind this traversal key so that:
  * The key may be DeepFrozen trivially.
  * The key does not convey any authority to its referent."
    (declare (ignore this))
    (e. tw |write| "<a traversal key>")))

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
  "Return a hash value which corresponds to the equality test E.ELIB:SAMEP."
  (%sameness-hash target +hash-depth+ nil nil))

(defun settledp (a)
  (sameness-fringe a nil nil))
