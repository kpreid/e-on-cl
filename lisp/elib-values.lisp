; Except for otherwise-labeled sections, this file is:
; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :elib)

; --- Nil ---

(def-vtable null
  (:|__printOn| (this tw)
    (declare (ignore this))
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "null")))

(defmethod e-audit-check-dispatch ((auditor (eql +deep-frozen-stamp+)) (specimen null))
  t)

; --- String ---

(def-vtable string
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (if (e-is-true (e. tw |isQuoting|))
      (e. e.syntax:+e-printer+ |printString| tw this)
      (e. tw |write| this)))
  (:|__conformTo| (this guard)
    (setf guard (ref-shorten guard))
    ; XXX should this coercion be in the condition guard instead?
    (cond
      ((and (typep guard 'cl-type-guard)
            (subtypep 'simple-error (cl-type-specifier guard)))
        (make-condition 'simple-error
          :format-control "~A" 
          :format-arguments (list this)))
      (t this)))
  (:|op__cmp| (this other)
    ; XXX is this also available for EList, and if so can we move it there with no loss of efficiency?
    (e-coercef other 'string)
    (cond
      ((string= this other)  0)
      ((string< this other) -1)
      ((string> this other)  1)
      (t                    (error "shouldn't happen: broken string comparison"))))
  (:|add| (a b)
    "Concatenate this string and the other string."
    ; XXX OPT: check the simple case of b being a string
    (concatenate 'string a (e-print b)))
  (:|startsWith| (this prefix)
    "Return whether 'prefix' is a prefix of this string."
    (as-e-boolean (string= this prefix :end1 (min (length this) 
                                                  (length prefix)))))
  (:|endsWith| (this suffix)
    "Return whether 'prefix' is a suffix of this string."
    (as-e-boolean (string= this suffix :start1 (max 0 (- (length this) (length suffix))))))
  ; XXX simplify structure of both split and replaceAll
  (:|split| (this sep)
    "Return a list of substrings of this string which are separated by the string 'sep'. Will return empty elements at the end. The empty string results in a one-element result list."
    (e-coercef sep 'string)
    (when (string= sep "")
      (error "split: separator may not be empty"))
    (let* ((nsep (loop for pos = (search sep this) then (search sep this :start2 (+ (length sep) pos))
                       while pos count t))
           (result (make-array (1+ nsep) :element-type 'string :initial-element ""))
           (start 0)
           (rfill 0))
      (labels ((output (source &key (start 0) (end nil))
                 (setf (aref result rfill) (subseq source start end))
                 (incf rfill))
               (pick ()
                 (let ((pos (search sep this :start2 start)))
                   ;(print `(pos-is ,pos))
                   (cond 
                     ((null pos) (output this :start start :end pos)
                                 nil)
                     (t
                       (output this :start start :end pos)
                       (setf start (+ pos (length sep)))
                       t)))))
        (loop while (pick))
        result)))

  (:|rjoin| (this items)
    "Return the strings in 'items' concatenated and separated by this string.
    
someString.rjoin([\"\"]) and someString.rjoin([]) both result in the empty string."
    (let* ((items (loop for item across (e-coerce items 'vector) collect (e-coerce item 'string)))
           (string (make-array 
                     (reduce #'+ items
                       :initial-value 0
                       :key #'length)
                     :element-type 'character ; too clever: `(or ,@(mapcar #'array-element-type items))
                     :fill-pointer 0
                     :adjustable t)))
      (with-output-to-string (out string)
        (loop for item in items
              for sep = "" then this
              do (princ sep out)
                 (princ item out))
        string)))

  (:|replaceAll| (this old new)
    "Return this string with all occurrences of the string 'old' (searching from the beginning) replaced with the string 'new'."
    (declare (optimize (debug 3)))
    (e-coercef old 'string)
    (e-coercef new 'string)
    (when (string= old "")
      (error "replaceAll: search string may not be empty"))
    (let* ((nreplace (loop for pos = (search old this) then (search old this :start2 (+ (length old) pos))
                           while pos count t))
           (result (make-array (+ (length this) (* nreplace (- (length new) (length old)))) 
                               :element-type 'character
                               :initial-element #\%))
           (start 0)
           (rfill 0))
      (labels ((output (source &key (start 0) (end nil))
                 (replace result source
                                   :start1 rfill
                                   :start2 start
                                   :end2   end)
                 (incf rfill (- (or end (length this)) start)))
               (pick ()
                 ;(print `(replaceAll ,this ,old ,new - ,result ,start ,rfill))
                 (let ((pos (search old this :start2 start)))
                   ;(print `(pos-is ,pos))
                   (cond 
                     ((null pos) (output this :start start :end pos)
                                 nil)
                     (t
                       (output this :start start :end pos)
                       (replace result new :start1 rfill)
                       (incf rfill (length new))
                       ;(output new)
                       (setf start (+ pos (length old)))
                       t)))))
        (loop while (pick))
        result)))
  
  (:|isBare| (this)
    "Return true, because this is an ordinary String."
    (declare (ignore this))
    +e-true+))

(def-atomic-sameness string string= sxhash)

(defmethod e-audit-check-dispatch ((auditor (eql +deep-frozen-stamp+)) (specimen string))
  "Strings are atomic."
  t)

; --- Cons ---

; to have methods and a public maker eventually

(def-vtable cons
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. e.syntax:+e-printer+ |printCons| tw this)))

;(defmethod is-transparent-selfless (x cons)
;  (declare (ignore x))
;  t)

; --- Symbol ---

(def-vtable symbol
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    ; xxx does with-standard-io-syntax constitute adequate measures to hide any effects from dynamic variables?
    (e. tw |print| 
      (with-standard-io-syntax
        (if (e. tw |isQuoting|)
          (prin1-to-string this)
          (princ-to-string this))))))

;(def-atomic-sameness symbol eql sxhash)

; --- Character ---

(declaim (inline char-nearby))
(defun char-nearby (char delta)
  "Return the next or previous character after 'char', if possible."
  (declare (character char)
           (type (member +1 -1) delta)
           ;; Expecting code deletion: this is inlined and used with
           ;; constant deltas.
           #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (loop with code = (char-code char)
        initially (assert (char= char (code-char code)) 
                          (char)
                          "~A is not a simple character" (e-quote char))
        do (incf code delta)
           (cond
             ((< code 0)
               (error "there is no character before ~A" (e-quote char)))
             ((>= code char-code-limit)
               (error "there is no character after ~A" (e-quote char))))
        thereis (code-char code)))

(def-vtable character
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (if (e-is-true (e. tw |isQuoting|))
      (e. e.syntax:+e-printer+ |printCharacter| tw this)
      (e. tw |write| (make-string 1 :initial-element this))))
  (:|op__cmp| (this other)
    (e-coercef other 'character)
    (cond
      ((char< this other) -1.0)
      ((char> this other) 1.0)
      ((char= this other) 0.0)
      (t                 |NaN|)))
  (:|getCodepoint| (this)
    "Return the Unicode codepoint of this character."
    ; XXX assuming char-codes == Unicode without verifying
    (char-code this))
  (:|next| (this)
    "Return the next character in the total ordering of characters. Throws an exception if this is the last character."
    (char-nearby this +1))
  (:|previous| (this)
    "Return the previous character in the total ordering of characters. Throws an exception if this is the first character."
    (char-nearby this -1)))

(defmethod e-audit-check-dispatch ((auditor (eql +deep-frozen-stamp+)) (specimen character))
  t)

; --- ConstList (vector) ---

; Just as in Java E arrays serve as ConstLists, so do vectors here, and we make the same assumption of non-mutation.

; XXX this should be move to some utility section somewhere, or the equality section of elib-guts
; xxx OPT memoize results since we're doing all this subtypeping?
(declaim (ftype (function (t t) function) fast-sameness-test))
(defun fast-sameness-test (a-type b-type)
  "Return a function which will operate equivalently to EEQ-IS-SAME-EVER, provided that its two arguments are of a-type and b-type, respectively."
  (flet ((either-subtypep (type)
          (and (or (subtypep a-type type)
                   (subtypep b-type type))
               (not (or (subtypep 'ref a-type)
                        (subtypep 'ref b-type))))))
    (cond
      ; disjoint types - will never succeed
      ((subtypep `(and ,a-type ,b-type) nil)
        (constantly nil))
      ((either-subtypep '(or symbol))
        #'eq)
      ((either-subtypep '(or number character))
        ; XXX this should really be a test for "is an eql-atomic type, as in def-atomic-sameness"
        #'eql)
      ; general comparison
      (t 
        #'eeq-is-same-ever))))

; XXX we should have tests for the above but we don't have any test system for the Lisp side yet - this is the one-time code I wrote to test it:
; (mapcar #'(lambda (x) (cons x (eval (car x)))) '(
;   ((fast-sameness-test 't 't) (#'eeq-is-same-ever))
;   ((fast-sameness-test 'integer 'float) (constantly nil))
;   ((fast-sameness-test 'function 't) #'eeq-is-same-ever)
;   ((fast-sameness-test 'symbol 't) #'eeq-is-same-ever)
;   ((fast-sameness-test 'symbol 'function) (constantly nil))
;   ((fast-sameness-test 'character '(member #\a)) #'eql)
;   ((fast-sameness-test 'bit 'integer) #'eql)))

(defun reprint (obj fqn printer)
  (declare (ignore fqn)) ; XXX should be put into the getAllegedType
  (e-lambda nil ()
    ;; XXX pass optSealedDispatch, etc.
    (:|__printOn| (tw)
      (funcall printer (e-coerce tw +the-text-writer-guard+)))
    (otherwise (mverb &rest args)
      (apply #'e-call-dispatch obj mverb args))))

; XXX documentation
(def-vtable vector
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. e.syntax:+e-printer+ |printList| tw this +e-true+))
  (:|__optUncall| (this)
    `#(,+the-make-list+ "run" ,this))
  (:|asMap| (vector)
    "Return a ConstMap mapping the indices of this list to the elements of this list. For example, ['a', 'b'].asMap() == [0 => 'a', 1 => 'b']."
    ; xxx offer empty map constant when vector is empty?
    (e. +the-make-const-map+ |fromColumns|
      (let ((k -1)) (map '#.`(vector (integer 0 (,array-dimension-limit))) (lambda (v) (declare (ignore v)) (incf k)) vector))
      vector))
  (:|asKeys| (vector)
    "Return a ConstMap mapping the elements of this list to null."
    ; XXX preserve internal-element-type if possible
    (e. +the-make-const-map+ |fromIteratable|
      (e-lambda "org.cubik.cle.prim.listAsKeysIterator" () (:|iterate| (f)
        (loop for key across vector do
          (e. f |run| key nil))))
      +e-false+))
  (:|asSet| (vector)
    "Return a ConstSet with the elements of this list, omitting duplicates."
    (e. (e-import "org.cubik.cle.prim.makeConstSet") 
        |make| (e. vector |asKeys|)))
  (:|asStream| (vector
      &aux (position 0))
    "Return a stream providing the elements of this list."
    ;; XXX this is a mess. Revisit this once we have multiple uses of InStreamShell and figure out how the interface should be fixed to make common uses simple.
    (multiple-value-bind (backend backend-resolver) (make-promise)
      (with-result-promise (stream)
        (flet ((update-available (&aux (a (- (length vector) position)))
                (e. backend |setAvailable| a)
                (if (= a 0)
                  (e. stream |close|))))
          (prog1
            (reprint
              (e. (e-import "org.erights.e.elib.eio.makeInStreamShell")
                |run|
                +the-any-guard+ ; XXX should be (e. vector |valueType|)
                backend-resolver
                (e-lambda "org.cubik.cle.prim.listStreamImpl" ()
                  (:|semiObtain| (at-least at-most proceed report)
                    (e-coercef at-least 'integer)
                    (e-coercef at-most '(or integer null)) ; XXX ALL
                    (e-coercef proceed 'string) ; XXX enums
                    (e-coercef report 'string) ; XXX enums
                    (let* ((end (if at-most
                                  (min (length vector)
                                       (+ position at-most))
                                  (length vector)))
                           (chunk (- end position)))
                      (assert (>= chunk at-least)) ;; sanity check - the shell should take care of this for us
                      (prog1
                        (subseq vector position end)
                        (when (equal proceed "ADVANCE")
                          (setf position end)
                          (update-available)))))
                  (:|tryAvailable| (minimum)
                    "no-op"
                    (declare (ignore minimum))
                    nil)))
                "org.cubik.cle.prim.listStream"
                (lambda (tw)
                  (if (stringp vector)
                    (progn
                      (e. tw |write| "\"")
                      (e. tw |print| (subseq vector position (min (length vector) (+ position 20))))
                      (e. tw |write| "\\...\".asStream()"))
                    (progn
                      (e. tw |write| "[" #|]|#)
                      (e. (subseq vector position (min (length vector) (+ position 5))) |printOn| "" ", " "" tw)
                      (e. tw |write| #|[|# ", ...].asStream()")))))
            (update-available))))))
  (:|size/0| 'length)
  (:|get| (this index)
    "Return the 'index'th element of this list."
    (aref this (e-coerce index 'integer)))
  (:|last| (v) 
    "Return the last element of this list, or throw if it is empty."
    (aref v (1- (length v))))
  (:|add| (this other) 
    "Return the concatenation of both lists."
    (e-coercef other 'vector)
    (concatenate 'vector this other))
  (:|multiply| (vector times)
    "Return a list containing the elements of this list repeated 'times' times."
    (let* ((step (length vector))
           (result (make-array (* step times)
                    :element-type (array-element-type vector))))
      (dotimes (i times)
        (setf (subseq result (* step i)) vector))
      result))
  (:|iterate| (vector func)
    (loop for i from 0
          for elem across vector
          do  (e. func |run| i elem))
    nil)
  ; XXX OPT use less expensive eq tests if actual element type can be so tested
  (:|indexOf1| (vector elem)
    (or (position elem vector :test (fast-sameness-test `(eql ,elem)
                                                        (array-element-type vector)))
        -1))
  (:|lastIndexOf1| (vector elem)
    (or (position elem vector :test (fast-sameness-test `(eql ,elem)
                                                        (array-element-type vector))
                              :from-end t)
        -1))
  (:|startOf| (vector subseq)
    (e-coercef subseq 'vector)
    (or (search subseq vector :test (fast-sameness-test (array-element-type subseq)
                                                        (array-element-type vector)))
        -1))
  (:|lastStartOf| (vector subseq)
    (e-coercef subseq 'vector)
    (or (search subseq vector :test (fast-sameness-test (array-element-type subseq)
                                                        (array-element-type vector))
                              :from-end t)
        -1))
  (:|run/2| 'subseq)
  (:|run/1| 'subseq)
  (:|with| (vector elem)
    (concatenate 'vector vector (list elem)))
  (:|snapshot/0| 'identity)
  (:|diverge| (this)
    (e. this |diverge| +the-any-guard+))
  (:|diverge| (this value-guard)
    "Returns a FlexList with the same initial contents as this, with the specified element guard."
    (e. (e-import "org.erights.e.elib.tables.makeFlexList")
      |diverge|
      this
      value-guard))
  (:|printOn| (this left sep right tw)
    "Prints 'left', the values separated by 'sep', and 'right'.

'left' value0 'sep' ... 'right'"
    ; XXX write independent test for this
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| left)
    (loop
      for elem across this
      for prefix = "" then sep
      do (e. tw |print| prefix)
         (e. tw |quote| elem))
    (e. tw |print| right)))

; XXX make this an e-lambda
(defclass make-e-list () ())
(defglobal +the-make-list+ (make-instance 'make-e-list))

(def-vtable make-e-list
  ; XXX write tests for this
  (:|fromIteratableValues| (this iteratable) 
    (declare (ignore this))
    (let ((values))
      (e. iteratable |iterate| (efun (k v)
        (declare (ignore k))
        ; out-of-dynamic-extent calls are harmless
        (push v values)))
      (coerce (nreverse values) 'vector))))

(defmethod e-audit-check-dispatch ((auditor (eql +deep-frozen-stamp+)) (specimen make-e-list))
  t)

(defmethod e-call-match ((recip make-e-list) mverb &rest args)
  ; XXX __respondsTo
  (if (eq mverb (e-util:mangle-verb "run" (length args)))
    (apply #'vector args)
    (call-next-method)))


; --- Number ---

(def-vtable number
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (prin1-to-string this)))
  (:|add|      (a b)
    ; XXX inheriting CL result-type rules - any problems?
    (e-coercef b 'number)
    (+ a b))
  ; XXX tests for lack of coercion on: subtract multiply min max approxDivide floorDivide truncDivide mod remainder op__cmp
  (:|subtract| (a b)
    (e-coercef b 'number)
    (- a b))
  (:|negate/0|   '-)
  (:|multiply| (a b)
    (e-coercef b 'number)
    (* a b))
  (:|min| (a b)
    (e-coercef b 'number)
    (min a b))
  (:|max| (a b)
    (e-coercef b 'number)
    (max a b))
  (:|approxDivide| (a b) 
    (e-coercef b 'number)
    (handler-case
      (if *java-e-compatible*
        (coerce (/ a b) 'float64)
        (/ a b))
      (division-by-zero ()
        (cond
          ((= a 0)
            |NaN|)
          ((> a 0)
            |Infinity|)
          ((< a 0)
            |-Infinity|)))))
  (:|floorDivide| (a b) 
    (e-coercef b 'number)
    (floor (/ a b)))
  (:|truncDivide| (a b) 
    (e-coercef b 'number)
    (truncate (/ a b)))
  (:|mod| (a b) 
    (e-coercef b 'number)
    (mod a b))
  (:|remainder| (a b) 
    (e-coercef b 'number)
    (rem a b))
  (:|aboveZero| (this)
    (as-e-boolean (> this 0)))
  (:|belowZero| (this)
    (as-e-boolean (< this 0)))
  (:|atLeastZero| (this)
    (as-e-boolean (>= this 0)))
  (:|atMostZero| (this)
    (as-e-boolean (<= this 0)))
  (:|isZero| (this)
    (as-e-boolean (= this 0)))
  (:|op__cmp| (this other)
    (e-coercef other 'number)
    (cond
      ((< this other) -1.0)
      ((> this other) 1.0)
      ((= this other) 0.0)
      (t              |NaN|)))
  
  (:|toString| (this)
    "this.toString([].asMap())"
    (e. this |toString| (e. #() |asMap|)))
  (:|toString| (this options)
    "General number printing. The only option currently defined is \"base\", which must be an integer from 2 to 36, and defaults to 10."
    ; XXX won't work with non-integers
    (mapping-bind options
                  ((base "base" 10))
      (e-coercef base '(integer 2 36) (efun (c) (declare (ignore c))
                                        (error "base must be in 2..36, not ~A" base)))
      (string-downcase
        (with-standard-io-syntax
          (format nil "~VR" base this))))))

(defmethod e-audit-check-dispatch ((auditor (eql +deep-frozen-stamp+)) (specimen number))
  ; xxx is the specimen type too broad?
  t)

(def-vtable integer
  (:|__conformTo| (this guard) ; XXX should this be implemented by the guard? same question for float32 too
    ; XXX should use coerce/observable-type, not typep
    (setf guard (ref-shorten guard))
    (if (and (typep guard 'cl-type-guard) 
             (not (typep this (cl-type-specifier guard)))
             (subtypep `(eql ,(coerce this 'float64))
                       (cl-type-specifier guard)))
      (coerce this 'float64)
      this))
  (:|previous/0| '1-)
  (:|next/0| '1+)
  (:|isNaN| (this)
    (declare (ignore this))
    +e-false+))

(def-fqn integer "org.cubik.cle.native.int")

(def-vtable float
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| 
      (with-standard-io-syntax
        (let ((*read-default-float-format* 'double-float)
              (*print-readably* nil)) ; for clisp
          (prin1-to-string this)))))
  (:|isNaN| (this)
    (declare (ignore this))
    +e-false+))

(def-vtable (eql #.|NaN|)
  (:|__printOn| (this tw)
    (declare (ignore this))
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "NaN"))
  (:|isNaN| (this)
    (declare (ignore this))
    +e-true+))

(def-vtable (eql #.|Infinity|)
  (:|__printOn| (this tw)
    (declare (ignore this))
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "Infinity")))

(def-vtable (eql #.|-Infinity|)
  (:|__printOn| (this tw)
    (declare (ignore this))
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "-Infinity")))

(defglobal +the-make-int+ (e-lambda "org.cubik.cle.prim.makeInt" ()
  (:|run| (value)
    (e-coercef value 'string)
    (handler-case
      (parse-integer value)
      (error (e)
        (declare (ignore e))
        (error "not recognized as an integer: ~A" (e-quote value)))))))

; --- Condition ---

; XXX the <typename: desc> is inherited from Java-E: consider whether it's the Right Thing
(def-vtable condition
  (:|__printOn| (this tw
      &aux (interesting-type (not (typep this 'simple-error))))
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "problem: ")
    (when interesting-type
      (e. tw |print| "<" (simplify-fq-name (cl-type-fq-name (class-name (class-of this)))) ": "))
    (e. tw |print|
      ; CMUCL workaround
      (if (typep this 'simple-condition)
        (apply #'format nil (simple-condition-format-control this)
                            (simple-condition-format-arguments this))
        (princ-to-string this)))
    (when interesting-type
      (e. tw |print| ">")))
  (:|leaf| (this)
    "Java-E compatibility; currently just returns self."
    this))

(def-vtable type-error
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (let* ((specimen (type-error-datum this))
           (observed-type (observable-type-of specimen))) 
      (e. tw |print| "problem: the ")
      (if (eql observed-type 't)
        ; xxx should this code be in a function?
        (e. tw |quote| (e. (e. specimen |__getAllegedType|) |getFQName|))
        (e. tw |print| (cl-type-simple-expr observed-type)))
      (e. tw |print| " ")
      (e. tw |quote| (type-error-datum this))
      (e. tw |print| " doesn't coerce to "
                     (e-util:aan (cl-type-simple-expr (type-error-expected-type this)))))))

(def-vtable synchronous-call-error
  ; XXX should we really be doing this?
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| (format nil "problem: ~A" this))))
                      
; A vtable for CL:UNBOUND-VARIABLE is defined in elang.lisp.

(def-vtable local-throw-sealed-box
  (:|__printOn| (this tw)
    (declare (ignore this))
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<sealed problem>")))

; --- TraversalKey ---

; TraversalKey code is:
;  Copyright 2002 Combex, Inc. under the terms of the MIT X license
;  found at http://www.opensource.org/licenses/mit-license.html

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
      :snap-hash (eeq-same-yet-hash wrapped fringe))))

(defglobal +the-make-traversal-key+ (e-lambda "makeTraversalKey" ()
  (:|run/1| 'make-traversal-key)))

(defmethod eeq-same-dispatch ((a traversal-key) (b traversal-key))
  ;(format t "eeq-same-dispatch traversal-keys ~A ~A" (tk-wrapped a) (tk-wrapped b))
  (and
    (eql (tk-snap-hash a)
         (tk-snap-hash b))
    (eeq-is-same-yet (tk-wrapped a)
                     (tk-wrapped b))
    (eql (hash-table-count (tk-fringe a))
         (hash-table-count (tk-fringe b)))
    (loop for aelem being each hash-key of (tk-fringe a)
          always (nth-value 1 (gethash aelem (tk-fringe b))))))

(defmethod eeq-hash-dispatch ((a traversal-key))
  (tk-snap-hash a))
  
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

; --- TypeDesc, etc ---


(defun message-types-to-map (mtypes)
  (e. +the-make-const-map+ |fromColumns|
    (map 'vector 
      #'(lambda (md) 
        (format nil "~A/~A" (message-desc-verb md) 
                            (length (message-desc-params md)))) 
      mtypes)
    (coerce mtypes 'vector)))


(defmethod type-desc-message-types ((this type-desc))
  (message-types-to-map (type-desc-message-types-v this)))

(defmethod shared-initialize :after ((this type-desc) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (with-slots (message-types-v) this
    (loop 
      with seen = (make-hash-table)
      for md across message-types-v
      for mverb = (message-desc-mverb md)
      do (when (gethash mverb seen)
           (error "duplicate message desc for ~A: ~A then ~A"
             mverb 
             (e-quote (gethash mverb seen))
             (e-quote md)))
         (setf (gethash mverb seen) md))))

(defun message-desc-mverb (md)
  (e-util:mangle-verb (message-desc-verb md) (length (message-desc-params md))))


(defglobal +the-make-type-desc+ (e-lambda
    "org.erights.e.elib.base.makeTypeDesc"
    (:stamped +deep-frozen-stamp+)
  (:|run| (doc-comment fq-name supers auditors mtypes)
    (e-coercef doc-comment 'string)
    (e-coercef fq-name     '(or null string))
    (e-coercef supers      'vector)
    (e-coercef auditors    'vector)
    (e-coercef mtypes      '(vector message-desc))
    (setf mtypes (copy-seq mtypes))
    ; (vector TYPE) enforces only upgraded-type of elements
    (loop 
      for i below (length mtypes)
      do (e-coercef (aref mtypes i) 'message-desc))
    (make-instance 'type-desc
      :doc-comment doc-comment 
      :fq-name fq-name 
      :supers supers 
      :auditors auditors 
      :message-types-v mtypes))))

(defglobal +the-make-message-desc+ (e-lambda
    "org.erights.e.elib.base.makeMessageDesc"
    (:stamped +deep-frozen-stamp+)
  (:|run| (doc-comment verb params opt-result-guard)
    (e-coercef doc-comment 'string)
    (e-coercef verb        'string)
    (e-coercef params      '(vector param-desc))
    (setf params (copy-seq params))
    ; (vector TYPE) enforces only upgraded-type of elements
    (loop for i below (length params) do
      (e-coercef (aref params i) 'param-desc))
    (e-coercef opt-result-guard 't)
    (make-instance 'message-desc
      :doc-comment doc-comment
      :verb verb
      :params params
      :opt-result-guard opt-result-guard))))

(defglobal +the-make-param-desc+ (e-lambda
    "org.erights.e.elib.base.makeParamDesc"
    (:stamped +deep-frozen-stamp+)
  (:|run| (opt-name opt-guard)
    (e-coercef opt-name '(or null string))
    (e-coercef opt-guard 't)
    (make-instance 'param-desc :opt-name opt-name :opt-guard opt-guard))))


; Hmm. Could we write something like DEF-STRUCTOID-VTABLE to implement the common features of these?

(def-vtable type-desc
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (let* ((simple-name (copy-seq (simplify-fq-name (or (type-desc-opt-fq-name this) "_"))))
           (initial (position-if #'both-case-p simple-name)))
      (when initial
        (setf (char simple-name initial)
              (char-upcase (char simple-name initial))))
      (e. tw |print| simple-name)))
  (:|__optUncall| (this)
    (with-slots (doc-comment opt-fq-name supers auditors message-types-v) this
      `#(,+the-make-type-desc+ "run" #(,doc-comment ,opt-fq-name ,supers ,auditors ,message-types-v))))
  (:|getOptFQName/0| 'type-desc-opt-fq-name)
  (:|getFQName| (td)
    (or (type-desc-opt-fq-name td) "_"))
  (:|getDocComment/0| 'type-desc-doc-comment)
  (:|getSupers/0| 'type-desc-supers)
  (:|getAuditors/0| 'type-desc-auditors)
  (:|getMessageTypes/0| 'type-desc-message-types))

(def-vtable message-desc
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (with-slots (verb doc-comment params opt-result-guard) this
      (e. e.syntax:+e-printer+ |printMethodHeader| tw +e-false+ doc-comment verb params opt-result-guard)))
  (:|__optUncall| (this)
    (with-slots (doc-comment verb params opt-result-guard) this
      `#(,+the-make-message-desc+ "run" #(,doc-comment ,verb ,params ,opt-result-guard))))
  (:|getVerb/0|           'message-desc-verb)
  (:|getDocComment/0|     'message-desc-doc-comment)
  (:|getParams/0|         'message-desc-params)
  (:|getOptResultGuard/0| 'message-desc-opt-result-guard))

(def-vtable param-desc
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (with-slots (opt-name opt-guard) this
      (e. e.syntax:+e-printer+ |printGuardedNounPattern| tw opt-name opt-guard)))
  (:|__optUncall| (this)
    (with-slots (opt-name opt-guard) this
      `#(,+the-make-param-desc+ "run" #(,opt-name ,opt-guard))))
  (:|getOptName/0|  'param-desc-opt-name)
  (:|getOptGuard/0| 'param-desc-opt-guard)
  (:|getName| (this)
    "Returns _ if this ParamDesc has no name."
    (or (param-desc-opt-name this) "_")))

(defmethod eeq-is-transparent-selfless ((a type-desc))
  (declare (ignore a))
  t)
(defmethod eeq-is-transparent-selfless ((a message-desc))
  (declare (ignore a))
  t)
(defmethod eeq-is-transparent-selfless ((a param-desc))
  (declare (ignore a))
  t)

; --- Weak references ---

; XXX cmucl support not tested, just written from the same documentation as for sbcl

#+(or ccl clisp)
(defclass weak-ref-impl (vat-checking)
  (#+ccl (table :initarg :table)
   #+clisp (weak-pointer :initarg :weak-pointer)))

(defglobal +the-make-weak-ref+ (e-lambda "org.erights.e.elib.vat.makeWeakRef" ()
  ; XXX run/4
  (:|run| (referent reactor)
    "Make a weak reference to the given ref. If 'reactor' is not null, invoke its run/0 method when the referent is GCed."
    ; XXX what happens for fixnums, null, etc? should we disallow all selfless objects?
    (assert (null reactor) () "Sorry, reactors not implemented yet")
    
    #+sbcl (progn
      ;(sb-ext:finalize referent (lambda () ))
      (sb-ext:make-weak-pointer referent))
    
    #+cmu (progn
      ;(extensions:finalize referent (lambda () ))
      (extensions:make-weak-pointer referent))
    
    #+ccl (let ((table (make-hash-table :size 1 :weak :value)))
      ;(ccl:terminate-when-unreachable referent (lambda (obj) ))
      (setf (gethash 't table) referent)
      (make-instance 'weak-ref-impl :table table))
    
    ; CLISP's ext:weak-pointer is not a class, so we can't define a vtable for it.
    #+clisp (make-instance 'weak-ref-impl :weak-pointer (ext:make-weak-pointer referent))
    
    #-(or sbcl cmu ccl clisp) (error "sorry, no weak reference implementation for this Lisp yet"))))

#+(or sbcl cmu ccl clisp) (def-vtable 
    #+sbcl sb-ext:weak-pointer
    #+cmu extensions:weak-pointer
    #+(or ccl clisp) weak-ref-impl
  (:|__printOn| (this tw
      &aux (value (e. this |get|)))
    (e-coercef tw +the-text-writer-guard+)
    (if value
      (progn
        (e. tw |print| "<weak:")
        (e. tw |quote| value)
        (e. tw |print| ">"))
      (e. tw |print| "<dead weak ref>")))
  (:|get| (this)
    "Return the normal ref which is this weak ref's referent, or null if it has been GCed. This method cannot distinguish a weak reference to null."
    #+sbcl  (sb-ext:weak-pointer-value this)
    #+cmu   (extensions:weak-pointer-value this)
    #+ccl   (gethash 't (slot-value this 'table) nil)
    #+clisp (ext:weak-pointer-value (slot-value this 'weak-pointer))
    ))

; --- "E" object ---

; XXX 'E' should probably be in knot.lisp

(defglobal +the-e+ (e-lambda "org.erights.e.elib.prim.E"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| (tw) ; XXX this can be deleted, I think - try later
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<E>"))
  (:|call| (r v a)
    (e-coercef v 'string)
    (e-coercef a 'vector)
    (e-call r v a))
  (:|callWithPair| (rec verb-args)
    (e-coercef verb-args '(vector * 2))
    (e-call rec (aref verb-args 0) (aref verb-args 1)))
  (:|send| (r v a)
    (e-coercef v 'string)
    (e-coercef a 'vector)
    (e-send r v a))
  (:|sendOnly| (r v a)
    ; xxx provide actual send-only
    (e-coercef v 'string)
    (e-coercef a 'vector)
    (e-send r v a))
  (:|toQuote/1| 'e-quote)
  (:|toString/1| 'e-print)))

