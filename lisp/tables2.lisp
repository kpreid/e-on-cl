; Copyright 2005-2006 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib.tables)

; --- Setting up genhash ---

(with-simple-restart (continue "Skip hash function registration.")
  (register-hash-function 'samep #'same-hash #'samep))

;;; --- Text position algorithms for Twine ---

(defun displaced-subseq (string &optional (start 0) end)
  ;; XXX utility, to be moved
  "Act as SUBSEQ, but produce a displaced string."
  (unless end
    (setf end (length string)))
  (make-array (- end start)
              :element-type (array-element-type string)
              :displaced-to string
              :displaced-index-offset start))

;; XXX make this make sense
(defun drop-newline-op (function text  &rest options)
  (apply function #\Newline text :end (1- (length text)) options))
(defun all-newline-op (function text  &rest options)
  (apply function #\Newline text options))

(defun moved-text-position (line column text &optional (newline-op #'all-newline-op))
  (flet ()
    (cond
      ((= (length text) 0) 
       (values line column))
      ((not (funcall newline-op #'find text)) 
       (values line (+ column (length text))))
      (t
       (values (+ line (funcall newline-op #'count text))
               (- (length text) (1+ (funcall newline-op #'position text :from-end t))))))))

(defun source-span-for-string (text uri)
  (when (plusp (length text))
    (multiple-value-bind (end-line end-col)
        (moved-text-position 1 0 text #'drop-newline-op)
      (efuncall +the-make-source-span+ 
        uri
        (as-e-boolean (not (drop-newline-op #'find text)))
        1        0
        end-line (1- end-col)))))

; --- SourceSpan for Twine ---

(defclass source-span ()
  ((uri        :initarg :uri        :type string      :reader span-uri)
   (one-to-one :initarg :one-to-one :type boolean     :reader span-one-to-one-p)
   (start-line :initarg :start-line :type (integer 1) :reader span-start-line)
   (end-line   :initarg :end-line   :type (integer 1) :reader span-end-line)
   (start-col  :initarg :start-col  :type (integer 0) :reader span-start-col)
   (end-col    :initarg :end-col    :type (integer 0) :reader span-end-col)))

(def-fqn source-span "org.erights.e.elib.base.SourceSpan")

(def-vtable source-span
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +selfless-stamp+))
  (:|__printOn/1| (span out)
    (e-coercef out +the-text-writer-guard+)
    (with-slots (uri one-to-one start-line end-line start-col end-col) span
      (e. out |write| "<")
      (e. out |print| uri)
      (e. out |write| "#:")
      (e. out |print| (if one-to-one "span" "blob"))
      (e. out |write| "::")
      (e. out |print| start-line)
      (e. out |write| ":")
      (e. out |print| start-col)
      (e. out |write| "::")
      (e. out |print| end-line)
      (e. out |write| ":")
      (e. out |print| end-col)
      (e. out |write| ">")))
  (:|__optUncall/0| (span)
    (with-slots (uri one-to-one start-line end-line start-col end-col) span
      (vector +the-make-source-span+ "run" (vector uri
                                                   (as-e-boolean one-to-one)
                                                   start-line
                                                   start-col
                                                   end-line 
                                                   end-col))))
  (:|getUri/0| 'span-uri)
  (:|isOneToOne/0| (span) (as-e-boolean (span-one-to-one-p span)))
  (:|getStartLine/0| 'span-start-line)
  (:|getEndLine/0| 'span-end-line)
  (:|getStartCol/0| 'span-start-col)
  (:|getEndCol/0| 'span-end-col)
  (:|notOneToOne/0| (span)
    (with-slots (uri one-to-one start-line end-line start-col end-col) span
      (make-instance 'source-span :uri uri
                                  :one-to-one nil
                                  :start-line start-line
                                  :start-col start-col
                                  :end-line end-line
                                  :end-col end-col))))

(defobject +the-make-source-span+ "org.erights.e.elib.base.makeSourceSpan"
    (:stamped +deep-frozen-stamp+
     :stamped +standard-graph-exit-stamp+)
  (:|asType| () 
    (type-specifier-to-guard 'source-span))
  (:|run| (uri one-to-one start-line start-col end-line end-col) 
    (e-coercef uri        'string)
    (e-coercef one-to-one 'e-boolean)
    (e-coercef start-line '(integer 1))
    (e-coercef end-line   '(integer 1))
    (e-coercef start-col  '(integer 0))
    (e-coercef end-col    '(integer 0))
    (unless (or (not (e-is-true one-to-one))
                (= start-line end-line))
      (error "one-to-one span must be on a single line"))
    (make-instance 'source-span :uri uri
                                :one-to-one (e-is-true one-to-one)
                                :start-line start-line
                                :end-line end-line
                                :start-col start-col
                                :end-col end-col)))

(defun ~span (stream arg colon at)
  "Print a SourceSpan, if present, preceded by \" @\". For use in format strings."
  (declare (ignore colon at))
  (let ((tw (make-text-writer-to-cl-stream stream)))
    (unwind-protect 
      (when (ref-shorten arg)
        (e. tw |write| " @ ")
        (e. tw |print| arg))
      (e. tw |close|))))

; --- Twine ---

(defgeneric twine-string (twine)
  (:method ((string string)) string))

(defclass %twine () ()
  (:documentation "Class of non-bare twines."))

(def-vtable %twine
  (:|__printOn| (this out)
    (e-coercef out +the-text-writer-guard+)
    (e. out |printSame| (twine-string this)))
  (:|__conformTo| (this guard)
    (declare (ignore guard))
    (twine-string this))
  (:|isBare| (this) 
    (declare (ignore this))
    (error "twine type not implementing isBare"))
  (:|getParts| (this) 
    (declare (ignore this))
    (error "twine type not implementing getParts"))
  (:|getOptSpan| (this) 
    (declare (ignore this))
    (error "twine type not implementing getOptSpan"))
  (:|run| (this start end)
    (declare (ignore this start end))
    (error "twine type not implementing run/2"))
  
  ;; XXX Twine plus String
  (:|add| (this other)
    (e-coercef other 'twine)
    (make-twine-from-parts (lambda (f) (e. (e. this |getParts|) |iterate| f)
                                       (e. (e. other |getParts|) |iterate| f))))
  (:|split| (this sep)
    "Return a list of subtwines of this twine which are separated by the string 'sep'. Will return empty elements at the end. The empty twine results in a one-element result list."
    (e.elib::split-by-runs (twine-string this) sep this)))

(defmethod e-call-match ((rec %twine) mverb &rest args)
  (apply #'e-call-dispatch (twine-string rec) mverb args))

(deftype twine ()
  '(or string %twine))
  
(def-fqn twine "org.erights.e.elib.tables.Twine")


(defun coalesce (t1 t2)
  (flet ((strings () (concatenate 'string (twine-string t1)
                                          (twine-string t2))))
    (if (and (e-is-true (e. t1 |isBare|)) 
             (e-is-true (e. t2 |isBare|)))
      ;; plain strings
      (strings)
      ;; not plain
      (let ((span1 (e. t1 |getOptSpan|))
            (span2 (e. t2 |getOptSpan|)))
        (if (and span1 span2
                 (string= (e. span1 |getUri|) (e. span2 |getUri|))
                 (samep (e. span1 |isOneToOne|)
                                   (e. span2 |isOneToOne|)))
          ;; compatible spans (same URI)
          (if (e. span1 |isOneToOne|)
            (if (and (= (e. span1 |getStartLine|)
                        (e. span2 |getStartLine|))
                     (= (e. span1 |getEndCol|)
                        (1- (e. span2 |getStartCol|))))
              ;; one-to-one and adjacent
              (make-instance 'leaf-twine 
                :string (strings)
                :span (efuncall +the-make-source-span+ 
                        (e. span1 |getUri|)
                        +e-true+
                        (e. span1 |getStartLine|)
                        (e. span1 |getStartCol|)
                        (e. span2 |getEndLine|)
                        (e. span2 |getEndCol|)))
              ;; not adjacent
              nil)
            (if (samep span1 span2)
              ;; blob and identical
              (make-instance 'leaf-twine 
                :string (strings)
                :span span1)
              ;; blob and not identical
              nil))
          ;; incompatible spans
          nil)))))

(defun make-twine-from-parts (iter)
  (let ((parts '()))
    (funcall iter
      (efun (k part)
        (declare (ignore k))
        (if parts
          (let ((coalesced (coalesce (first parts) part)))
            (if coalesced
              (setf (first parts) coalesced)
              (push part parts)))
          (push part parts))))
    (nreverse-here parts)
    (cond
      ((null parts)
       "")
      ((null (rest parts))
       (first parts))
      (t
       (make-instance 'composite-twine :parts parts)))))


(defclass composite-twine (%twine)
  ((parts :initarg :parts :type list)))

(defmethod twine-string ((this composite-twine))
  (apply #'concatenate 'string (mapcar #'twine-string (slot-value this 'parts))))

(def-vtable composite-twine
  (:|__optUncall| (this)
    (with-slots (parts) this
      (vector +the-make-twine+ "fromParts" (vector (coerce parts 'vector)))))
  (:|isBare| (this) 
    (declare (ignore this))
    +e-false+)
  (:|getOptSpan| (this) 
    (declare (ignore this))
    nil)
  (:|getParts| (this) (coerce (slot-value this 'parts) 'vector)))


(defclass leaf-twine (%twine)
  ((string :initarg :string :type string :reader twine-string)
   (span :initarg :span :type source-span)))

(def-vtable leaf-twine
  (:|__optUncall| (this)
    (with-slots (string span) this
      (vector +the-make-twine+ "fromString" (vector string span))))
  (:|isBare| (this) 
    (declare (ignore this))
    +e-false+)
  (:|getOptSpan| (this) (slot-value this 'span))
  (:|getParts| (this) (vector this))
  
  (:|run| (this start end)
    (e-coercef start 'integer)
    (e-coercef end 'integer)
    (e. +the-make-twine+ |fromString|
      (efuncall (twine-string this) start end)
      (when (plusp (- end start))
        (let* ((span (slot-value this 'span))
               (string (twine-string this))
               (drun (displaced-subseq string start end)))
          (multiple-value-bind (run-start-line run-start-col)
              (moved-text-position (span-start-line span) 
                                   (span-start-col span) 
                                   (displaced-subseq string 0 start))
            (multiple-value-bind (run-end-line run-end-col)
                (moved-text-position run-start-line run-start-col drun)
              ;(print (list run-start-line run-start-col run-end-line run-end-col))
              ;(force-output)
              (efuncall +the-make-source-span+ 
                (e. span |getUri|)
                (as-e-boolean (not (all-newline-op #'find drun)))
                run-start-line run-start-col
                run-end-line (1- run-end-col)))))))))


(defobject +the-make-twine+ "org.erights.e.elib.tables.makeTwine"
    (:stamped +deep-frozen-stamp+)
  (:|fromSequence| (iteratable) 
    "Return a Twine composed of the characters in the given sequence (object that implements iterate/1).
    
The ConstList version of this is called fromIteratableValues, unfortunately. XXX decide what the right name for this operation is."
    ; XXX duplicated code with ConstList
    ;; XXX implement efficiently if the source is a vector
    (let ((values))
      (e. iteratable |iterate| (efun (k v)
        (declare (ignore k))
        ; out-of-dynamic-extent calls are harmless
        (push v values)))
      (coerce (nreverse values) 'string)))
  (:|fromString| (string span)
    (e-coercef string 'string)
    (e-coercef span '(or null source-span))
    (if span
      (progn 
        (unless (plusp (length string))
          (error "an empty twine may not have a source span"))
        (unless (or (not (span-one-to-one-p span))
                    (= (length string) (- (1+ (span-end-col span)) 
                                          (span-start-col span))))
          (error "the source span, ~A, must match the size of the string, ~S, or be not one-to-one" (e-quote span) (length string)))
        (make-instance 'leaf-twine :string string :span span))
      string)))

; --- Primitive safe mutable array access ---

(defobject +the-flex-array-array-brand+ "org.cubik.cle.prim.flexArrayArrayBrand" ())
(defclass flex-array-array-sealed-box ()
  ((array :initarg :array
         :accessor unseal-flex-array-array-sealed-box)))

(defun make-flex-array (array)
  "Wrap a Lisp array with an object which exposes mutation methods to E, and does not attempt to be an E collection. Written for use by FlexList and does not have a complete set of methods yet."
  (e-lambda "org.cubik.cle.prim.Array" ()
    
    (:|__optSealedDispatch| (brand)
      (if (eql brand +the-flex-array-array-brand+)
        (make-instance 'flex-array-array-sealed-box :array array)))
  
    (:|elt| (i)
      (e-coercef i 'integer)
      (elt array i))
    (:|setElt| (i new)
      (e-coercef i 'integer)
      (setf new (ref-shorten new))
      (setf (elt array i) new))
    (:|aref| (i)
      (e-coercef i 'integer)
      (aref array i)) ; XXX support n-ary aref
    (:|length| () (length array))
    (:|getFillPointer| () (fill-pointer array))
    (:|setFillPointer| (new) 
      (e-coercef new 'integer)
      (setf (fill-pointer array) new))

    (:|getAdjustable| () (as-e-boolean (adjustable-array-p array)))
    (:|getDimension| (i) 
      (e-coercef i 'integer)
      (array-dimension array i))
    
    (:|snapshotVector| ()
      "Return a vector (i.e. ConstList) containing the current contents of the array, which must be one-dimensional."
      (copy-seq array))
    
    (:|vectorPushExtend| (new)
      (setf new (ref-shorten new))
      ; XXX leaves undefined elements in any extra-extension area
      (vector-push-extend new array)) ; XXX will this fail safely if the array is >1-dimensional?
    
    (:|fill| (value)           
      (setf value (ref-shorten value))
      (fill array value))
    (:|fill| (value start)     
      (setf value (ref-shorten value))
      (e-coercef start 'integer)
      (fill array value :start start))
    (:|fill| (value start end) 
      (setf value (ref-shorten value))
      (e-coercef start 'integer)
      (e-coercef end 'integer)
      (fill array value :start start :end end))

    (:|replace| (seq start1 end1 start2 end2)
      "XXX for now seq must be another flex-array"
      (e-coercef start1 'integer)
      (e-coercef end1   'integer)
      (e-coercef start2 'integer)
      (e-coercef end2   'integer)
      (let ((source (unseal-flex-array-array-sealed-box
                      (e. seq |__optSealedDispatch|
                        +the-flex-array-array-brand+))))
        (replace array source
          :start1 start1
          :end1   end1
          :start2 start2
          :end2   end2)))
    
    (:|adjust| (new-dimensions initial-element)
      ; NOTE: Exposing the ability to create displaced arrays, or access to displaced-to arrays, is unsafe: "If A is displaced to B, the consequences are unspecified if B is adjusted in such a way that it no longer has enough elements to satisfy A." -- CLHS
      (setf new-dimensions 
        (map 'list #'(lambda (d) (e-coerce d 'integer))
          (e-coerce new-dimensions 'vector)))
      ; unspecified behavior otherwise:
      (assert (or (not (array-has-fill-pointer-p array))
                  (< (fill-pointer array) (elt new-dimensions 0))))
      (make-flex-array
        (adjust-array array
          new-dimensions
          :initial-element initial-element)))))

(defobject +the-make-array+ "org.cubik.cle.prim.makeArray" 
    (:stamped +deep-frozen-stamp+)
  (:|fromSequence| (seq adjustable)
    "Makes a one-dimensional array with a fill pointer at the end and an element type of any."
    (e-coercef seq 'sequence)
    (make-flex-array (make-array (length seq) 
                                 :initial-contents seq
                                 :adjustable (e-is-true adjustable)
                                 :fill-pointer t))))

; --- ConstMap ---

(defclass const-map () ())

(defmethod print-object ((map const-map) stream)
  (if (and *print-readably* *read-eval*)
    (format stream "#.~S" 
      `(e. +the-make-const-map+ :|fromColumns| 
         ,@(coerce (e. map |getPair|) 'list)))
    (print-unreadable-object (map stream :type t :identity nil)
      (loop for sep = "" then " "
            for key across (ref-shorten (e. map |getKeys|))
            for value across (ref-shorten (e. map |getValues|))
            do (format stream "~A~W => ~W" sep key value)))))

(defun comparer-adapter ()
  (let ((comparer (eelt (vat-safe-scope *vat*) "__comparer")))
    (lambda (a b) (e-is-true (e. comparer |lessThan| a b)))))

(defun maps-no-sugar (map key)
  (e. map |fetch| key (efun () (return-from maps-no-sugar nil)))
  t)

(def-vtable const-map
  (audited-by-magic-verb (this auditor)
    (declare (ignore this))
    (eql auditor +selfless-stamp+))

  ; XXX import documentation strings lazily from EMap interface
  (:|__optUncall| (this)
    `#(,+the-make-const-map+ "fromColumns" ,(e. this |getPair|)))
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (if (zerop (e. this |size|))
      (e. tw |print| "[].asMap()")
      (e. this |printOn| "[" " => " ", " "]" tw)))
  (:|keyType| (this) (declare (ignore this)) +the-any-guard+)
  (:|valueType| (this) (declare (ignore this)) +the-any-guard+)
  (:|snapshot/0| 'identity)
  (:|readOnly/0| 'identity)
  
  ; The folowing are methods equally applicable to non-const maps, but all of them are currently implemented in E.
  
  ; xxx with is generally used repeatedly: have a special efficiently-accumulating map
  (:|with| (map new-key new-value)
    (e. +the-make-const-map+ |fromIteratable|
      (e-lambda "org.cubik.cle.prim.mapWithIterator" () (:|iterate| (f)
        (e. map |iterate| f)
        (efuncall f new-key new-value)
        nil))
      +e-false+))

  ; xxx using more complicated and slow version to imitate Java-E's ordering behavior
  (:|without| (map key-to-remove)
    "Return a ConstMap including all entries in this map except for the given key, which is replaced in the ordering with the last element of the map."
    (if (maps-no-sugar map key-to-remove)
      (let (last-pair 
            removing-nonlast)
        (e. map |iterate| (efun (k v) (setf last-pair (list k v))))
        (setf removing-nonlast (not (samep (first last-pair) key-to-remove)))
        (e. +the-make-const-map+ |fromIteratable|
          (e-lambda "org.cubik.cle.prim.mapWithoutIterator" () (:|iterate| (f)
            (e. map |iterate| (efun (key value)
              (cond
                ((and last-pair removing-nonlast (samep key key-to-remove))
                  (efuncall f (first last-pair) (second last-pair)))
                ((samep key (first last-pair))
                  #|do nothing|#)
                (t
                  (efuncall f key value)))
              nil))
            nil))
          +e-true+))
      map))

  (:|sortKeys| (map)
    (e. +the-make-const-map+ |fromIteratable|
      (e-lambda "org.cubik.cle.prim.sortKeysIterator" () (:|iterate| (f)
        (loop for key across (stable-sort (copy-seq (e-coerce (e. map |getKeys|) 'sequence))
                                          (comparer-adapter))
              do
          (efuncall f key (eelt map key)))
        nil))
      +e-true+))

  (:|and| (map mask)
    (e-coercef mask +the-any-map-guard+)
    (e. +the-make-const-map+ |fromIteratable|
      (e-lambda "org.cubik.cle.prim.mapAndIterator" () (:|iterate| (f)
        (e. map |iterate| (efun (key value)
          (when (maps-no-sugar mask key)
            (efuncall f key value))
          nil))
        nil))
      +e-true+))

  ; simpler but not-like-Java-E butNot. XXX discuss whether it would be acceptable to use this
  ;(:|butNot| (map mask)
  ;  (e-coercef mask +the-any-map-guard+)
  ;  (e. +the-make-const-map+ |fromIteratable|
  ;    (e-lambda (:|iterate| (f)
  ;      (e. map |iterate| (e-lambda (:|run| (key value)
  ;        (unless (e-is-true (e. mask |maps| key))
  ;          (efuncall f key value))
  ;        nil)))
  ;      nil))
  ;    +e-true+))

  (:|butNot| (map mask)
    (e-coercef mask +the-any-map-guard+)
    (let* ((map-keys (e-coerce (e. map |getKeys|) 'vector))
           (ordering (make-array (length map-keys) 
                                 :initial-contents map-keys
                                 :fill-pointer t)))
      (loop with i = 0
            while (< i (length ordering))
            do (if (maps-no-sugar mask (aref ordering i))
                 (if (= i (1- (length ordering)))
                   (vector-pop ordering)
                   (setf (aref ordering i) (vector-pop ordering)))
                 (incf i)))
      (e. +the-make-const-map+ |fromIteratable|
        (e-lambda "org.cubik.cle.prim.mapButNotIterator" () (:|iterate| (f)
          (loop for key across ordering do
            (efuncall f key (e. map |fetch| key nil)))
          nil))
        +e-true+)))
  
  (:|or| (front behind)        (e. front |or| behind +e-false+ 'nil))
  (:|or| (front behind strict) (e. front |or| behind strict    'nil))
  (:|or| (front behind strict opt-ejector)
    (cond
      ((eql 0 (e. front  |size|)) (e. behind |snapshot|))
      ((eql 0 (e. behind |size|)) (e. front |snapshot|))
      (t 
        (e. +the-make-const-map+ |fromIteratable|
          (e-lambda "org.cubik.cle.prim.mapOrIterator" () (:|iterate| (f)
            (e. behind |iterate| f)
            (e. front |iterate| f)
            nil))
          strict opt-ejector))))

  ;; XXX move this into mapSugar  
  (:|iterate| (map func)
    (let* ((pair (ref-shorten (e. map |getPair|))))
      (loop for key   across (aref pair 0)
            for value across (aref pair 1)
            do (efuncall func key value))
      nil)))

(defmethod e-call-match ((rec const-map) mverb &rest args)
  (apply #'sugar-cache-call rec mverb 'const-map "org.erights.e.elib.tables.mapSugar" args))

(def-fqn const-map "org.erights.e.elib.tables.ConstMap")
(def-class-opaque const-map)


(defglobal +the-map-guard+ (make-instance 'cl-type-guard :type-specifier 'const-map))

(defglobal +the-any-map-guard+ +the-map-guard+) ; XXX what this *should* be is a stamp-checking guard to allow standard FlexMaps and ROMaps (which are currently implemented in E)

; --- genhash ConstMap ---

(defclass genhash-const-map-impl (const-map)
  ((table       :initarg :table)
   (keys        :initarg :keys
                :type vector)
   (values      :initarg :values
                :type vector)))

(defmethod shared-initialize :after ((this genhash-const-map-impl) slot-names &key &allow-other-keys)
  "Allow genhash-const-map-impls to be created with only :keys and :values initargs."
  (declare (ignore slot-names))
  (unless (slot-boundp this 'table)
    (with-slots (table keys) this
      (setf table (make-generic-hashtable :test 'samep))
      (loop for key across keys
            for i from 0
            do (setf (hashref key table) i)))))

(def-vtable genhash-const-map-impl
  (:|getPair| (this)
    (with-slots (keys values) this
      (vector keys values)))
  (:|snapshot/0| 'identity)
  (:|fetch| (this key absent-thunk)
    (with-slots (table keys values) this
      (let ((index (hashref key table)))
        (if index
          (aref values index)
          (efuncall absent-thunk)))))
  (:|size| (this)
    (with-slots (keys) this (length keys)))
  (:|getKeys| (this)
    (with-slots (keys) this keys))
  (:|getValues| (this)
    (with-slots (values) this values)))

; --- genhash FlexMap ---

(defgeneric ensure-storage (this))

(defun copy-samep-genhash (old &aux (new (make-generic-hashtable :test 'samep)))
  (map-generic-hash #'(lambda (k v) (setf (hashref k new) v)) old)
  new)

(defclass genhash-flex-map-impl (vat-checking)
  ((self        :initarg :self)
   (key-guard   :initarg :key-guard   :initform +the-any-guard+)
   (value-guard :initarg :value-guard :initform +the-any-guard+)
   (snapshot    :initform nil
                :documentation "If non-nil, then reads should be redirected to this ConstMap (currently always genhash-const-map-impl). Writes should cause its state to be copied into new table, keys, and values.")
   (table       :initarg :table
                :initform (make-generic-hashtable :test 'samep))
   (keys        :initform (make-array 0 :fill-pointer 0 :adjustable t)
                :type (or null (and vector 
                                    (satisfies adjustable-array-p)
                                    (satisfies array-has-fill-pointer-p))))
   (values      :initform (make-array 0 :fill-pointer 0 :adjustable t)
                :type (or null (and vector
                                    (satisfies adjustable-array-p)
                                    (satisfies array-has-fill-pointer-p))))))


(defmethod ensure-storage ((this genhash-flex-map-impl))
  (with-slots (table keys values snapshot) this
    (when snapshot
      (assert (typep snapshot 'genhash-const-map-impl)) ; due to slot-value below - XXX ugly
      (let* ((s-keys (e. snapshot |getKeys|))
             (s-values (e. snapshot |getValues|))
             (size (length s-keys)))
        ; XXX specialise keys and values arrays
        (setf keys   (make-array size
                                 :fill-pointer t
                                 :adjustable t
                                 :initial-contents s-keys)
              values (make-array size
                                 :fill-pointer t
                                 :adjustable t
                                 :initial-contents s-values)
              table (copy-samep-genhash (slot-value snapshot 'table))
              snapshot nil)))))

(def-vtable genhash-flex-map-impl
  (:|__printOn| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    ; XXX call e-printer
    ; XXX this is duplicated code with simple-flex-map, because we expect to throw out most of the CL-written flex-map machinery
    (if (zerop (e. this |size|))
      (e. tw |print| "[].asMap()")
      (e. (slot-value this 'self) |printOn| "[" " => " ", " "]" tw))
    (e. tw |print| ".diverge()"))

  (:|size| (this)
    (with-slots (snapshot keys) this
      (if snapshot
        (e. snapshot |size|)
        (length keys))))

  (:|put| (this key value strict opt-ejector)
    (ensure-storage this)
    (with-slots (table keys values key-guard value-guard) this
      (e-coercef key key-guard)
      (e-coercef value value-guard)
      (let ((index (hashref key table)))
        (if index
          (if (e-is-true strict)
            (ejerror opt-ejector "~A already in map" (e-quote key))
            (setf (aref values index) value))
          (progn 
            (setf (hashref key table) (length keys))
            (vector-push-extend key   keys)
            (vector-push-extend value values)))))
    nil)

  ; XXX need to change the protocol to have an ejector
  (:|removeKey| (this key strict &aux opt-ejector)
    (ensure-storage this)
    (with-slots (table keys values key-guard value-guard) this
      (e-coercef key key-guard)
      (let ((index (hashref key table)))
        (if index
          (let ((last-index (1- (length keys))))
            (when (and (> last-index 0) (/= index last-index))
              ; Move last entry to the position of the deleted entry
              (setf (hashref (aref keys last-index) table)
                      index
                    (aref keys   index) (aref keys   last-index)
                    (aref values index) (aref values last-index)))
            (decf (fill-pointer keys))
            (decf (fill-pointer values))
            (hashrem key table))
          (when (e-is-true strict)
            (ejerror opt-ejector "~A not in map" (e-quote key))))))
    nil)

  ; XXX to be moved to flexmap shell?
  (:|keyType|   (this) (slot-value this 'key-guard))
  (:|valueType| (this) (slot-value this 'value-guard))

  (:|fetch| (this key absent-thunk)
    (with-slots (snapshot table keys values key-guard) this
      (e-coercef key key-guard)
      (if snapshot
        (e. snapshot |fetch| key absent-thunk)
        (let ((index (hashref key table)))
          (if index
            (aref values index)
            (efuncall absent-thunk))))))

  (:|getPair| (this)
    (with-slots (snapshot keys values) this 
      (if snapshot
        (e. snapshot |getPair|)
        (vector (copy-seq keys) (copy-seq values)))))

  (:|getKeys| (this)
    (with-slots (snapshot keys) this
      (if snapshot
        (e. snapshot |getKeys|)
        (copy-seq keys)))))


; --- --- Interfaces --- ---

; --- ConstMap maker ---

(defobject +the-make-const-map+ "org.erights.e.elib.tables.makeConstMap"
    (:stamped +deep-frozen-stamp+
     :stamped +standard-graph-exit-stamp+)
  (:|__printOn| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "__makeMap"))
  (:|asType| ()
    ; XXX we provide the sugared Map guard instead of the primitive one - is this really appropriate?
    (eelt (vat-safe-scope *vat*) "Map"))
  (:|fromPairs| (pairs)
    (e-coercef pairs 'vector)
    (loop
      with keys   = (make-array (length pairs))
      with values = (make-array (length pairs))
      for i from 0
      for pair-maybe across pairs
      for pair = (e-coerce pair-maybe 'vector)
      do (setf (aref keys i)   (aref pair 0)
               (aref values i) (aref pair 1))
      finally (return (e. +the-make-const-map+ |fromColumns| keys values))))
  (:|fromColumns| (keys values)
    ; XXX Java-E uses the valueType of keys and values
    (e-coercef keys 'vector)
    (e-coercef values 'vector)
    (make-instance 'genhash-const-map-impl :keys keys :values values))
  (:|fromProperties| (props)
    "Java-E compatibility. Makes a ConstMap from whatever we're providing that imitates a Java properties(?) object."
    ; XXX this should probably become coercion to ConstMap
    (declare (ignore props))
    (e. +the-make-const-map+ |fromPairs| #()))
  (:|fromIteratable| (iteratable estrict)
    (e. +the-make-const-map+ |fromIteratable| iteratable estrict nil))
  (:|fromIteratable| (iteratable estrict opt-ejector)
    "Given an Iteratable, return the ConstMap constructed from its elements. If estrict is false, later elements override earlier ones."
    ; XXX have version with capacity argument
    (let* ((strict (e-is-true estrict))
           (closed nil)
           (keys   (make-array 4 :adjustable t :fill-pointer 0))
           (values (make-array 4 :adjustable t :fill-pointer 0))
           (table  (make-generic-hashtable :test 'samep)))
      (e. iteratable |iterate| 
        (e-lambda "org.cubik.cle.prim.ConstMapConstructionIterator" ()
          (:|run| (key value)
            (cond
              (closed
                (error "ConstMap construction already finished"))
              ((not (hashref key table))
                ; normal case
                (setf (hashref key table) (length values))
                (vector-push-extend key keys)
                (vector-push-extend value values))
              (strict
                (ejerror opt-ejector "~A already in under-construction ConstMap as ~A" (e-quote key) (e-quote (aref values (hashref key table)))))
              (t
                (setf (aref values (hashref key table)) value)))
            nil)))
      (setf closed t)
      (make-instance 'genhash-const-map-impl :keys keys :values values :table table))))

; --- FlexMap maker ---

(defun make-primitive-flex-map ()
  (make-instance 'genhash-flex-map-impl))

; XXX support fromTypes/3 then make and/1 use the capacity arg
(defobject +the-make-flex-map+ "org.erights.e.elib.tables.makeFlexMap"
    (:stamped +deep-frozen-stamp+)
  (:|fromTypes| (key-guard value-guard)
    ; XXX try coerce to primitive and make use of that
    ;     "If the guards coerce to primitive-type guards, then the map will use the coercion result instead of the provided guard, and may be able to transparently use a more efficient internal representation."
    ; XXX specialize columns when possible
    (with-result-promise (map)
      (let* ((impl 
              (make-instance 'e.elib.tables::genhash-flex-map-impl
                :self map
                :key-guard key-guard
                :value-guard value-guard))
             (map-shell
              (efuncall (e-import "org.erights.e.elib.tables.makeFlexMapShell")
                map
                impl)))
        (e-lambda "org.erights.e.elib.tables.GenhashFlexMapImplOuter" ()
          (:|__printOn| (out)
            (e. map-shell |__printOn| out))
          (:|__optUncall| ()
            `#(,(e. map |snapshot|) "diverge" #(,key-guard ,value-guard)))
          (:|snapshot| ()
            (with-slots (snapshot table keys values) impl
              (unless snapshot
                ; The principle here is that the most common use of snapshotting is to build a collection and then make it immutable. XXX benchmark such decisions.
                (setf snapshot (make-instance 'genhash-const-map-impl 
                                  :table table
                                  :keys keys 
                                  :values values))
                (setf table nil
                      keys nil
                      values nil))
              snapshot))
          (otherwise (&rest spread-msg)
            (apply #'e-call-dispatch map-shell spread-msg)))))))

