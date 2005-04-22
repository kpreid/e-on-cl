; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib.tables)

; --- Setting up genhash ---

(register-hash-function 'eeq-is-same-ever #'eeq-same-yet-hash #'eeq-is-same-ever)

; --- ... ---

; --- Twine ---

; minimal to make Twine guard work and be distinct
; XXX actual twines

(deftype twine ()
  'string)
  
(def-fqn twine "org.erights.e.elib.tables.Twine")

(defvar +the-make-twine+ (e-named-lambda "org.erights.e.elib.tables.makeTwine"
  :stamped +deep-frozen-stamp+
  ))

; --- Primitive safe mutable array access ---

(defvar +the-flex-array-array-brand+ (e-lambda))
(defclass flex-array-array-sealed-box ()
  ((array :initarg :array
         :accessor unseal-flex-array-array-sealed-box)))

(defun make-flex-array (array)
  "Wrap a Lisp array with an object which exposes mutation methods to E, and does not attempt to be an E collection. Written for use by FlexList and does not have a complete set of methods yet."
  (e-named-lambda "org.cubik.cle.prim.Array"
    
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

    (:|getDimension| (i) 
      (e-coercef i 'integer)
      (array-dimension array i))
    
    (:|snapshotVector| ()
      "Return a vector (i.e. ConstList) containing the current contents of the array, which must be one-dimensional."
      (copy-seq array))
    
    (:|vectorPushExtend| (new)
      (setf new (ref-shorten new))
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

(defvar +the-make-array+ (e-named-lambda "org.cubik.cle.prim.makeArray"
  (:|fromSequence| (seq)
    "Makes a one-dimensional array with a fill pointer at the end and an element type of any."
    (make-flex-array (make-array (length seq) :initial-contents seq :fill-pointer t)))))

; --- ConstMap ---

(defclass const-map () ())


(def-vtable const-map
  ; XXX import documentation strings lazily from EMap interface
  (:|__optUncall/0| (this)
    `#(,+the-make-const-map+ "fromColumns" ,(e. this |getPair|)))
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (if (= 0 (e. this |size|))
      (e. tw |print| "[].asMap()")
      (e. this |printOn| "[" " => " ", " "]" tw)))
  (:|keyType/0| (this) (declare (ignore this)) +the-any-guard+)
  (:|valueType/0| (this) (declare (ignore this)) +the-any-guard+)
  (:|snapshot/0| #'identity)
  (:|readOnly/0| #'identity)
  
  ; The folowing are methods equally applicable to non-const maps, but all of them are currently implemented in E.
  
  (:|get/1| (this key)
    (e. this |fetch|
      key
      (e-lambda (:|run| ()
        ; XXX can this error be made more informative?
        (error "~A not found" (e-quote key))))))
  (:|maps/1| (this key)
    (block nil
      (e. this |fetch| key (e-lambda (:|run| () (return +e-false+))))
      +e-true+))
  
  ; xxx with is generally used repeatedly: have a special efficiently-accumulating map
  (:|with/2| (map new-key new-value)
    (e. +the-make-const-map+ |fromIteratable|
      (e-lambda (:|iterate| (f)
        (e. map |iterate| f)
        (e. f |run| new-key new-value)
        nil))
      +e-false+))

  ; xxx using more complicated and slow version to imitate Java-E's ordering behavior
  (:|without/1| (map key-to-remove)
    "Return a ConstMap including all entries in this map except for the given key, which is replaced in the ordering with the last element of the map."
    (if (e-is-true (e. map |maps| key-to-remove))
      (let (last-pair 
            removing-nonlast)
        (e. map |iterate| (e-lambda (:|run/2| (&rest p) (setf last-pair p))))
        (setf removing-nonlast (not (eeq-is-same-ever (first last-pair) key-to-remove)))
        (e. +the-make-const-map+ |fromIteratable|
          (e-lambda (:|iterate| (f)
            (e. map |iterate| (e-lambda (:|run| (key value)
              (cond
                ((and last-pair removing-nonlast (eeq-is-same-ever key key-to-remove))
                  (e. f |run| (first last-pair) (second last-pair)))
                ((eeq-is-same-ever key (first last-pair))
                  #|do nothing|#)
                (t
                  (e. f |run| key value)))
              nil)))
            nil))
          +e-true+))
      map))

  (:|optExtract/1| (this key)
    (block nil
      (vector
        (e. this |fetch| key
          (e-lambda (:|run| ()
            (return nil))))
        (e. this |without| key))))

  (:|and/1| (map mask)
    (e-coercef mask +the-any-map-guard+)
    (e. +the-make-const-map+ |fromIteratable|
      (e-lambda (:|iterate| (f)
        (e. map |iterate| (e-lambda (:|run| (key value)
          (when (e-is-true (e. mask |maps| key))
            (e. f |run| key value))
          nil)))
        nil))
      +e-true+))

  ; simpler but not-like-Java-E butNot. XXX discuss whether it would be acceptable to use this
  ;(:|butNot/1| (map mask)
  ;  (e-coercef mask +the-any-map-guard+)
  ;  (e. +the-make-const-map+ |fromIteratable|
  ;    (e-lambda (:|iterate| (f)
  ;      (e. map |iterate| (e-lambda (:|run| (key value)
  ;        (unless (e-is-true (e. mask |maps| key))
  ;          (e. f |run| key value))
  ;        nil)))
  ;      nil))
  ;    +e-true+))

  (:|butNot/1| (map mask)
    (e-coercef mask +the-any-map-guard+)
    (let* ((map-keys (e-coerce (e. map |getKeys|) 'vector))
           (ordering (make-array (length map-keys) 
                                 :initial-contents map-keys
                                 :fill-pointer t)))
      (loop with i = 0
            while (< i (length ordering))
            do (if (e-is-true (e. mask |maps| (aref ordering i)))
                 (if (= i (1- (length ordering)))
                   (vector-pop ordering)
                   (setf (aref ordering i) (vector-pop ordering)))
                 (incf i)))
      (e. +the-make-const-map+ |fromIteratable|
        (e-lambda (:|iterate| (f)
          (loop for key across ordering do
            (e. f |run| key (e. map |get| key)))
          nil))
        +e-true+)))
  
  (:|or/1| (front behind)        (e. front |or| behind +e-false+ 'nil))
  (:|or/2| (front behind strict) (e. front |or| behind strict    'nil))
  (:|or/3| (front behind strict opt-ejector)
    (cond
      ((eql 0 (e. front  |size|)) (e. behind |snapshot|))
      ((eql 0 (e. behind |size|)) (e. front |snapshot|))
      (t 
        (e. +the-make-const-map+ |fromIteratable|
          (e-lambda (:|iterate| (f)
            (e. behind |iterate| f)
            (e. front |iterate| f)
            nil))
          strict opt-ejector))))
  
  (:|iterate/1| (map func)
    (let* ((pair (ref-shorten (e. map |getPair|))))
      (loop for key   across (aref pair 0)
            for value across (aref pair 1)
            do (e. func |run| key value))
      nil))
  
  (:|printOn/5| (map left-s map-s sep-s right-s tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |write| left-s)
    (let ((this-sep ""))
      (e. map |iterate| (e-lambda (:|run| (key value)
        (e. tw |write| this-sep)
        (e. tw |quote| key)
        (e. tw |write| map-s)
        (e. tw |quote| value)
        (setf this-sep sep-s)))))
    (e. tw |write| right-s))
  
  (:|diverge/0| (this) (e. this |diverge| +the-any-guard+ +the-any-guard+))
  (:|diverge/2| (this key-guard value-guard)
    (let ((flex-map (e. +the-make-flex-map+ |fromTypes| key-guard value-guard)))
      (e. flex-map |putAll| this +e-true+ (e-lambda (:|run| (problem)
        ; xxx explain exactly what keys, after the problem argument becomes more informative
        (declare (ignore problem))
        (error "duplicate keys in ~A under ~A coercion"
          (e-quote this)
          (e-quote key-guard)))))
      flex-map)))


(def-fqn const-map "org.erights.e.elib.tables.ConstMap")
(defmethod eeq-is-transparent-selfless ((a const-map))
  (declare (ignore a)) 
  t)
(def-class-opaque const-map)


(defvar e.elib:+the-map-guard+ (make-instance 'cl-type-guard :type-specifier 'const-map))

(defvar e.elib:+the-any-map-guard+ e.elib:+the-map-guard+) ; XXX what this *should* be is a stamp-checking guard to allow standard FlexMaps and ROMaps (which are currently implemented in E)

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
      (setf table (make-generic-hashtable :test 'eeq-is-same-ever))
      (loop for key across keys
            for i from 0
            do (setf (hashref key table) i)))))

(def-vtable genhash-const-map-impl
  (:|getPair/0| (this)
    (with-slots (keys values) this
      (vector keys values)))
  (:|snapshot/0| #'identity)
  (:|fetch/2| (this key absent-thunk)
    (with-slots (table keys values) this
      (let ((index (hashref key table)))
        (if index
          (aref values index)
          (e. absent-thunk |run|)))))
  (:|size/0| (this)
    (with-slots (keys) this (length keys)))
  (:|getKeys/0| (this)
    (with-slots (keys) this keys))
  (:|getValues/0| (this)
    (with-slots (values) this values)))

; --- genhash FlexMap ---

(defgeneric ensure-storage (this))

(defun copy-eeq-genhash (old &aux (new (make-generic-hashtable :test 'eeq-is-same-ever)))
  (map-generic-hash #'(lambda (k v) (setf (hashref k new) v)) old)
  new)

(defclass genhash-flex-map-impl (vat-checking)
  ((self        :initarg :self)
   (key-guard   :initarg :key-guard   :initform +the-any-guard+)
   (value-guard :initarg :value-guard :initform +the-any-guard+)
   (snapshot    :initform nil
                :documentation "If non-nil, then reads should be redirected to this ConstMap (currently always genhash-const-map-impl). Writes should cause its state to be copied into new table, keys, and values.")
   (table       :initarg :table
                :initform (make-generic-hashtable :test 'eeq-is-same-ever))
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
              table (copy-eeq-genhash (slot-value snapshot 'table))
              snapshot nil)))))

(def-vtable genhash-flex-map-impl
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    ; XXX call e-printer
    ; XXX this is duplicated code with simple-flex-map, because we expect to throw out most of the CL-written flex-map machinery
    (if (= 0 (e. this |size|))
      (e. tw |print| "[].asMap()")
      (e. (slot-value this 'self) |printOn| "[" " => " ", " "]" tw))
    (e. tw |print| ".diverge()"))

  (:|size/0| (this)
    (with-slots (snapshot keys) this
      (if snapshot
        (e. snapshot |size|)
        (length keys))))

  (:|put/4| (this key value strict opt-ejector)
    (ensure-storage this)
    (with-slots (table keys values key-guard value-guard) this
      (e-coercef key key-guard)
      (e-coercef value value-guard)
      (let ((index (hashref key table)))
        (if index
          (if (e-is-true strict)
            (eject-or-ethrow opt-ejector (make-condition 'simple-error
              :format-control "~A already in map"
              :format-arguments (list (e-quote key))))
            (setf (aref values index) value))
          (progn 
            (setf (hashref key table) (length keys))
            (vector-push-extend key   keys)
            (vector-push-extend value values)))))
    nil)

  ; XXX need to change the protocol to have an ejector
  (:|removeKey/2| (this key strict &aux opt-ejector)
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
            (eject-or-ethrow opt-ejector (make-condition 'simple-error
              :format-control "~A not in map"
              :format-arguments (list (e-quote key))))))))
    nil)

  ; XXX to be moved to flexmap shell?
  (:|keyType/0|   (this) (slot-value this 'key-guard))
  (:|valueType/0| (this) (slot-value this 'value-guard))

  (:|fetch/2| (this key absent-thunk)
    (with-slots (snapshot table keys values key-guard) this
      (e-coercef key key-guard)
      (if snapshot
        (e. snapshot |fetch| key absent-thunk)
        (let ((index (hashref key table)))
          (if index
            (aref values index)
            (e. absent-thunk |run|))))))

  (:|getPair/0| (this)
    (with-slots (snapshot keys values) this 
      (if snapshot
        (e. snapshot |getPair|)
        (vector (copy-seq keys) (copy-seq values)))))

  (:|getKeys/0| (this)
    (with-slots (snapshot keys) this
      (if snapshot
        (e. snapshot |getKeys|)
        (copy-seq keys)))))


; --- --- Interfaces --- ---

; --- ConstMap maker ---

(defvar +the-make-const-map+ (e-named-lambda "org.erights.e.elib.tables.makeConstMap"
  :stamped +deep-frozen-stamp+
  (:|__printOn/1| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "__makeMap"))
  (:|asType/0| ()
    ; XXX we provide the sugared Map guard instead of the primitive one - is this really appropriate?
    (e. (vat-safe-scope *vat*) |get| "Map"))
  (:|fromPairs/1| (pairs)
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
  (:|fromColumns/2| (keys values)
    ; XXX Java-E uses the valueType of keys and values
    (e-coercef keys 'vector)
    (e-coercef values 'vector)
    (make-instance 'genhash-const-map-impl :keys keys :values values))
  (:|fromProperties/1| (props)
    "Java-E compatibility. Makes a ConstMap from whatever we're providing that imitates a Java properties(?) object."
    ; XXX this should probably become coercion to ConstMap
    (declare (ignore props))
    (e. +the-make-const-map+ |fromPairs| #()))
  (:|fromIteratable/2| (iteratable estrict)
    (e. +the-make-const-map+ |fromIteratable| iteratable estrict nil))
  (:|fromIteratable/3| (iteratable estrict opt-ejector)
    "Given an Iteratable, return the ConstMap constructed from its elements. If estrict is false, later elements override earlier ones."
    ; XXX have version with capacity argument
    (let* ((strict (e-is-true estrict))
           (closed nil)
           (keys   (make-array 4 :adjustable t :fill-pointer 0))
           (values (make-array 4 :adjustable t :fill-pointer 0))
           (table  (make-generic-hashtable :test 'eeq-is-same-ever)))
      (e. iteratable |iterate| 
        (e-named-lambda "org.cubik.cle.prim.ConstMapConstructionIterator"
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
                (eject-or-ethrow opt-ejector (make-condition 'simple-error :format-control "~A already in under-construction ConstMap as ~A" :format-arguments (list (e-quote key) (e-quote (aref values (hashref key table)))))))
              (t
                (setf (aref values (hashref key table)) value)))
            nil)))
      (setf closed t)
      (make-instance 'genhash-const-map-impl :keys keys :values values :table table)))))

; --- FlexMap maker ---

(defun make-primitive-flex-map ()
  (make-instance 'genhash-flex-map-impl))

; XXX support fromTypes/3 then make and/1 use the capacity arg
(defvar +the-make-flex-map+ (e-lambda "org.erights.e.elib.tables.makeFlexMap"
  :stamped +deep-frozen-stamp+
  (:|fromTypes/2| (key-guard value-guard)
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
              (e. (e. (e. (vat-safe-scope *vat*) 
                          |get| "import__uriGetter")
                      |get| "org.erights.e.elib.tables.makeFlexMapShell")
                |run|
                map
                impl)))
        (e-named-lambda "org.erights.e.elib.tables.GenhashFlexMapImplOuter"
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
            (apply #'e-call-dispatch map-shell spread-msg))))))))
