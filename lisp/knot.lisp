; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.knot)

(declaim (inline map-from-hash)
         (ftype (function (t function hash-table) (or null vector list)) map-from-hash))
; XXX this should be in some other file - it's here because it was first written for use in the scope code
(defun map-from-hash (result-type function hash-table)
  "Like cl:map, but iterating over hash tables."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (cond
    ((subtypep result-type 'nil)
      (maphash function hash-table))
    ((subtypep result-type 'list)
      (loop for key being each hash-key of hash-table using (hash-value value)
            collect (funcall function key value)))
    ((subtypep result-type 'vector)
      (loop with v = (make-array (hash-table-count hash-table)
                       :element-type
                         (cond
                           ((or (eql result-type 'vector)
                                (and (consp result-type)
                                     (eql (first result-type) 'vector)
                                     (eql (second result-type) '*)))
                             't)
                           ((and (consp result-type)
                                 (eql (first result-type) 'vector))
                             (second result-type))
                           (t
                             (error "unsupported vector result type ~S in map-from-hash" result-type))))
            for key being each hash-key of hash-table using (hash-value value)
            for i from 0
            do (setf (aref v i) (funcall function key value))
            finally (return v)))
    (t
      (error "unsupported result type ~S in map-from-hash" result-type))))

(defvar +e-ref-kit-slot+)

; --- Scope objects ---

(defclass scope () 
  ((fqn-prefix :initarg :fqn-prefix 
               :initform "__unnamed_outer"
               :type string)
   (slot-table :initarg :slot-table
               :type hash-table)
   (slot-ordering-cache :initform nil :type (or null (vector string)))))

(def-fqn scope "org.erights.e.elang.scope.Scope") ; XXX should we have a non-elang fqn?

(defun scope-slot-ordering (scope)
  (with-slots (slot-table slot-ordering-cache) scope
    (or slot-ordering-cache
        (setf slot-ordering-cache
          (sort (map-from-hash 'vector
                  (lambda (noun slot) (declare (ignore slot)) noun)
                  slot-table)
                #'string<)))))

(defun make-scope (fqn-prefix init-list)
  (make-instance 'scope
    :fqn-prefix fqn-prefix
    :slot-table
      (let ((table (make-hash-table :test #'equal 
                                    :size (length init-list))))
        (loop for (varspec value) in init-list do
          (if (eql 0 (position #\& varspec))
            (setf (gethash (subseq varspec 1) table) 
                  value)
            (setf (gethash varspec table) 
                  (make-instance 'elib:e-simple-slot :value value))))
        table)))

(defvar +the-make-scope+ (e-named-lambda "org.erights.e.elang.scope.makeScope"
  :stamped +deep-frozen-stamp+
  (:|asType/0| ()
    (make-instance 'cl-type-guard :type-specifier 'scope))
  (:|fromState/2| (state fqn-prefix)
    "XXX document"
    (e-coercef state +the-any-map-guard+)
    (e-coercef fqn-prefix 'string)
    (make-scope fqn-prefix
                (loop with (keys values) = (coerce (e. state |getPair|) 'list)
                      for key across keys
                      for value across values
                      collect (list key value))))))

; XXX have scopes use hash tables instead of alists
; XXX reduce code duplication among get/fetch methods

(def-vtable scope
  (:|__printOn/1| (this tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<scope " (slot-value this 'fqn-prefix) ">")
    nil)
  (:|__optUncall/0| (this)
    (with-slots (slot-table fqn-prefix) this
      `#(,+the-make-scope+
         "fromState" 
         #(,(e. +the-make-const-map+ |fromPairs|
              (map 'vector
                (lambda (noun &aux (slot (gethash noun slot-table)))
                  (if (and (typep slot 'e-simple-slot) 
                           (not (e-is-true (e. noun |startsWith| "&"))))
                    (vector noun (e-slot-get slot))
                    (vector (concatenate 'string "&" noun) slot)))
                (scope-slot-ordering this)))
           ,fqn-prefix))))
  (:|or/1| (inner outer)
    (e-coercef outer 'scope)
    (make-instance 'scope
      :fqn-prefix (slot-value inner 'fqn-prefix)
      :slot-table
        (let ((new-table (make-hash-table :test #'equal 
                                          :size (hash-table-count (slot-value outer 'slot-table)))))
          (loop for old-scope in (list outer inner)
                for old-table = (slot-value old-scope 'slot-table)
                do
            (loop for noun being each hash-key of old-table using (hash-value slot) do
              (setf (gethash noun new-table) slot)))
          new-table)))
  (:|maps/1| (this noun)
    (e-coercef noun 'string)
    (with-slots (slot-table) this
      (as-e-boolean (nth-value 1 (gethash noun slot-table)))))
  (:|get/1| (scope noun)
    (e-slot-get (e. scope |getSlot| noun)))
  (:|getSlot/1| (this noun)
    (e-coercef noun 'string)
    (with-slots (slot-table) this
      (multiple-value-bind (slot present) (gethash noun slot-table)
        (if present
          slot
          (error "binding not in scope: ~A" (e-quote noun))))))
  (:|fetch/2| (this noun absent-thunk)
    (e-coercef noun 'string)
    (with-slots (slot-table) this
      (multiple-value-bind (slot present) (gethash noun slot-table)
        (if present
          (e-slot-get slot)
          (e. absent-thunk |run|)))))
  (:|put/2| (this noun value)
    (e-slot-set (e. this |getSlot| noun) value))
  (:|getState/0| (this)
    "Return a ConstMap containing the bindings in this scope, as \"&\" + noun => slot."
    (e. +the-make-const-map+ |fromIteratable| this +e-true+))
  (:|iterate/1| (scope afunc)
    "Iterate over the bindings in this scope, as \"&\" + noun => slot."
    (with-slots (slot-table) scope
      (loop for noun across (scope-slot-ordering scope) do
        (e. afunc |run| (concatenate 'string "&" noun) 
                        (gethash noun slot-table)))
      nil))
  (:|with/2| (scope noun value)
    (e. scope |withSlot| noun (make-instance 'e-simple-slot :value value)))
  (:|withSlot/2| (scope new-noun new-slot)
    (e-coercef new-noun 'string)
    ; xxx support efficient accumulation?
    (with-slots (slot-table) scope
      (make-instance 'scope 
        :fqn-prefix (slot-value scope 'fqn-prefix)
        :slot-table
          (let ((new-table (make-hash-table :test #'equal 
                                            :size (1+ (hash-table-count slot-table)))))
            (loop for noun being each hash-key of slot-table using (hash-value slot) do
              (setf (gethash noun new-table) slot))
            (setf (gethash new-noun new-table) new-slot)
            new-table))))
  (:|withPrefix/1| (scope new)
    (e-coercef new 'string)
    (with-slots (slot-table) scope
      (make-instance 'scope 
        :fqn-prefix new
        :slot-table slot-table)))
  (:|getFQNPrefix/0| (scope)
    (slot-value scope 'fqn-prefix)))

(defmethod eeq-is-transparent-selfless ((a scope))
  (declare (ignore a))
  t)

; --- standard scope definitions ---

(defvar +the-looper+ (e-named-lambda "org.erights.e.elang.interp.loop"
  :stamped +deep-frozen-stamp+
  (:|__printOn/1| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<__loop>")
    nil)
  (:|run/1| (body)
    (loop while (e-is-true (e. body |run|))))))

(defvar +the-thrower+ (e-named-lambda "org.erights.e.elib.prim.throw"
  :stamped +deep-frozen-stamp+
  (:|__printOn/1| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "throw")
    nil)
  (:|run/1| (problem)
    (error (e-problem-to-condition (e-coerce problem 'condition))))
  (:|eject/2| (opt-ejector problem)
    (elib:eject-or-ethrow opt-ejector (e-problem-to-condition problem)))
  (:|free/1| (problem)
    ; XXX there should be a function for this
    (error (if *compatible-catch-leakage*
             problem
             (make-condition 'elib::free-problem :value problem))))))

(defun split-fqn-prefix (fqn)
  (let ((pos (position #\. fqn)))
    (if pos
      (cons (subseq fqn 0 pos) (split-fqn-prefix (subseq fqn (1+ pos))))
      (progn
        (assert (string= fqn ""))
        nil))))

(defvar +make-first-char-splitter+ (e-named-lambda "org.quasiliteral.text.makeFirstCharSplitter"
  (:|run/1| (specials)
    (e-coercef specials 'string)
    (flet ((match (ch) (position ch specials)))
      (e-named-lambda "org.quasiliteral.text.FirstCharSplitter"
        (:|findIn/1| (str)
          (e-coercef str 'string)
          (or (position-if #'match str)
              -1))
        (:|findInFrom/2| (str start) ; XXX write tests
          (e-coercef str 'string)
          (e-coercef start `(integer 0 ,(length str)))
          (or (position-if #'match str :start start)
              -1)))))))

(defun uncall-to-unget (loader portrayal)
  (e-coercef portrayal '(or null vector))
  ; xxx I do not understand the justification in the doc-comment of Java-E baseLoader#optUnget.
  (if (and portrayal 
           (=   (length portrayal) 3)
           (eql (aref portrayal 0) loader)
           (eql (aref portrayal 1) "get"))
    (let ((args (e-coerce (aref portrayal 2) 'vector)))
      (if (= (length args) 1)
        (aref args 0)))))

(defun unget-to-uncall (loader name)
  (if name
    `#(,loader "get" #(,name))))

; --- direct E-to-Lisp interfaces and powers ---

; XXX look into whether e-lambda, now that it has matcher support, would be cleaner for implementing this
(defun wrap-function (f &key stamps)
  (labels ((wrapper (mverb &rest args)
      (cond
        ((eql mverb (e-util:mangle-verb "run" (length args)))
          (apply f args))
        ((eql mverb (e-util:mangle-verb "tuple" (length args)))
          (multiple-value-call #'vector (apply f args)))
        (t
          (case mverb
            ((:|__printOn/1|) (destructuring-bind (tw) args
              (e-coercef tw +the-text-writer-guard+)
              (e. tw |print| "<native function ")
              (e. tw |quote| (nth-value 2 (function-lambda-expression f)))
              (e. tw |print| ">")
              nil))
            ((:|__respondsTo/2|) (destructuring-bind (verb arity) args
              (e-coercef verb 'string)
              (e-coercef arity 'integer)
              (as-e-boolean
                (or (and (or (string= verb "run") 
                             (string= verb "tuple"))
                         (e-util:function-responds-to f arity))
                    (e-is-true (elib:miranda #'wrapper mverb args nil))))))
            ; XXX would it hurt to have elib:miranda provide the magic verb?
            ((elib:audited-by-magic-verb) (destructuring-bind (auditor) args
              (not (not (find auditor stamps :test #'eeq-is-same-ever)))))
            (otherwise
              (elib:miranda #'wrapper mverb args (lambda ()
                (error "No such method on wrapped function: ~A" mverb)))))))))
    #'wrapper))

(defun make-symbol-accessor (symbol)
  (e-named-lambda "org.cubik.cle.prim.lisp$symbolAccessor"
    (:|__printOn/1| (tw)
      (e-coercef tw +the-text-writer-guard+)
      (e. tw |print| "<" symbol ">")
      nil)
    (:|getValue/0| ()
      "CL:SYMBOL-VALUE"
      (symbol-value symbol))
    (:|getFunction/0| ()
      "CL:SYMBOL-FUNCTION with a wrapper"
      (wrap-function (symbol-function symbol)))
    (:|setValue/1| (new)
      "(SETF CL:SYMBOL-VALUE)"
      (setf (symbol-value symbol) new)
      nil)))

(defvar +lisp+ (e-named-lambda "org.cubik.cle.prim.lisp"
  "This object is the maximum possible authority that an E program may hold, offering near-complete access to the underlying Lisp system. Handle with care."
  (:|get/2| (package-name symbol-name)
    ; xxx should allow ejectors/absent-thunks for missing package and symbol
    "Returns the named symbol in the named package. This is CL:FIND-SYMBOL, except that it throws an exception instead of returning nil, and takes the package argument first."
    (e-coercef package-name 'string)
    (e-coercef symbol-name 'string)
    (unless (find-package package-name)
      (error "the package ~S does not exist" package-name))
    (multiple-value-bind (symbol status) (find-symbol symbol-name package-name)
      (if status
        (make-symbol-accessor symbol)
        (error "the symbol ~S does not exist in ~S" symbol-name package-name))))))

; --- scope construction utilities ---

; XXX #+eventually-frozen-path-loader is not expected to be in *features* - because this code isn't working yet - it's just a descriptive commenting-out 

(defvar +the-make-path-loader+ (e-named-lambda "org.cubik.cle.prim.makePathLoader"
  (:|run/2| (name fetchpath
      &aux #+eventually-frozen-path-loader (eventually-deep-frozen (e. (e. (vat-safe-scope *vat*) |get| "DeepFrozen") |eventually|)))
    (e-coercef name 'string)
    (e-coercef fetchpath 'vector)
    (with-result-promise (loader)
      (elib:e-named-lambda "org.cubik.cle.prim.makePathLoader$loader"
        ; :stamped (deep-frozen-if-every fetchpath)
        #+eventually-frozen-path-loader :stamped
        #+eventually-frozen-path-loader eventually-deep-frozen
        (:|__printOn/1| (tw)
          (e-coercef tw +the-text-writer-guard+)
          (e. tw |print| "<" name ":*>")
          nil)
        #+eventually-frozen-path-loader (:|__optSealedDispatch/1| (brand)
          ; XXX this implementation of the EventuallyDeepFrozen state check is not really *correct* per the auditor's rules, but will handle the correct cases, since we know that the parts of this which are not actually DeepFrozen 
          (cond
            ((eeq-is-same-ever brand (e. eventually-deep-frozen |getPeekBrand|))
              (e. (e. eventually-deep-frozen |getPeekSealer|) 
                  |seal|
                  (e. +the-make-const-map+ |fromPairs|
                    `#(#("&fetchpath" ,(make-instance 'e-simple-slot :value fetchpath))))))))
        (:|fetch/2| (fqn absent-thunk)
          (if (string= ".*" fqn :start2 (- (length fqn) 2))
            (e. (e. (e. (vat-safe-scope *vat*) |get| "import__uriGetter") |get| "org.erights.e.elang.interp.makePackageLoader") |run| loader (concatenate 'string name ":") fqn)
            (loop for sub across fetchpath
                  do (block continue (return (e. sub |fetch| fqn (e-named-lambda "org.cubik.cle.prim.makePathLoader$loader$continueSearchThunk" (:|run/0| () (return-from continue))))))
                  finally (e. absent-thunk |run|))))
        (:|get/1| (fqn) 
          (e. loader |fetch| fqn 
            (e-named-lambda "org.cubik.cle.prim.makePathLoader$loader$getFailureThunk" (:|run/0| () (error "~A can't find ~A" (e-quote loader) (e-quote fqn))))))
        (:|optUncall/1| (specimen)
          (loop for sub across fetchpath thereis
            (and (e-is-true (e. sub |__respondsTo| "optUnget" 1))
                 (progn
                   ;(format t "~&; ~A for optUnget of ~A querying sub ~A~%" (e-quote loader) (e-quote specimen) (e-quote sub))
                   (unget-to-uncall loader (e. sub |optUnget| specimen))))))
        (:|optUnget/1| (specimen)
          ; xxx this is how Java-E does it, and claims a justification, but *what*?
          (uncall-to-unget loader (e. loader |optUncall| specimen))))))))

(defvar *emaker-search-list*
  (list (merge-pathnames
    (make-pathname :directory '(:relative "lib"))
    (asdf:component-pathname (asdf:find-system :cl-e))))
  ; XXX we could test for this failure mode, or we could make load-emaker-without-cache always cache filesystem state (i.e. source code or not-found) to ensure a consistent view. (The "without-cache" part of the name refers to result-of-evaluation cache, which requires such things as DeepFrozen auditing.)
  "This variable is expected to be modified by startup code such as clrune, but should not be modified afterward to preserve the pseudo-deep-frozenness of <import>.")

(defun fqn-to-relative-pathname (fqn)
  (let* ((pos (or (position #\. fqn :from-end t) -1))
         (prefix-dirs (split-fqn-prefix (subseq fqn 0 (1+ pos)))))
    (make-pathname :directory (cons :relative prefix-dirs)
                   :name (subseq fqn (1+ pos))
                   :type "emaker")))

; XXX threading: global state
(defvar *emaker-load-counts* (make-hash-table :test #'equal))

(defun load-emaker-without-cache (fqn absent-thunk)
  (let* ((subpath (fqn-to-relative-pathname fqn)))
    (loop for root in *emaker-search-list*
          for path = (merge-pathnames subpath root)
          when (probe-file path)
            return (progn
              ;(when *trace-emaker-loading*
              ;  (format *trace-output* "~&; loading emaker ~A~%" fqn))
              (incf (gethash fqn *emaker-load-counts* 0))
               ;XXX replace read-entire-file with file-getter access
              (elang:eval-e 
                (e.syntax:e-source-to-tree
                  (e.extern:read-entire-file path))
                (e. (vat-safe-scope *vat*) 
                    |withPrefix| (concatenate 'string fqn "$"))))
          finally (e. absent-thunk |run|))))

; XXX move this macro elsewhere
(defmacro lazy-value-scope ((fqn-form prefix) &body entries)
  (declare (string prefix))
  `(make-scope ,fqn-form (list
    ,@(loop for (name value-form) in entries collect
      `(list ,(concatenate 'string "&" prefix (string name))
             (make-lazy-apply-slot (lambda () ,value-form)))))))

; xxx review which of these ought to be moved to safe-extern-scope
(defun make-primitive-loader ()
  (lazy-value-scope ("org.cubik.cle.prim.primLoader" "org.cubik.cle.prim.")
    (:|loop|             +the-looper+)
    (:|throw|            +the-thrower+)
  
    (:|int|              (make-instance 'cl-type-guard :type-specifier 'integer))
    (:|ConstList|        (make-instance 'cl-type-guard :type-specifier 'vector)) ; XXX and not string?
    (:|ConstMap|         elib:+the-map-guard+) ; used by nonprimitive map guard
    (:|makeScope|        +the-make-scope+)
    (:|makeStaticScope|  elang:+the-make-static-scope+)
    (:|makeSafeScope|    (wrap-function #'make-safe-scope))
    (:|makeTextWriter|   elib:+the-make-text-writer+)
    (:|makeTraversalKey| elib:+the-make-traversal-key+)
    
    (:|makeTypeDesc|     +the-make-type-desc+)
    (:|makeMessageDesc|  +the-make-message-desc+)
    (:|makeParamDesc|    +the-make-param-desc+)

    (:|makeArray|        e.elib.tables:+the-make-array+)
    (:|makeFlexMap|      elib:+the-make-flex-map+)
    
    (:|FlexList|         elib:+the-flex-list-guard+)
    (:|Throwable|        elib:+the-exception-guard+)
    (:|TraversalKey|     (make-instance 'cl-type-guard :type-specifier 'elib:traversal-key))
    
    (:|equalizer|        (make-equalizer))
    (:|E|                elib:+the-e+)
    (:|Ref|              (e-slot-get +e-ref-kit-slot+)) ; XXX reduce indirection
    
    (:|DeepFrozen|
      (e. (load-emaker-without-cache "org.erights.e.elib.serial.DeepFrozenAuthor" (e-named-lambda "org.erights.e.elib.prim.safeScopeDeepFrozenNotFoundThunk" (:|run/0| () (error "DeepFrozenAuthor missing")))) 
          |run| 
          elib:+deep-frozen-stamp+
          elib:+the-make-traversal-key+))
    
    (:|makeConstSet|
      (e. (e. (e. (vat-safe-scope *vat*) |get| "import__uriGetter") |get| "org.erights.e.elib.tables.makeConstSetAuthor") 
          |run| elib:+selfless-stamp+))
    
    (:|makeBaseGuard|
      (e. (e. (e. (vat-safe-scope *vat*) |get| "import__uriGetter") |get| "org.erights.e.elib.slot.makeBaseGuardAuthor") 
          |run| elib:+deep-frozen-stamp+ elib:+selfless-stamp+))))

; XXX ++
(defvar vm-node-maker-importer
  (let* ((prefix "org.erights.e.elang.evm."))
    (e-named-lambda "vm-node-maker-importer"
      :stamped +deep-frozen-stamp+
      (:|fetch/2| (fqn absent-thunk
          &aux (local-name (e.util:without-prefix fqn prefix)))
        (if local-name
          (let* ((sym (find-symbol local-name :e.elang.vm-node)))
            (or (and sym
                     (get sym 'static-maker))
                (e. absent-thunk |run|)))
          (e. absent-thunk |run|)))
      (:|optUnget/1| (specimen)
        ; XXX O(N) not good - have elang-nodes.lisp build a hash table of makers at load time
        (block opt-unget
          (do-symbols (node-type (find-package :e.elang.vm-node))
            (when (eeq-is-same-yet specimen (get node-type 'static-maker))
              (return-from opt-unget (concatenate 'string prefix (string node-type)))))
          nil)))))

; XXX ++
; XXX optUnget?
(defvar vm-node-type-importer (e-named-lambda "vm-node-type-importer"
  :stamped +deep-frozen-stamp+
  (:|fetch/2| (fqn absent-thunk
      &aux (local-name (e.util:without-prefix fqn "org.erights.e.elang.evm.type.")))
    (if local-name
      (let* ((sym (find-symbol local-name :e.elang.vm-node)))
        (if sym
          (make-instance 'cl-type-guard :type-specifier sym)
          (e. absent-thunk |run|)))
      (e. absent-thunk |run|)))))

; XXX move this utility function elsewhere
; XXX avoiding loading deep-frozen auditor because we need this during the construction of the path loader - so we reject some we could accept. the Right Solution is to add lazy auditing as MarkM suggested
(defun deep-frozen-if-every (subs
    &aux #-(and) (deep-frozen-guard (e. (vat-safe-scope *vat*) |get| "DeepFrozen")))
  (if (every (lambda (x)
               #-(and) (e-is-true (e. deep-frozen-guard |isDeepFrozen| x))
               (e-is-true (e. +the-audit-checker+ |run| +deep-frozen-stamp+ x))) subs)
    +deep-frozen-stamp+
    (e-named-lambda "org.cubik.cle.prim.deepFrozenIfEveryStubAuditor" (:|audit/2| (constantly +e-false+)))))

; this could be less primitive, but then it would have more dependencies
(defvar +traceln+ (elib:e-named-lambda "org.cubik.cle.prim.traceln"
  (:|run/1| (message)
    ; xxx use stream writer on *trace-output*?
    ; xxx Should we tag traceln according to the FQN-prefix of the safe scope, or in nested scopes such as emakers' FQN-prefix-scopes?
    (format *trace-output* "~&~A"
      (with-text-writer-to-string (tw)
        (e. tw |print|      "; trace: ")
        (e. (e. tw |indent| ";        ") |print| message)
        (e. tw |println|))))))

; XXX merge these two: make lazy-apply robust in the presence of failure, and make lazy-eval use lazy-apply with a particular thunk

(defun make-lazy-eval-slot (scope source &aux value this)
  (setf this (e-named-lambda "LazyEvalSlot"
    :stamped +deep-frozen-stamp+
    (:|getValue/0| ()
      (when source
        (let ((source-here source))
          (multiple-value-bind (promise resolver) (make-promise)
            (setf value  promise
                  source nil)
            (handler-case
              (progn
                (setf value (elang:eval-e (e.syntax:e-source-to-tree source-here) (e. scope |withPrefix| (format nil "~A<lazy-eval>$" (e. scope |getFQNPrefix|)))))
                (e. resolver |resolve| value))
              (error (p)
                (e. resolver |smash| p))))))
      value)
    (:|setValue/1| (new)
      (declare (ignore new))
      (error "not an assignable slot: ~A" (e-quote this)))
    (:|isFinal/0| () elib:+e-true+))))

(defun make-lazy-apply-slot (maker &aux value-box)
  (e-named-lambda "lazyApplySlot"
    :stamped +deep-frozen-stamp+
    (:|getValue/0| ()
      (unless value-box
        (multiple-value-bind (promise resolver) (make-promise)
          ; XXX doesn't handle failure
          (setf value-box (list promise))
          (e. resolver |resolve| (funcall maker))))
      (car value-box))
    (:|isFinal/0| () elib:+e-true+)))

(defun make-safe-extern-loader ()
  (lazy-value-scope ("__cle_safe_extern" "")
    ("org.apache.oro.text.regex.Perl5Compiler" e.extern:+rx-perl5-compiler+)
    ("org.apache.oro.text.regex.Perl5Matcher"  e.extern:+rx-perl5-matcher+)
    ("org.cubik.cle.prim.parser"               e.syntax:+prim-parser+)
    ("org.cubik.cle.prim.ePrinter"             e.syntax:+e-printer+)
    ("org.cubik.cle.prim.makeFirstCharSplitter" +make-first-char-splitter+)
    ("org.cubik.cle.prim.makePathLoader"       +the-make-path-loader+)
    ("org.cubik.cle.prim.simplifyFQName" 
      (e-named-lambda "org.cubik.cle.prim.simplifyFQName" 
        :stamped +deep-frozen-stamp+
        (:|run/1| (x) (elib:simplify-fq-name (elib:e-coerce x 'string))))) ; XXX replace this with wrap-function
    ))

(defun f+ (f1 f2)
  (lambda (&rest args)
    (multiple-value-call f1 (apply f2 args))))

; XXX simplify the amount of wrapping this requires / make those of these primitives which are safe (all of them?) importable
(defvar +e-ref-kit-slot+ (make-lazy-apply-slot (lambda ()
  (e. (load-emaker-without-cache
        "org.erights.e.elib.ref.RefAuthor" 
        (e-named-lambda "org.erights.e.elib.prim.RefAuthorNotFoundThunk" (:|run/0| () (error "RefAuthor missing")))) 
      |run|
      (wrap-function (f+ #'vector #'make-promise)
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function #'ref-state
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function (f+ #'as-e-boolean #'ref-is-resolved)
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function (f+ #'as-e-boolean #'eeq-is-settled)
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function #'make-unconnected-ref
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function (lambda (ref)
        (as-e-boolean (typep ref 
          '(or (satisfies elib:eeq-is-transparent-selfless)
               null
               string
               character
               integer
               float64
               elib:e-boolean))))
                     :stamps (list +deep-frozen-stamp+))
      (wrap-function #'ref-opt-problem
                     :stamps (list +deep-frozen-stamp+))
      'elib:broken
      'elib:near
      'elib:eventual
      +deep-frozen-stamp+))))

(defun default-safe-scope-roots ()
  (let* ((emaker-importer
           (let ((deep-frozen-cache (make-hash-table :test #'equal)))
             (e-named-lambda "org.cubik.cle.internal.emakerImporter"
               :stamped +deep-frozen-stamp+
               (:|fetch/2| (fqn absent-thunk)
                 (e-coercef fqn 'string)
                 (multiple-value-bind (cache-value cache-present) (gethash fqn deep-frozen-cache)
                   (if cache-present
                     cache-value
                     (let ((result (load-emaker-without-cache fqn absent-thunk)))
                       (when (e-is-true (e. (e. (vat-safe-scope *vat*) |get| "DeepFrozen") |isDeepFrozen| result))
                         (setf (gethash fqn deep-frozen-cache) result))
                       result))))))))
    (make-scope "__defaultSafeScopeRoots.thisShouldNotBeVisible$"
      `(("&import__uriGetter"  ,(make-lazy-apply-slot (lambda ()
          ; wrapper to provide stamped DeepFrozenness since we can't currently do it 'properly' without dependency cycles
          (let ((real-loader 
                  (e. +the-make-path-loader+ |run| "import" (vector 
                    (make-primitive-loader)
                    (make-safe-extern-loader)
                    emaker-importer
                    vm-node-type-importer
                    vm-node-maker-importer))))
            (e-named-lambda "org.cubik.cle.prim.ImportLoaderMagic"
              :stamped +deep-frozen-stamp+
              (:|__printOn/1| (tw) (e. real-loader |__printOn| tw))
              (otherwise (mverb &rest args)
                (apply #'e-call-dispatch real-loader mverb args)))))))))))
 
(defun make-safe-scope (&optional (fqn-prefix "__safe$") (roots (default-safe-scope-roots))
    &aux (&<import> (e. roots |getSlot| "import__uriGetter")))
  (with-result-promise (safe-scope-vow)
    (labels ((typical-lazy (source)
               (make-lazy-eval-slot safe-scope-vow source))
             (lazy-import (fqn)
               (make-lazy-apply-slot (lambda () (e. (e-slot-get &<import>) |get| fqn)))))
      (make-scope fqn-prefix
      
        `(; --- self-referential / root ---
          ("safeScope"  ,safe-scope-vow)
          ("&import__uriGetter"  ,&<import>)

          ; --- primitive: values not available as literals ---
          ; XXX true can be defined as (0 =~ _), and false as (!true) or (0 =~ []). Do we want to do so?
          ("null"       ,nil)
          ("false"      ,elib:+e-false+)
          ("true"       ,elib:+e-true+)
          ("NaN"        ,elib:|NaN|)
          ("Infinity"   ,elib:|Infinity|)
          
          ; --- primitive: guards ---
          ("any"        ,(make-instance 'cl-type-guard :type-specifier 't))
          ("void"       ,elib:+the-void-guard+)
      
          ; --- primitive: flow control ---
          ("&E"         ,(lazy-import "org.cubik.cle.prim.E")) ; XXX should be interp.E
          ("&throw"     ,(lazy-import "org.cubik.cle.prim.throw")) ; XXX should be elang.interp.throw
          ("&__loop"    ,(lazy-import "org.cubik.cle.prim.loop")) ; XXX should be elang.interp.loop
          
          ; --- primitive: tracing ---
          ("traceln"    ,+traceln+)

          ; --- nonprimitive flow control ---
          ("&require"   ,(lazy-import "org.erights.e.elang.interp.require"))
          
          ; --- data constructors ---
          ("__makeFinalSlot"     ,elib:+the-make-simple-slot+)
          ("__makeInt"           ,elib:+the-make-int+)
          ("__makeList"          ,elib:+the-make-list+)
          ("__makeMap"           ,elib:+the-make-const-map+)
          ("__makeTwine"         ,elib:+the-make-twine+)
          ("&__makeOrderedSpace" ,(lazy-import "org.erights.e.elang.coord.OrderedSpaceMaker"))
          ("&term__quasiParser"  ,(typical-lazy "<import:org.quasiliteral.quasiterm.makeQBuilder>.getTerm__quasiParser()"))
      
          ; --- data guards: atomic ---
          ("boolean"    ,(make-instance 'cl-type-guard :type-specifier 'e-boolean))
          ("&int"       ,(typical-lazy "__makeOrderedSpace(<import:org.cubik.cle.prim.int>, \"int\")"))
          ("float64"    ,(make-instance 'cl-type-guard :type-specifier 'float64))
          ("char"       ,(make-instance 'cl-type-guard :type-specifier 'character))    
          ("String"     ,(make-instance 'cl-type-guard :type-specifier 'string))
          ("Twine"      ,(make-instance 'cl-type-guard :type-specifier 'elib:twine))
          ("TextWriter" ,elib:+the-text-writer-guard+)
          
          ; --- data guards: nonatomic, nonprimitive ---
          ("&List"        ,(lazy-import "org.erights.e.elib.slot.List"))
          ("&Map"         ,(lazy-import "org.erights.e.elib.slot.Map"))
          ("&Set"         ,(lazy-import "org.erights.e.elib.tables.type.ConstSet"))
          ("&Tuple"       ,(lazy-import "org.erights.e.elib.slot.Tuple"))
          ("&__Portrayal" ,(typical-lazy "Tuple[any, String, List[any]]"))
  
          ; --- protocol/guard constructors ---
          ("&__makeProtocolDesc" ,(lazy-import "org.erights.e.elang.interp.makeProtocolDesc"))
          ("&__makeMessageDesc"  ,(lazy-import "org.erights.e.elib.base.makeMessageDesc"))
          ("&__makeParamDesc"    ,(lazy-import "org.erights.e.elib.base.makeParamDesc"))
          
          ; --- utility: guard meta ---
          ("&__makeGuard" ,(lazy-import "org.erights.e.elib.slot.makeBaseGuard"))
          ; The ValueGuard and Guard guards do not currently reject anything, but this may change (e.g. DeepFrozen)
          ("&ValueGuard"  ,(lazy-import "org.erights.e.elib.slot.type.ValueGuard"))
          ("&Guard"       ,(lazy-import "org.erights.e.elib.slot.type.Guard"))
      
          ; --- primitive: reference operations ---        
          ("__auditedBy" ,+the-audit-checker+)
          ("&Ref"        ,(lazy-import "org.cubik.cle.prim.Ref")) ; XXX should be elib.ref.Ref
          ("&__equalizer",(lazy-import "org.cubik.cle.prim.equalizer")) ; XXX should be elib.tables.equalizer
          ("&DeepFrozen" ,(lazy-import "org.cubik.cle.prim.DeepFrozen")) ; XXX fqn
          
          ; --- utility: guards ---
          ("&nullOk"    ,(lazy-import "org.erights.e.elib.slot.nullOk"))
          ("&notNull"   ,(lazy-import "org.erights.e.elang.interp.notNull"))
          
          ; --- utility: reference conditions ---
          ("&PassByCopy" ,(lazy-import "org.erights.e.elib.serial.PassByCopy"))
          ("&near"       ,(lazy-import "org.erights.e.elib.slot.near"))
          ("&vow"        ,(lazy-import "org.erights.e.elang.interp.vow"))
          ("&rcvr"       ,(lazy-import "org.erights.e.elang.interp.rcvr"))
          
          ; --- E-syntax ---
          ("&e__quasiParser" ,(lazy-import "org.erights.e.elang.syntax.makeEParser"))
          ("&epatt__quasiParser" ,(lazy-import "org.erights.e.elang.syntax.epatt__quasiParser"))
          
          ; --- utility: data ---
          ("&simple__quasiParser" ,(lazy-import "org.quasiliteral.text.simple__quasiParser"))
          ("&rx__quasiParser" ,(lazy-import "org.erights.e.elang.interp.PerlMatchMakerMaker"))
              
          ; --- utility: alias loaders ---
          ("&elib__uriGetter"   ,(lazy-import "org.erights.e.elib.*"))
          ("&elang__uriGetter"  ,(lazy-import "org.erights.e.elang.*"))
          ("&type__uriGetter"   ,(lazy-import "org.erights.e.elang.interp.typeLoader"))
          
          ; --- utility: EIO ---
          ("&EIO"               ,(lazy-import "org.erights.e.elib.eio.EIO"))

          ; --- utility: miscellaneous ---
          ("&__comparer"        ,(lazy-import "org.erights.e.elang.interp.comparer"))
          ("&__identityFunc"    ,(typical-lazy "def identityFunc(x) :any { return x }"))
          ("__MatchContext"     ,(e-named-lambda "org.erights.e.elib.slot.MatchContext"
            (:|coerce/2| (specimen opt-ejector)
              (vector specimen opt-ejector))))
          ("&opaque__uriGetter" ,(lazy-import "org.erights.e.elib.serial.opaque__uriGetter"))
          ("&__makeVerbFacet" ,(lazy-import "org.erights.e.elang.interp.__makeVerbFacet"))
          
          ; --- user/REPL ---
          ("&help"              ,(lazy-import "org.erights.e.elang.interp.help")))))))

(defun make-io-scope (&key ((:stdout out-cl-stream)) ((:stderr error-cl-stream)))
  (let ((vat-priv-scope
          (e. (make-scope "__localPrivileged$"
                ; NOTE: these stamps are process-wide, but their effect is local unless amplified with some authority allowing sharing objects between vats (e.g. a hypothetical thread-safe stamp)
                ; XXX makeProxyResolver will eventually convey GC-notification authority and so is arguably not a *local* privilege.
                `(("DeepFrozenStamp"   ,elib:+deep-frozen-stamp+)
                  ("SelflessStamp"     ,elib:+selfless-stamp+)
                  ("makeProxyResolver" ,elib:+the-make-proxy-resolver+)))
            |or| (vat-safe-scope *vat*))))
    (e. (e. (e. (vat-safe-scope *vat*) |get| "import__uriGetter") 
            |get|
            "org.cubik.cle.makeIOScope")
        |run| 
        "__privileged$"
        vat-priv-scope
        (e. +the-make-const-map+ |fromPairs| 
          `#(#("timer"           ,e.extern:+the-timer+)
             #("file__uriGetter" ,(e.extern:make-file-getter '#()))
             #("gc"              ,e.extern:+gc+)
             #("makeWeakRef"     ,+the-make-weak-ref+)
             #("stdin"      ,(e-named-lambda "fake-stdin"))
             #("stdout"     ,(make-text-writer-to-cl-stream
                              out-cl-stream
                              :autoflush t
                              :should-close-underlying nil))
             #("stderr"     ,(make-text-writer-to-cl-stream
                              error-cl-stream
                              :autoflush t
                              :should-close-underlying nil))
             #("lisp"       ,+lisp+)
                            ; XXX should use e-extern's pathname-to-E-style-path facilities
             #("props"      ,(e. +the-make-const-map+ |fromPairs|
                              `#(#("e.home" ,(namestring (asdf:component-pathname (asdf:find-system :cl-e))))))))))))

