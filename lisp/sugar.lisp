; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defmacro defobject (symbol qualified-name (&rest options) &body methods)
  "The common pattern of a single-instance object, marked thread-sharable, stored in some global variable (or constant). Does not specify deep-frozen, as e.g. e.knot:+lisp+ is not."
  `(defglobal ,symbol 
    (e-lambda ,qualified-name
              (:stamped +thread-sharable-stamp+ ,@options)
              ,@methods)))

(declaim (inline eelt efuncall))
(defun eelt (container &rest indexes)
  "Like CL:ELT, but for E containers. Equivalent to the idiom (e. container |get| index...)"
  (e-call container "get" indexes))
(defun efuncall (function &rest args)
  "Like CL:FUNCALL, but for E functions. Equivalent to the idiom (e. function |run| arg...)"
  (e-call function "run" args))
(define-compiler-macro eelt (container &rest indexes)
  `(e. ,container "get" ,@indexes))
(define-compiler-macro efuncall (function &rest args)
  `(e. ,function "run" ,@args))

; XXX not fully tested
; get-setf-expansion usage provided by Robert J. Macomber ("Thas") on <irc://irc.freenode.net/%23lisp> 2005-03-25; used with permission
(defmacro place-slot (place &environment environment)
  "Return an E Slot accessing the given place. The place's subforms will be evaluated immediately once."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    `(let* (,@(mapcar #'list vars vals))
      (e-lambda "org.cubik.cle.prim.PlaceSlot" ()
        (:|get| ()             ,reader-form)
        (:|put| (,@store-vars) ,writer-form nil)
        (:|readOnly| ()
          (e-lambda "org.cubik.cle.prim.ReadOnlyPlaceSlot" ()
            (:|get| ()         ,reader-form)))))))


(defun e-slot-value (slot)
  "Together with (setf e-slot-value), this function allows accessing an E Slot as a CL place."
  (e. slot |get|))

(defun (setf e-slot-value) (new slot)
  (e. slot |put| new))


(defmacro def-shorten-methods (gf-name arity)
  "Define methods on the generic function 'gf-name' such that if any of its arguments is an E ref, it will be re-callled with the ref shortened. If the ref is  not NEAR, an error will be signaled."
  `(progn ,@
    (loop for i below arity collect
      (loop
        for j below arity
        for shortening = (= i j)
        for sym = (gensym)
        collect `(,sym ,(if shortening 'e.elib::ref 't)) into params
        collect (if shortening 
                  `(let ((v (ref-shorten ,sym)))
                    (assert (typep v '(not e.elib::ref)) (v)
                            "Argument ~A to ~S must be near." ',j ',gf-name)
                    v)
                  sym) into args
        finally (return
          `(defmethod ,gf-name ,params
            (funcall ',gf-name ,@args)))))))


(defmacro escape ((ejector-var) &body forms)
  (let ((escape-block (gensym (symbol-name ejector-var))))
    `(block ,escape-block
      (let ((,ejector-var (ejector ',(symbol-name ejector-var) 
                                   (lambda (v) (return-from ,escape-block v)))))
        ,@forms))))

(defmacro escape-bind ((ejector-var) try-form (result-var) &body catch-forms)
  "Execute TRY-FORM with an ejector bound to EJECTOR-VAR. If the ejector is used, control is transferred to the CATCH-FORMS with the ejector's argument bound to RESULT-VAR. Returns the value of whichever of TRY-FORM or the last of CATCH-FORMS returns."
  (let ((escape-block (gensym "ESCAPE-BIND-BLOCK"))
        (normal-block (gensym "ESCAPE-BIND-NORMAL")))
    `(block ,normal-block
      (let ((,result-var
              (block ,escape-block
                (return-from ,normal-block 
                  (let ((,ejector-var (ejector ',(symbol-name ejector-var) 
                                               (lambda (v) (return-from ,escape-block v)))))
                    ,try-form)))))
        ,@catch-forms))))

(declaim (inline to-condition))
(defun to-condition (defaulted-type datum &rest arguments)
  "Implements CLHS 9.1.2.1."
  (etypecase datum
    (symbol
      (apply #'make-condition datum arguments))
    ((or string function)
      (make-condition defaulted-type :format-control datum :format-arguments arguments))
    (condition
      datum)))

(declaim (inline ejerror))
(defun ejerror (ejector &rest args)
  (eject-or-ethrow ejector (apply #'to-condition 'simple-error args)))

(defun call-with-vat (function &rest initargs)
  (assert (null *vat*))
  ; xxx eventually we will need a shutdown operation on the vat to break inter-vat refs, do some sort of shutdown on registered input streams, etc.
  (let* ((*runner* (make-runner-for-this-thread))
         (vat (apply #'make-instance 'vat :runner *runner* initargs)))
    (let ((*vat* vat))
      (funcall function))
    (top-loop)))

(defmacro with-vat ((&rest initargs) &body start-forms)
  `(call-with-vat (lambda () ,@start-forms) ,@initargs))

(defun call-when-resolved (ref ereactor)
  "implemented here to avoid E-language dependencies - Ref.whenResolved is implemented in E code. XXX review whether Ref should be implemented in terms of this."
  (multiple-value-bind (result result-resolver) (make-promise)
    (let ((safe-reactor
           (with-result-promise (safe-reactor)
             (efun (_)
               (declare (ignore _))
               (if (ref-is-resolved ref)
                 (unless (e-is-true (e. result-resolver |isDone|))
                   (e. result-resolver |resolve| (e<- ereactor |run| ref)))
                 (e-send-only-dispatch ref :|__whenMoreResolved/1| safe-reactor))))))
      (efuncall safe-reactor nil)
      result)))

(defmacro when-resolved ((result-var) ref-form &body forms)
  "Execute the body forms when the value of ref-form becomes resolved. Returns a promise for the value of the last form.

The syntax is imitative of cl:multiple-value-bind - suggestions for better syntax welcome."
  `(call-when-resolved ,ref-form 
                       (efun (,result-var)
                         ,@forms)))

(defmacro mapping-bind (map-form (&body entries) &body body
    &aux (map-var (gensym "MAP"))
         (key-var (gensym "KEY"))
         (ej-var (gensym "EJ"))
         (junk (gensym)))
  "Equivalent of an E map pattern, vaguely like DESTRUCTURING-BIND and LET*. Defaults are evaluated in LET* style."
  `(let* ((,map-var ,map-form)
          ,@(loop
              for (var key-form . default-cell) in entries
              append 
              `((,key-var  ,key-form)
                (,var      (escape-bind (,ej-var)
                               (e. ,map-var |fetch| ,key-var ,ej-var)
                             (,junk)
                               (declare (ignore ,junk))
                               ,(if default-cell
                                  (first default-cell)
                                  `(error "needs a mapping for ~A, got ~A"
                                     ;; XXX avoid print
                                     (e-quote ,key-var)
                                     (e-quote ,map-var)))))
                (,map-var  (e. ,map-var |without| ,key-var)))))
    (unless (zerop (e-coerce (e. ,map-var |size|) '(integer 0)))
      (error "unexpected map entries ~A" (e-quote ,map-var)))
    ,@body))

(defun e-import (fqn)
  "Retrieve an object for the given FQN in the current vat's importer; in E notation, <import>[fqn]."
  (e-coercef fqn 'string)
  (eelt (e. (vat-safe-scope *vat*) |fetch| "import__uriGetter" +the-thrower+)
        fqn))

()
