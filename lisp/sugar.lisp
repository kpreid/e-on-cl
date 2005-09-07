; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

; XXX not fully tested
; get-setf-expansion usage provided by Robert J. Macomber ("Thas") on <irc://irc.freenode.net/%23lisp> 2005-03-25; used with permission
(defmacro place-slot (place &environment environment)
  "Return an E Slot accessing the given place. The place's subforms will be evaluated immediately once."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    `(let* (,@(mapcar #'list vars vals))
      (e-lambda "org.cubik.cle.prim.PlaceSlot" ()
        (:|getValue| ()             ,reader-form)
        (:|setValue| (,@store-vars) ,writer-form nil)
        (:|readOnly| ()
          (e-lambda "org.cubik.cle.prim.ReadOnlyPlaceSlot" ()
            (:|getValue| ()         ,reader-form)))))))


(defun e-slot-value (slot)
  "Together with (setf e-slot-value), this function allows accessing an E Slot as a CL place."
  (e. slot |getValue|))

(defun (setf e-slot-value) (new slot)
  (e. slot |setValue| new))


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


; I wrote this and found I didn't need it, but here it is in case it's useful later. Untested.
;(defmacro escape ((ejector-var) &body forms
;    &aux (tag-var (gensym)))
;  `(let ((,tag-var (gensym)))
;    (catch ,tag-var
;      (let ((,ejector-var (make-instance 'ejector :catch-tag ,tag-var)))
;        ,@forms))))

(defun call-with-vat (function &rest initargs)
  (assert (null *vat*))
  ; xxx eventually we will need a shutdown operation on the vat to break inter-vat refs, do some sort of shutdown on registered input streams, etc.
  (let ((*vat* (apply #'make-instance 'vat initargs)))
    (funcall function)
    (vat-loop)))

(defmacro with-vat ((&rest initargs) &body start-forms)
  `(call-with-vat (lambda () ,@start-forms) ,@initargs))

(defmacro when-resolved ((result-var) ref-form &body forms)
  "Execute the body forms when the value of ref-form becomes resolved. Returns a promise for the value of the last form.

The syntax is imitative of cl:multiple-value-bind - suggestions for better syntax welcome."
  `(e. (e. (vat-safe-scope *vat*) |get| "Ref") 
       |whenResolved| ,ref-form 
       (efun (,result-var)
         ,@forms)))

(defmacro mapping-bind (map-form (&body entries) &body body
    &aux (map-var (gensym "MAP")))
  "Equivalent of an E map pattern, vaguely like DESTRUCTURING-BIND and LET*. Defaults are evaluated in LET* style."
  `(let* ((,map-var ,map-form)
          ,@(loop
              for (var key-form . default-cell) in entries
              for pair-var   = (gensym "EXTRACTION")
              for cookie-var = (gensym "COOKIE")
              for key-var    = (gensym "KEY")
              append 
              `(,@(when default-cell
                  `((,cookie-var (make-symbol "COOKIE"))))
                (,key-var  ,key-form)
                (,pair-var ,(if default-cell
                              `(e. ,map-var |extract| ,key-var ,cookie-var)
                              `(e. ,map-var |optExtract| ,key-var)))
                (,var      (progn
                             (unless (ref-shorten ,pair-var)
                               (error "needs a mapping for ~A, got ~A"
                                 (e-quote ,key-var)
                                 (e-quote ,map-var)))
                             (e. ,pair-var |get| 0)))
                ,@(when default-cell
                  `((,var (if (eq (ref-shorten ,var) ,cookie-var) 
                            ,(first default-cell) 
                            ,var))))
                (,map-var  (e. ,pair-var |get| 1)))))
    (unless (= 0 (e-coerce (e. ,map-var |size|) '(integer 0)))
      (error "unexpected map entries ~A" (e-quote ,map-var)))
    ,@body))

(defun e-import (fqn)
  "Retrieve an object for the given FQN in the current vat's importer; in E notation, <import>[fqn]."
  (e-coercef fqn 'string)
  (e. (e. (vat-safe-scope *vat*) 
          |get| 
          "import__uriGetter")
      |get|
      fqn))

