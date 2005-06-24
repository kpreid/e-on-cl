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


(defmacro with-vat (&body forms)
  `(progn
    (assert (null *vat*))
    (let ((*vat* (make-instance 'vat)))
      ,@forms)))

