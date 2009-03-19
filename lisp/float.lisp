; Copyright 2005-2009 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

;;; --- Vtables ---

(def-vtable float
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (e. tw |print| 
      (with-standard-io-syntax
        (let ((*read-default-float-format* 'double-float)
              (*print-readably* nil)) ; for clisp
          (prin1-to-string this)))))
  (:|isNaN| (this)
    (declare (ignore this))
    +e-false+))

(def-vtable (eql #.|NaN|)
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (declare (ignore this))
    (e. tw |print| "NaN"))
  (:|isNaN| (this)
    (declare (ignore this))
    +e-true+))

(def-vtable (eql #.|Infinity|)
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (declare (ignore this))
    (e. tw |print| "Infinity"))
  (:|isNaN| (this)
    (declare (ignore this))
    +e-false+))

(def-vtable (eql #.|-Infinity|)
  (:|__printOn| (this (tw +the-text-writer-guard+))
    (declare (ignore this))
    (e. tw |print| "-Infinity"))
  (:|isNaN| (this)
    (declare (ignore this))
    +e-false+))

;;; --- Float parser ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; cl-yacc executes the action forms a little too soon

  (defun fp-float (sign mantissa-hi mantissa-lo exponent)
    "float parser action"
    (* sign
      (float ;; XXX this constructs a possibly-big rational before converting to float
        (* (+ (* (expt 10 (second mantissa-lo)) (first mantissa-hi)) 
              (first mantissa-lo))
           (expt 10 (- exponent (second mantissa-lo)))) 
        1.0d0)))

  (defun fp-digits (digits digit)
    "float parser action"
    (destructuring-bind (value count) digits
      (list (+ (* 10 value) (digit-char-p digit)) (1+ count))))

  (defun fp-digits-init ()
    "float parser action"
    '(0 0))

  (defun second-arg (a b)
    "float parser action"
    (declare (ignore a))
    b)

  (defun fp-exponent (marker sign magnitude)
    "float parser action"
    (declare (ignore marker))
    (* sign (first magnitude)))
    
  (defun fp-infinity (sign &rest text)
  "float parser action"
    (declare (ignore text))
    (* sign |Infinity|))
  
  #| eval-when |#)

(yacc:define-parser +float-parser+
  (:start-symbol float64)
  (:terminals (:0 :1 :2 :3 :4 :5 :6 :7 :8 :9
               :|e| :E :+ :- :.
               :N :|a|
               :|I| :|n| :|f| :|i| :|t| :|y|))
  
  (float64
    (sign digits fractional exponent #'fp-float)
    (sign :N :|a| :N (constantly |NaN|))
    (sign :|I| :|n| :|f| :|i| :|n| :|i| :|t| :|y| #'fp-infinity))

  (fractional ; returns digits structure
   (:. digits #'second-arg)
   (#'fp-digits-init))
  
  (exponent ; returns integer exponent
   (expflag sign digits #'fp-exponent)
   ((constantly 0)))
 
  (digits ; returns digits structure (integer value and number of digits)
   (digits digit #'fp-digits)
   (#'fp-digits-init))

  (sign ; optional sign: returns 1 or -1
    (:+ (constantly 1)) 
    (:- (constantly -1))
    ((constantly 1)))
  (expflag :|e| :|E|)
  (digit :0 :1 :2 :3 :4 :5 :6 :7 :8 :9))

;;; --- Float maker ---

(defobject +the-make-float64+ "org.erights.e.elib.atom.makeFloat64" 
    (:stamped +deep-frozen-stamp+)
  (:|run| ((string 'string))
    (let ((i 0))
      (yacc:parse-with-lexer
        (lambda () 
          (if (< i (length string))
            (let ((v (elt string i)))
              (incf i)
              (values (intern (string v) :keyword) v))
            (values nil nil))) 
        +float-parser+))))
