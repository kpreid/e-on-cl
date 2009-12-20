; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.knot)

;;; --- structured classless exceptions ---

;; xxx this section to be moved?

(define-condition e-structure-exception (error)
  ((types :initarg :types :reader se-types)
   (properties :initarg :properties :reader se-properties)
   (printer :initarg :printer :reader se-printer))
  (:report (lambda (condition stream)
             (let ((tw (make-text-writer-to-cl-stream stream
                         :autoflush nil
                         :should-close-underlying nil)))
               (efuncall (se-printer condition) tw condition)))))

(defun property-name-to-get-verb (name)
  (if (string= name "")
    "get"
    (concatenate 'string "get"
                         (string (aref name 0))
                         (subseq name 1))))

(def-vtable e-structure-exception
  (:|__printOn| (condition (tw +the-text-writer-guard+))
    (e. tw |write| "problem: ")
    (efuncall (se-printer condition) tw condition))
  (:|__getAllegedType| (condition)
    (efuncall +the-make-type-desc+
      "StructureException instance type"
      nil
      (map 'vector
           (lambda (type)
             (efuncall +the-make-type-desc+
               "StructureException autodefined supertype" type #() #() #()))
           (se-types condition))
      #()
      (map 'vector
           (lambda (name)
             (e-coercef name 'string)
             (efuncall +the-make-message-desc+
               "" (property-name-to-get-verb name) #() nil))
           (e. (se-properties condition) |getKeys|))))
  (:|_getProperties/0| 'se-properties))

(defmethod e-call-match (fail (rec e-structure-exception) mverb &rest args)
  (declare (ignore fail))
  (let ((name (without-prefix (unmangle-verb mverb) "get")))
    (if (and name (null args))
      (let ((pname (concatenate 'string
                     (string (char-downcase (aref name 0)))
                     (subseq name 1))))
        (e. (se-properties rec) |fetch| pname (efun () (return-from e-call-match (call-next-method)))))
      (call-next-method))))

(defobject +the-make-exception+ "org.cubik.cle.prim.makeException"
    (:stamped +deep-frozen-stamp+)
  (:|run| ((types 'vector)
           (properties +the-map-guard+)
           (printer (eelt (vat-safe-scope *vat*) "DeepFrozen")))
    (setf types (map 'vector (lambda (x) (e-coercef x 'string)) types))
    (make-condition
      'e-structure-exception
      :types types
      :properties properties
      :printer printer)))


; --- standard scope definitions ---

(defobject +the-e+ "org.erights.e.elib.prim.E"
    (:stamped +deep-frozen-stamp+)
  (:|call| (r (v 'string) (a 'vector))
    (e-call r v a))
  (:|callWithPair| (rec (verb-args '(vector * 2)))
    (e-call rec (aref verb-args 0) (aref verb-args 1)))
  (:|send| (r (v 'string) (a 'vector))
    (e-send r v a))
  (:|sendOnly| (r (v 'string) (a 'vector))
    (apply #'e-send-only-dispatch r (mangle-verb v (length a)) (coerce a 'list)))
  (:|toQuote/1| 'e-quote)
  (:|toString/1| 'e-print))

(defobject +the-looper+ "org.erights.e.elang.interp.loop"
    (:stamped +deep-frozen-stamp+)
  (:|__printOn| ((tw +the-text-writer-guard+))
    (e. tw |print| "<__loop>")
    nil)
  (:|run| (body)
    "Call body.run(), which must return a boolean, until it returns false."
    (loop while (e-is-true (efuncall body)))))

(defun split-fqn-prefix (fqn)
  ; xxx consider replacing with SPLIT-SEQUENCE
  ; XXX write tests for this particular function
  "Return a list of the components of this FQN prefix. XXX need to document precise empty-element and empty-string behavior."
  (let ((pos (position #\. fqn)))
    (if pos
      (cons (subseq fqn 0 pos) (split-fqn-prefix (subseq fqn (1+ pos))))
      (progn
        (assert (string= fqn ""))
        nil))))

(defobject +make-first-char-splitter+
    "org.quasiliteral.text.makeFirstCharSplitter"
    (:stamped +deep-frozen-stamp+)
  ;; In the future, this might become part of something for working with character sets in general, and Unicode character categories. Consider Cocoa's NSCharacterSet and NSScanner.
  (:|run| ((specials 'string))
    (flet ((match (ch) (position ch specials)))
      (e-lambda "org.quasiliteral.text.makeFirstCharSplitter$firstCharSplitter"
          (:stamped +deep-frozen-stamp+)
        (:|__printOn| ((out +the-text-writer-guard+))
          (e. out |write| "<finds any of ")
          (e. out |quote| specials)
          (e. out |write| ">"))
        (:|findIn| ((str 'string))
          "Equivalent to .findInFrom(str, 0)."
          (or (position-if #'match str)
              -1))
        (:|findInFrom| ((str 'string) (start `(integer 0 ,(length str))))
          ; XXX write tests
          "Return the first index greater than 'start' of a character of 'str' which is one of the special characters of this splitter, or -1 if no such index exists."
          (or (position-if #'match str :start start)
              -1))))))

(defobject +the-get-character+ "org.cubik.cle.prim.getCharacter"
    (:stamped +deep-frozen-stamp+)
  (:|run| (codepoint)
    ;; XXX Unicode assumption
    (or (code-char codepoint)
        (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
          (error "character U+~16,4,'0R not supported")))))

;;; --- Additional E/Lisp bridging ---

(defun e-to-lisp-function (e-function)
  "Wrap an E function (object with run/* methods) as an equivalent Lisp function."
  (lambda (&rest args) (apply #'efuncall e-function args)))

(defun e-to-lisp-mv-function (e-function)
  "Wrap an E function (object with run/* methods) as an equivalent Lisp function, treating a returned ConstList as multiple values."
  (lambda (&rest args) (values-list (coerce (e-coerce (apply #'efuncall e-function args) 'vector) 'list))))
