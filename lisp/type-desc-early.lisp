; This Common Lisp code is Copyright 2005-2007 Kevin Reid, under the terms of the
; MIT X license, and partially derived from Java code which is
; Copyright 2002 Combex, Inc. under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

; XXX tests for this
(defun simplify-fq-name (fq-name)
  " Drops any package or containing class prefixes and any \"__C\" or \"__T\"
    suffixes prior to the last \"significant\" name.
    
    A \"significant\" name is any name that doesn't begin with a digit
    (ruling out anonymous objects and classes) and that isn't \"self\"."
  (let* ((last-sep (or (position-if (lambda (x) (member x '(#\. #\$))) fq-name :from-end t)
                       -1))
         (result (subseq fq-name (1+ last-sep))))
    (let ((suffix-start (- (length result) 3)))
      (when (and (>= suffix-start 0)
                 (or (string= "__C" result :start2 suffix-start)
                     (string= "__T" result :start2 suffix-start)))
        (setf result (subseq result 0 suffix-start))))
    (if (and (> (length result) 0)
             (or (string= "self" result)
                 (digit-char-p (aref result 0))))
      ; result so far is not "significant"
      (concatenate 'string "..."
                           (simplify-fq-name (subseq fq-name 0 last-sep))
                           (list (aref fq-name last-sep))
                           result)
      (if (string= result "") ; sanity check
        fq-name
        result))))

(defun join-fq-name (prefix qname)
  "Return 'qname' if it has the format of a fully-qualified name, or 'qname' joined with 'prefix' otherwise."
  (cond
    ((and (string/= qname "") (string= qname "$" :end1 1))
      (concatenate 'string prefix (subseq qname 1)))
    (t
      ; XXX the error case comes from Java-E structure (separated isFQName). we should find out why Java-E bothers separating it, and decide whether to use the same structure here
      qname
      #-(and) (error "Unrecognized qualified name: ~A (being qualified with prefix ~A)" (e-quote qname) (e-quote prefix)))))

(deftype doc-comment ()
  '(or null string))

;;; --- Early CL-type stuff ---

(defgeneric observable-type-of (specimen)
  ;; This should arguably be observable-class-of.
  (:documentation "Given an object, return the narrowest type specifier which should visible (in its FQN form) to the E programmer. Usually specialized via the def-class-opaque macro."))

(defmacro def-class-opaque (class-name &optional (visible-type class-name))
  `(defmethod observable-type-of ((a ,class-name))
    (declare (ignore a))
    ',visible-type))

;;; --- Type description objects ---

;;; These are defined early so that the classes are available for use in the various __getAllegedType implementations, some of which construct the type description at macroexpansion time.

(defclass type-desc () 
  ((doc-comment :initarg :doc-comment
                :reader type-desc-doc-comment
                :type doc-comment)
   (opt-fq-name :initarg :fq-name
                :reader type-desc-opt-fq-name
                :type (or null string))
   (supers :initarg :supers
           :reader type-desc-supers
           :type (vector t))
   (auditors :initarg :auditors
             :reader type-desc-auditors
             :type (vector t))
   (message-types-v :initarg :message-types-v
                    :reader type-desc-message-types-v
                    :type (vector message-desc))))

(defgeneric type-desc-message-types (type-desc))

(defclass message-desc () 
  ((verb :initarg :verb 
         :reader message-desc-verb
         :type string)
   (doc-comment :initarg :doc-comment :initform nil
                :reader message-desc-doc-comment
                :type doc-comment)
   (opt-result-guard :initarg :opt-result-guard :initform nil
                     :reader message-desc-opt-result-guard
                     :type t)
   (params :initarg :params
           :reader message-desc-params
           :type (vector param-desc)))
  (:documentation "A description of a message signature within a type description."))

(defclass param-desc () 
  ((opt-name :initarg :opt-name :initform nil
             :reader param-desc-opt-name
             :type (or null string))
   (opt-guard :initarg :opt-guard :initform nil
              :reader param-desc-opt-guard
              :type t)))

(defclass eval-param-desc (param-desc)
  ((form :initarg :form
         :reader eval-param-desc-form))
  (:documentation "Gimmick so that a CL object's alleged type's parameter guards remain late-bound and don't need to be put into compiled files."))

(defmethod param-desc-opt-guard ((this eval-param-desc))
  (let ((guard-specifier (eval (eval-param-desc-form this))))
    (typecase guard-specifier
       ((or symbol cons)
        (type-specifier-to-guard guard-specifier))
       (otherwise
        guard-specifier))))

; The vtables for {type,message,param}-desc may be found in elib-values.lisp.

; This is necessary because E-LAMBDA and elang objects have 'literal' 
; TypeDescs in their __getAllegedType/0 methods.
; 
; The definition was copied from
; http://www.lispworks.com/reference/HyperSpec/Body/f_mk_ld_.htm
;
(defmethod make-load-form ((a type-desc) &optional environment)
  (make-load-form-saving-slots a :environment environment))
(defmethod make-load-form ((a message-desc) &optional environment)
  (make-load-form-saving-slots a :environment environment))
(defmethod make-load-form ((a param-desc) &optional environment)
  (make-load-form-saving-slots a :environment environment))

;; We aren't equipped to make eval-param-descs have a different E 
;; selfless-identity, so we make them look just like ordinary param-descs
;; instead.
(def-class-opaque param-desc)

