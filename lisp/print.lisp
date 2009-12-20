; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.elib)

(defobject +text-writer-stamp+
    "org.erights.e.elib.print.TextWriterStamp"
    (:stamped +deep-frozen-stamp+)
  (:|audit| (audition)
    (declare (ignore audition))
    +e-true+))

(defobject +the-text-writer-guard+
    "org.erights.e.elib.print.TextWriterGuard"
    (:stamped +deep-frozen-stamp+)
  (:|coerce/2| (standard-coerce
    (lambda (specimen) (approvedp +text-writer-stamp+ specimen))
    (lambda () +the-text-writer-guard+)
    :error (lambda (specimen) (format nil "~A is not audited as a TextWriter" specimen))
    :test-shortened nil)))

(defglobal +standard-syntax+
  (e-lambda "org.erights.e.elib.print.baseSyntax"
      (:stamped +deep-frozen-stamp+)
    (:|run| (writer)
      (labels ((spawn (is-quoting line-separator)
                (e-lambda |instance| ()
                  (:|enterReference| ()
                    |instance|)
                  (:|exitReference| ()
                    nil)
                  (:|cycle| ()
                    (e. writer |write| "<***CYCLE***>"))
                  (:|eventual| (is-resolved)
                    (if (e-is-true is-resolved)
                      (e. writer |write| "<Far ref>")
                      (e. writer |write| "<Promise>")))
                  (:|broken| (tw problem)
                    (e. writer |write| "<ref broken by ")
                    (e. tw |print| problem)
                    (e. writer |write| ">"))
                  (:|problem| (tw (fqn 'string) problem)
                    (e. tw |print| "<***"
                                    (aan fqn)
                                    " threw "
                                    problem
                                    " when printed"
                                    "***>"))
                  (:|write| ((text 'string))
                    (replace-newlines text line-separator (lambda (x) (e. writer |write| x))))
                  (:|indent| ((indent-arg 'string))
                    (spawn is-quoting (concatenate 'string line-separator indent-arg)))
                  (:|asQuoting| ()
                    (spawn t line-separator))
                  (:|asNotQuoting| ()
                    (spawn nil line-separator))
                  (:|isQuoting| ()
                    (as-e-boolean is-quoting)))))
        (spawn nil #.(string #\Newline))))))

(defun trace-error-print-handler (thing)
  ; XXX printing the backtrace is too noisy for running the tests - we need a 'store-but-don't-print' trace facility, or a tracelog like E-on-Java.
  (lambda (raw-condition)
    (efuncall
      (eelt (vat-safe-scope *vat*) "traceln")
      (format nil "problem while printing ~S: ~A (~S)~%~:W"
        thing
        (e-quote raw-condition)
        raw-condition
        (or #+(or) (backtrace-value))))))

(defun do-print-syntax (tw thing syntax in-error-printing nest)
  (ecase (ref-state thing)
    (eventual (e. syntax |eventual| (as-e-boolean (ref-is-resolved thing))))
    (broken   (e. syntax |broken| tw (ref-opt-problem thing))) ; XXX wart: tw argument
    (near     (if in-error-printing
                ; If we're in error printing, and the printing
                ; fails, just give up and let the exception
                ; propagate.
                (e. thing |__printOn| tw)
                (handler-case
                  (handler-bind
                      ((e-catchable-condition (trace-error-print-handler thing)))
                    (e. thing |__printOn| tw))
                  (e-catchable-condition (condition)
                    (e. syntax |problem| (funcall nest :in-error-printing t)
                                         (cl-type-fq-name (observable-type-of thing))
                                         ; XXX passing the problem to the syntax unrestrictedly will be an arguable authority leak once we generalize TextWriter to non-string output. Probably should restrict to Data, if we can do that at this layer.
                                         (transform-condition-for-e-catch condition))))))))

(defun make-text-writer (&key syntax delegate is-quoting autoflush (indent-step "    ")
    &aux (seen (e.elib.tables::make-primitive-flex-map)))
  (unless (e-is-true (efuncall +the-audit-checker+ +deep-frozen-stamp+ syntax))
    (e-coercef syntax (eelt (vat-safe-scope *vat*) "DeepFrozen")))
  (labels ((spawn (&key syntax should-close autoflush in-error-printing open-flags
              &aux (delegate delegate))
            "Makes an individual TextWriter."
            (push (make-instance 'e-var-slot :value t) open-flags)
            (with-result-promise (tw)
              (labels ((nest (&key (syntax syntax)
                                   (should-close should-close)
                                   (autoflush autoflush)
                                   (in-error-printing in-error-printing))
                        ; Ook.
                        (spawn :syntax syntax
                               :should-close should-close
                               :autoflush autoflush
                               :in-error-printing in-error-printing
                               :open-flags open-flags))
                       (assert-open ()
                         (assert (every #'(lambda (s) (e. s |get|)) open-flags) () "closed TextWriter")))
                (e-lambda "org.erights.e.elib.print.TextWriter"
                    (:stamped +text-writer-stamp+
                     :stamped +pass-by-construction+)
                  (:|__optUncall| ()
                    `#(,+the-make-text-writer+ "makePresence"
                       #(,(e-lambda "forwarder" ()
                            (otherwise (mverb &rest args)
                              (apply #'e-call-dispatch tw mverb args))))))
                  (:|__printOn| ((ptw +the-text-writer-guard+))
                    (e. ptw |print| "<textWriter>"))
                  (:|close| ()
                    "Prevent this TextWriter from printing anything more; close the underlying stream if appropriate."
                    (assert-open)
                    (when should-close
                      (e. delegate |close|))
                    (e. (first open-flags) |put| nil)
                    (setf delegate nil)
                    nil)
                  (:|flush| ()
                    (assert-open)
                    (when delegate
                      (e. delegate |flush|))
                    nil)
                  (:|write| (text)
                    (assert-open)
                    (e. syntax |write| text)
                    (when autoflush
                      (e. delegate |flush|))
                    nil)
                  (:|printSame| (thing)
                    ; XXX lousy name
                    (setf thing (ref-shorten thing))
                    (assert-open)
                    (let ((key (make-traversal-key thing)))
                      (let* ((sub-syntax (e. syntax |enterReference|))
                             (sub-tw (nest :should-close nil
                                           :syntax sub-syntax)))
                        (if (block nil (e. seen |fetch| key (efun () (return nil)))
                                                  t)
                          (e. sub-syntax |cycle|)
                          (progn
                            (e. seen |put| key nil +e-false+ nil)
                            (unwind-protect
                              (do-print-syntax sub-tw thing syntax in-error-printing #'nest)
                              (e. seen |removeKey| key +e-false+)
                              (e. sub-tw |close|))))
                        (e. syntax |exitReference|)))
                    nil)
                  (:|isQuoting| () (e. syntax |isQuoting|))
                  (:|printAll| ((vector 'vector))
                    (loop for x across vector do (e. tw |print| x))
                    nil)
                  (:|println| (obj)
                    (e. tw |print| obj)
                    (e. tw |write| #.(string #\Newline))
                    nil)
                  (:|println| ()
                    (e. tw |write| #.(string #\Newline))
                    nil)
                  (:|lnPrint| (obj)
                    (e. tw |write| #.(string #\Newline))
                    (e. tw |print| obj)
                    nil)
                  (:|indent| ()
                    (e. tw |indent| indent-step))
                  (:|indent| ((step 'string))
                    (nest :syntax (e. syntax |indent| step)
                          :should-close nil))
                  (:|withAutoflush| ()
                    (nest :autoflush t))
                  (otherwise (mverb &rest args
                      &aux (mv-string (symbol-name mverb))
                           (slash     (position #\/ mv-string))
                           (arity     (parse-integer mv-string :start (1+ slash))))
                    (assert (eql arity (length args))
                            ()
                            "Mismatch: Arity ~A, arglist ~W" arity args)
                    ;; XXX if we ever write another cond like this,
                    ;; extract it into a mverb-verb-case macro and put
                    ;; it in util.lisp.
                    (cond
                      ((string= mv-string "print" :end1 slash)
                        (let ((tw (nest :syntax (e. syntax |asNotQuoting|))))
                          (dolist (arg args) (e. tw |printSame| arg))))
                      ((string= mv-string "quote" :end1 slash)
                        (let ((tw (nest :syntax (e. syntax |asQuoting|))))
                          (dolist (arg args) (e. tw |printSame| arg))))
                      (t
                        (no-such-method tw mverb args)))))))))
    (spawn :syntax (let ((base (efuncall syntax delegate)))
                     (if is-quoting
                       (e. base |asQuoting|)
                       base))
           :should-close t
           :autoflush autoflush)))

(defun replace-newlines (text indent-string write-func)
  (with-input-from-string (input text)
    (loop
      for (line missing-newline-p) = (multiple-value-list
                                       (read-line input nil nil))
      while line
      do (funcall write-func line)
         (unless missing-newline-p
           (funcall write-func indent-string)))))


(defun make-text-writer-to-cl-stream (stream &key autoflush should-close-underlying quote)
  (make-text-writer
    :syntax +standard-syntax+
    :is-quoting quote
    :autoflush autoflush
    :delegate (e-lambda "org.cubik.cle.internal.StreamTWDelegate" ()
      (:|write| (text)
        (setf text (ref-shorten text))
        (check-type text string)
        (princ text stream)
        nil)
      (:|flush| ()
        (force-output stream)
        nil)
      (:|close| ()
        (when should-close-underlying
          (close stream))
        nil))))


(defclass string-buffer (vat-checking)
  ((buffer :initarg :buffer
           :type string
           :reader string-buffer-buffer)))

(def-vtable string-buffer
  (:|__printOn| (this (tw +the-text-writer-guard+))
    ; XXX should we not print the brackets if not isQuoting?
    (e. tw |print| "<stringBuffer ")
    (e. tw |quote| (copy-seq (string-buffer-buffer this)))
    (e. tw |print| ">"))
  (:|snapshot| (this)
    ; XXX make non-adjustable exact-sized string
    (copy-seq (string-buffer-buffer this))))


(defobject +the-make-text-writer+ "org.erights.e.elib.oldeio.makeTextWriter"
    (:stamped +deep-frozen-stamp+
     :stamped +standard-graph-exit-stamp+)
  (:|makeBufferingPair| () (e. +the-make-text-writer+ |makeBufferingPair| +empty-const-map+))
  (:|makeBufferingPair| ((options 'e.elib.tables:const-map))
    "Return a tuple of a TextWriter and a StringBuffer from which the output of the TextWriter is readable."
    ; xxx arbitrary initial size figure. CLISP didn't like an initial size of 0 (adjust-array signaled the error "index too large")
    (let ((buffer (make-array 80
                              :element-type 'character
                              :adjustable t
                              :fill-pointer 0)))
      (vector (make-text-writer
                :syntax (e. options |fetch| "syntax" (efun () +standard-syntax+))
                :delegate (e-lambda "org.cubik.cle.internal.StringTWDelegate" ()
                  (:|write| (piece)
                    (setf piece (ref-shorten piece))
                    (check-type piece string)
                    (let* ((old-size (length buffer))
                           (new-size (+ (length piece) old-size)))
                      ; XXX code copied from FlexList#replace/5. Is this a sign?
                      (when (< (array-dimension buffer 0) new-size)
                        (adjust-array buffer (* new-size 2)))
                      (setf (fill-pointer buffer) new-size)
                      (replace buffer piece :start1 old-size)))
                  (:|flush| () nil)
                  (:|close| () nil)))
              (make-instance 'string-buffer :buffer buffer))))
  (:|makePresence| (remote)
    (make-text-writer
      ;; XXX do something about custom syntaxes
      :syntax +standard-syntax+
      :delegate (e-lambda "org.cubik.cle.internal.RemoteTWDelegate" ()
        (:|write| (piece)
          (e<- remote |write| piece))
        (:|flush| () (e<- remote |flush|))
        (:|close| () (e<- remote |close|)))))
  (:|run| (underlying autoflush)
    "For Java-E compatibility. Returns the original stream, or its withAutoflush/0. E-on-CL provides TextWriters where Java-E provides Java streams, and in Java-E this would wrap a Java stream in a TextWriter."
    (if (e-is-true autoflush)
      (e. underlying |withAutoflush|)
      underlying)))
