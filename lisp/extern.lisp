; Copyright 2005-2007 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.extern)

; --- File access ---

(defglobal +standard-external-format+
  (labels ((f (k) (member k *features*)))
    (cond
      ((and (f :clisp) (f :unicode))
        #+clisp (ext:make-encoding :charset "UTF-8"))
      ((or (f :sb-unicode) (f :allegro))
        :utf-8)
      (t
        :default)))
  "The :external-format OPEN keyword argument to use for text file access.")

(defglobal +standard-external-format-common-name+ "utf-8"
  "The name of the +standard-external-format+ that will be be recognized by other systems.")

(defun read-entire-stream (s
    &aux (increment 1024)
         (buf (make-array increment :element-type (stream-element-type s)
                                    :adjustable t))
         (pos 0))
  (loop
    do (adjust-array buf (+ pos increment))
    until (= pos (setf pos (read-sequence buf s :start pos))))
  (adjust-array buf pos)
  (coerce buf 'simple-string))

; XXX this should be an internal symbol - external uses should be via make-file-getter
(defun read-entire-file (path)
  (with-open-file (s path :direction :input)
    (read-entire-stream s)))

(defun pathname-to-file (pathname
    &aux (components (pathname-directory (cl-fad:pathname-as-directory pathname))))
  ; XXX this is not a good approach to conversion
  (assert (eql (first components) :absolute))
  (make-file-getter (coerce (rest components) 'vector)))

(defun path-components-to-pathname (path-components)
  ;; XXX this is probably broken in various ways
  (let* ((path-components (coerce path-components 'list))
         ;; XXX the following is right for SBCL (and we can use a sbcl routine for it if my patch for bug 296 goes through --kpreid) but possibly not for other lisps
         (end (first (last path-components)))
         (name-and-type (let ((dot (position #\. end :from-end t)))
                          (cond
                            ((string= end "")
                             (list nil nil))
                            ((and dot (> dot 0))
                             (list (subseq end 0 dot)
                                   (subseq end (1+ dot))))
                            (t
                             (list end nil))))))
  (make-pathname
    :directory (cons :absolute
                     (butlast path-components))
    :name (first name-and-type)
    :type (second name-and-type))))

(defobject +file-pathname-brand+ "filePathnameBrand"
  (:stamped +deep-frozen-stamp+))

(defclass cheap-sealed-box ()
  ((value :initarg :value :accessor %cheap-sealed-box-value)))
(defclass file-pathname-box (cheap-sealed-box) ())
(defun unseal (box type)
  (%cheap-sealed-box-value (coerce box type)))
(defun file-ref-pathname (file)
  (unseal (e. file |__optSealedDispatch| +file-pathname-brand+)
          'file-pathname-box))

(defun make-file-getter (path-components)
  ;; XXX prohibit slashes? what are our consistency rules?
  (let ((pathname (path-components-to-pathname path-components)))
    (e-lambda |file| ()
      (:|__printOn| ((tw +the-text-writer-guard+))
        (e. tw |print| "<file://")
        (if (not (zerop (length path-components)))
          (loop for x across path-components do
            ;; XXX URI escaping
            (e. tw |print| "/" x))
          (e. tw |print| "/"))
        (e. tw |print| ">")
        nil)
      (:|__optSealedDispatch| (brand)
        (cond
          ((samep brand +file-pathname-brand+)
            (make-instance 'file-pathname-box :value pathname))))
      (:|getPath| ()
        ;; XXX move to URI syntax?
        (with-text-writer-to-string (tw)
          (loop for x across path-components do
              (e. tw |print| "/" x))
          (when (zerop (length path-components))
            ; must be the root
            ; xxx Java-E appends a / to the regular path iff the file exists and is a directory. should we do this?
            (e. tw |print| "/"))))
      
      (:|getName| ()
        "Return the name of the file or directory, sans path."
        (elt path-components (1- (length path-components))))
      
      (:|getPlatformPath| ()
        "Return the pathname of this file in the host OS's syntax."
        (native-namestring pathname))
      
      (:|exists| ()
        "Return whether an actual file designated by this object currently exists."
        (as-e-boolean
          (some #'probe-file
            (list pathname
                  (cl-fad:pathname-as-directory pathname)))))
      (:|get| ((subpath 'string))
        (let* ((splat (e. subpath |split| "/")))
          (make-file-getter
            (concatenate 'vector
              path-components
              (loop for component across splat
                    unless (zerop (length component))
                    collect component)))))
      (:|getOpt| (subpath)
        ;; XXX needs tests
        "Return the file at 'subpath' in this directory if it exists, otherwise null."
        (let ((sub (eelt |file| subpath)))
          (when (e-is-true (e. sub |exists|))
            sub)))
      (:|getText| ()
        (read-entire-file pathname))
      (:|getTwine| ()
        (e. (e. |file| |getText|)
            |asFrom|
            ;; XXX we should have a formal way to retrieve the URL
            (concatenate 'string "file://" (e. |file| |getPath|))))
      (:|textReader| (&aux (stream (open pathname :if-does-not-exist :error)))
        ; XXX external format, etc.
        (e-lambda "textReader" (:doc "Java-E compatibility")
          ; XXX there'll probably be other situations where we want Readers so we should have a common implementation. we could even write it in E code?
          (:|readText| ()
            "Return the entire remaining contents of the file."
            (read-entire-stream stream))))
      (:|iterate| (f)
        (loop for subpath in (cl-fad:list-directory pathname)
          do (efuncall f (file-namestring (cl-fad:pathname-as-file subpath)) (pathname-to-file subpath))))
      (:|deepReadOnly| ()
        (efuncall (e-import "org.cubik.cle.file.makeReadOnlyFile") |file| +e-false+))
      (:|shallowReadOnly| ()
        (efuncall (e-import "org.cubik.cle.file.makeReadOnlyFile") |file| +e-true+))
      (:|_clFileWriteDate| ()
        "XXX this interface needs changing. no tests. quick fix to support changes in emaker loading."
        (file-write-date pathname))
      
      (:|createNewFile| (opt-ejector)
        "Creates the file, empty, if it does not already exist. Fails if it already exists as a directory."
        (handler-case
            (progn
              (with-open-file (stream pathname
                               :direction :output
                               :element-type '(unsigned-byte 8) ; XXX OK assumption?
                               :if-exists nil
                               :if-does-not-exist :create
                               :external-format :default))
              nil)
          (file-error (condition)
            (eject-or-ethrow opt-ejector condition))))
      (:|delete| (opt-ejector)
        (handler-case
            (progn (delete-file pathname)
                   nil)
          (file-error (condition)
            (eject-or-ethrow opt-ejector condition))))
      
      (:|setText| (text) ; XXX ejector
        "..."
        (with-open-file (stream pathname
                         :direction :output
                         :element-type 'character
                         :if-exists :supersede
                         :if-does-not-exist :create ; XXX correct?
                         :external-format :default) ; XXX E-on-Java documents as UTF-8
          (write-string text stream)
          nil)))))

; --- GC ---

(defobject +gc+ "org.cubik.cle.prim.gc" ()
  ; xxx extension: reliability/0 => "NONE", "FULL", "PARTIAL", "UNKNOWN"
  ; appropriately exposed, this would allow our test suite to test weak pointers if the GC is sufficiently reliable to not cause false failures
  (:|run| ()
    "Perform as much immediate garbage collection as the underlying implementation provides."
    #+sbcl  (sb-ext:gc :full t)
    #+cmu   (extensions:gc) ; other documentation claimed SYSTEM:GC
    #+ccl   (ccl:gc)
    #+clisp (ext:gc)
    #+allegro (excl:gc t nil)
    #+abcl  (ext:gc)
    (values)))

; --- Timer ---

; xxx consider offering a pseudo-deterministic timer service, such as either:
;   * gives the time at the start of the turn, as if it had just received a message updating its time, so a log can reify this message
;   * eventual-only time request

(defconstant +java-epoch-delta+ 2208988800)

(defun cl-to-java-time (ct)
  (* 1000 (- ct +java-epoch-delta+)))

(defun java-to-cl-time (jt)
  (+ (/ jt 1000) +java-epoch-delta+))

(defobject +the-timer+ "$timer" ()
  (:|__printOn| ((tw +the-text-writer-guard+))
    (e. tw |print| "<a Timer>")
    nil)
  (:|now| ()
    (cl-to-java-time (get-fine-universal-time)))
  (:|whenPast| ((time 'real) thunk)
    (let ((utime (java-to-cl-time time)))
      (multiple-value-bind (p r) (make-promise)
        (enqueue-timed *vat* utime (lambda ()
          (e. r |resolve|
            (e. (eelt (vat-safe-scope *vat*) "trace") |runAsTurn|
              thunk
              (efun () (format nil "timer whenPast at ~A" time))))))
        p))))

; --- CL-PPCRE interface ---

(defclass ppcre-scanner-box () ((scanner :initarg :scanner :accessor ppcre-scanner)))

; XXX consider: should we, instead of emulating the Java classes, provide a natural interface to the CL-PPCRE library and write an emulation layer in E?
;   +: less/nicer native (CL) code
;   +: if other things want to use CL-PPCRE they can do so faster/more expressively (maybe)
;   -: we have to invent an E-style protocol for CL-PPCRE

; XXX instead replace this with the new +lisp+ facilities?

; compatibility with Java-E's use of org.apache.oro.text.regex.*
(defobject +rx-perl5-compiler+ "org.apache.oro.text.regex.makePerl5Compiler" ()
  (:|run| ()
    (e-lambda "org.apache.oro.text.regex.perl5Compiler" ()
      (:|compile(String)| ((s 'string))
        ;(print (list 'oro-compiling s))
        (make-instance 'ppcre-scanner-box :scanner (cl-ppcre:create-scanner s))))))

(defobject +rx-perl5-matcher+ "org.apache.oro.text.regex.makePerl5Matcher" ()
  (:|run| (&aux result-obj)
    (e-lambda "org.apache.oro.text.regex.perl5Matcher" ()
      (:|matches(PatternMatcherInput, Pattern)| ((input 'string) (pattern 'ppcre-scanner-box))
        (multiple-value-bind (match-start match-end reg-starts reg-ends)
            (cl-ppcre:scan (ppcre-scanner pattern) input)
          (setf result-obj
            ;; XXX this match range check is what the original
            ;; Java code, but may not be what we actually want for
            ;; the rx__quasiParser (as documented:
            ;; http://www.erights.org/javadoc/org/apache/oro/text/regex/Perl5Matcher.html#matches(org.apache.oro.text.regex.PatternMatcherInput,org.apache.oro.text.regex.Pattern)
            ;; ). Now that we've copied-and-modified
            ;; makePerlMatchMaker, do we "improve" this
            ;; and/or move to a more natural interface to cl-ppcre?
            (if (and match-start
                     (= match-start 0)
                     (= match-end (length input)))
              (e-lambda "$matchResult" ()
                (:|groups| ()
                  (1+ (length reg-starts)))
                (:|group| ((index 'unsigned-byte))
                  (if (zerop index)
                    (subseq input match-start match-end)
                    (let* ((cindex (1- index))
                           (rstart (aref reg-starts cindex))
                           (rend   (aref reg-ends   cindex)))
                      (if rstart
                        (subseq input rstart rend)
                        nil)))))
              nil))
          (as-e-boolean result-obj)))
      (:|getMatch| ()
        result-obj))))

;;; --- External processes ---

;; XXX remove SBCLisms

(defun make-pseudo-far (near)
  ;; XXX avoid importing every time
  (efuncall (e-import "org.cubik.cle.makePseudoFarRef") near))

(defun convert-stream-option (option which)
  (cond
    ((samep option "PIPE")
     :stream)
    ((samep option t)
     t)
    (t
     (error "unrecognized ~(~A~) option: ~A" which (e-quote option)))))

(defun convert-stream (stream which)
  (when stream
    (funcall
      (if (eql which :stdin) #'e.streams:cl-to-eio-out-stream
                             #'e.streams:cl-to-eio-in-stream)
      stream
      (format nil "process ~(~A~)" which))))

(defobject +spawn+ "org.cubik.cle.prim.spawn" ()
  (:|run| (file args)
    (efuncall +spawn+ file args +empty-const-map+))
  (:|run| (file (args 'vector) options)
    (mapping-bind options
                  ((in-o "stdin" t) ;; XXX using t for these isn't right
                   (out-o "stdout" t)
                   (err-o "stderr" t))
      (multiple-value-bind (exit-promise exit-resolver) (make-promise)
        (let* ((vat *vat*)
               (u-process (run-program (file-ref-pathname file)
                            (map 'list #'ref-shorten args) ; XXX coerce
                            :wait nil
                            :input (convert-stream-option in-o :stdin)
                            :output (convert-stream-option out-o :stdout)
                            :error (convert-stream-option err-o :stderr)
                            :status-hook
                              (lambda (u-process)
                                ;; XXX this is a signal handler in sbcl - make safe
                                (when (member (external-process-status u-process)
                                              '(:exited :signaled))
                                  (enqueue-turn vat (lambda ()
                                    (e. exit-resolver |resolve| (external-process-exit-code u-process))))))))
               (e-stdin (convert-stream (external-process-input-stream u-process) :stdin))
               (e-stdout (convert-stream (external-process-output-stream u-process) :stdout))
               (e-stderr (convert-stream (external-process-error-stream u-process) :stderr)))
          (make-pseudo-far
            (e-lambda "$process" ()
              (:|getExitValue| () exit-promise)
              (:|getOptStdin| () e-stdin)
              (:|getOptStdout| () e-stdout)
              (:|getOptStderr| () e-stderr))))))))
