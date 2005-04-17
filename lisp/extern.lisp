; Copyright 2005 Kevin Reid, under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html ................

(in-package :e.extern)

; --- File access ---

(defvar +standard-external-format+ 
  (labels ((f (k) (member k *features*)))
    (cond
      ((and (f :clisp) (f :unicode))
        #+(and clisp unicode) 'charset:utf-8)
      ((f :sb-unicode)
        :utf-8)
      (t 
        :default)))
  "The :external-format OPEN keyword argument to use for text file access.")

(defvar +standard-external-format-common-name+ "utf-8"
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

(defun make-file-getter (path-components)
  (labels ((get-cl-pathname ()
             ; XXX this is probably not a very reliable way of translating to CL pathnames
             (make-pathname :directory (cons :absolute (coerce (subseq path-components 0 (1- (length path-components))) 'list))
                            :name      (aref path-components (1- (length path-components))))))
    (e-named-lambda "FileGetter"
      (:|__printOn/1| (tw)
        (e-coercef tw +the-text-writer-guard+)
        (e. tw |print| "<file://")
        (if (/= 0 (length path-components))
          (loop for x across path-components do
            (e. tw |print| "/" x))
          (e. tw |print| "/"))
        (e. tw |print| ">")
        nil)
      (:|getPath/0| ()
        (with-text-writer-to-string (tw)
          (loop for x across path-components do
              (e. tw |print| "/" x))
          (when (= 0 (length path-components))
            ; must be the root
            ; xxx Java-E appends a / to the regular path iff the file exists and is a directory. should we do this?
            (e. tw |print| "/"))))
      (:|get/1| (subpath)
        (e-coercef subpath 'string)
        (let* ((splat (e. subpath |split| '#.(coerce #(#\/ ) 'string))))
          (make-file-getter
            (concatenate 'vector
              path-components
              (loop for component across splat
                    when (/= 0 (length component))
                    collect component)))))
      (:|getTwine/0| ()
        ; XXX doesn't actually add twine info
        (read-entire-file (get-cl-pathname)))
      (:|textReader/0| (&aux (file (open (get-cl-pathname) :if-does-not-exist :error)))
        ; XXX external format, etc.
        (e-named-lambda "textReader"
          ; XXX there'll probably be other situations where we want Readers so we should have a common implementation. we could even write it in E code?
          "Java-E compatibility"
          (:|readText/0| ()
            "Return the entire remaining contents of the file."
            (read-entire-stream file))))
      (:|iterate/1| (f)
        (loop for subpath in (cl-fad:list-directory (get-cl-pathname))
          do (e. f |run| (file-namestring (cl-fad:pathname-as-file subpath)) (pathname-to-file subpath)))))))

; --- GC ---

(defvar +gc+ (e-named-lambda "org.cubik.cle.prim.gc"
  ; xxx extension: reliability/0 => "NONE", "FULL", "PARTIAL", "UNKNOWN"
  ; appropriately exposed, this would allow our test suite to test weak pointers if the GC is sufficiently reliable to not cause false failures
  (:|run/0| ()
    "Perform as much immediate garbage collection as the underlying implementation provides."
    #+sbcl  (sb-ext:gc :full t)
    #+cmu   (extensions:gc) ; other documentation claimed SYSTEM:GC
    #+ccl   (ccl:gc)
    #+clisp (ext:gc)        ; XXX untested
    #+allegro (excl:gc t nil) ; XXX untested
    #+abcl  (ext:gc)        ; XXX untested
    )))

; --- Timer ---

; xxx consider offering a pseudo-deterministic timer service, such as either:
;   * gives the time at the start of the turn, as if it had just received a message updating its time, so a log can reify this message
;   * eventual-only time request

(defvar +the-timer+ (e-named-lambda "Timer"
  (:|__printOn/1| (tw)
    (e-coercef tw +the-text-writer-guard+)
    (e. tw |print| "<a Timer>")
    nil)
  (:|now/0| ()
    ; translate between Java's and CL's epoch
    ; make this number a constant if we ever need to do the reverse translation
    (* 1000 (- (get-universal-time) 2208988800)))))

; --- CL-PPCRE interface ---

(defclass ppcre-scanner-box () ((scanner :initarg :scanner)))

; XXX consider: should we, instead of emulating the Java classes, provide a natural interface to the CL-PPCRE library and write an emulation layer in E?
;   +: less/nicer native (CL) code
;   +: if other things want to use CL-PPCRE they can do so faster/more expressively (maybe)
;   -: we have to invent an E-style protocol for CL-PPCRE

; XXX instead replace this with the new +lisp+ facilities?

; compatibility with Java-E's use of org.apache.oro.text.regex.*
(defvar +rx-perl5-compiler+ (e-named-lambda "org.apache.oro.text.regex.makePerl5Compiler"
  (:|run/0| ()
    (e-named-lambda "org.apache.oro.text.regex.perl5Compiler"
      (:|compile(String)/1| (s)
        (e-coercef s 'string)
        ;(print (list 'oro-compiling s))
        (make-instance 'ppcre-scanner-box :scanner (cl-ppcre:create-scanner s)))))))
  
(defvar +rx-perl5-matcher+ (e-named-lambda "org.apache.oro.text.regex.makePerl5Matcher"
  (:|run/0| (&aux result-obj)
    (e-named-lambda "org.apache.oro.text.regex.perl5Matcher"
      (:|matches(PatternMatcherInput, Pattern)/2| (input pattern)
        (e-coercef input 'string)
        (e-coercef pattern 'ppcre-scanner-box)
        (multiple-value-bind (match-start match-end reg-starts reg-ends)
            (cl-ppcre:scan (slot-value pattern 'scanner) input)
          ;(print (list 'oro-results match-start match-end reg-starts reg-ends))
          (setf result-obj (if match-start
            (e-named-lambda "org.apache.oro.text.regex.perl5Matcher$matchResult"
              (:|groups/0| () 
                (1+ (length reg-starts)))
              (:|group/1| (index) 
                (e-coercef index 'unsigned-byte)
                (if (= index 0)
                  (subseq input match-start match-end)
                  (let* ((cindex (1- index))
                         (rstart (aref reg-starts cindex))
                         (rend   (aref reg-ends   cindex)))
                    (if rstart
                      (subseq input rstart rend)
                      nil)))))
            nil))
          (as-e-boolean result-obj)))
      (:|getMatch/0| ()
        result-obj)))))