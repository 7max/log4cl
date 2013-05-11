

(eval-when (:compile-toplevel :execute)
  (let ((p (find-package :log4cl-test)))
    (when p
      (dolist (p2 (package-used-by-list p))
        (unuse-package p p2))
      (delete-package p)
      (in-package :cl-user))))

(defpackage :log4cl-test
  (:use :cl :log4cl-impl :stefil)
  (:export
   :test :test-speed
   :test-pattern-layout
   :make-expected
   :defsubsuite
   :subsuite-start
   :rand-filename
   :*tests-dir*
   :*temp-dir*))

(in-package :log4cl-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn 
    #+sbcl (declaim (sb-ext:muffle-conditions stefil::test-style-warning)) 
    (in-root-suite) 
    (defsuite* test)))

(defmacro defsubsuite (name)
  `(progn
     (defpackage ,name
       (:use :cl :log4cl :log4cl-test :stefil)
       (:shadow #:test)
       (:export #:test))
     (in-package ,name)))

(defmacro subsuite-start ()
  (let ((name (find-symbol (string '#:test) *package*))) 
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       #+sbcl (declaim (sb-ext:muffle-conditions stefil::test-style-warning))
       (progn 
         (in-root-suite)
         (in-suite log4cl-test:test)
         (defsuite* ,name)))))

(defparameter *file-tests-random-state* (make-random-state t))

(defun rand-filename (&optional (num-chars 5))
  (with-output-to-string (s)
    (dotimes (cnt num-chars)
      (princ (code-char
              (+ (char-code #\a)
                 (random 26 *file-tests-random-state*)))
             s))))

(defun can-create-file-p (dir)
  (when (stringp dir)
    (unless (member (elt dir (1- (length dir)))
                    '(#\\ #\/))
      (setq dir (concatenate 'string dir "/")))
    (setq dir (parse-namestring dir))
    (let ((file (merge-pathnames (rand-filename) dir))
          ok)
      (handler-case
          (with-open-file (s file :direction :output :if-does-not-exist :create)
            (print "test" s)
            (setq ok dir)))
      (ignore-errors (delete-file file))
      ok)))

(defparameter *temp-dir*
  (or (can-create-file-p "/tmp")
      (can-create-file-p (asdf::getenv "TEMP"))
      (can-create-file-p (asdf::getenv "TMP"))
      (can-create-file-p ".")))

(defparameter *tests-dir*
  (ensure-directories-exist
   (merge-pathnames (format nil "~a/" (rand-filename))
                    *temp-dir*)))

(deftest test-pattern-layout (pattern expected-result
                                      &key
                                      (level +log-level-info+)
                                      (logger (make-logger '(one two three)))
                                      (message "message"))
  "Output a log message into an appender with a pattern layout with
specified PATTERN and compare its output to EXPECTED-RESULT"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((output
            (with-output-to-string (s)
              (add-appender *root-logger*
                            (make-instance 'fixed-stream-appender
                             :stream s
                             :layout (make-instance 'pattern-layout
                                      :conversion-pattern pattern)))
              (setf (logger-log-level *root-logger*) level)
              ;; TODO need a macro that logs with specified level
              (cond 
                ((eql level +log-level-fatal+) (log-fatal :logger logger "~a" message))
                ((eql level +log-level-error+) (log-error :logger logger "~a" message))
                ((eql level +log-level-warn+) (log-warn :logger logger "~a" message))
                ((eql level +log-level-info+) (log-info :logger logger "~a" message))
                ((eql level +log-level-debug+) (log-debug :logger logger "~a" message))
                ((eql level +log-level-debu1+) (log-debu1 :logger logger "~a" message))
                ((eql level +log-level-debu2+) (log-debu2 :logger logger "~a" message))
                ((eql level +log-level-debu3+) (log-debu3 :logger logger "~a" message))
                ((eql level +log-level-debu4+) (log-debu4 :logger logger "~a" message))
                ((eql level +log-level-trace+) (log-trace :logger logger "~a" message))
                ((eql level +log-level-debu5+) (log-debu5 :logger logger "~a" message))
                ((eql level +log-level-debu6+) (log-debu6 :logger logger "~a" message))
                ((eql level +log-level-debu7+) (log-debu7 :logger logger "~a" message))
                ((eql level +log-level-debu8+) (log-debu8 :logger logger "~a" message))
                ((eql level +log-level-debu9+) (log-debu9 :logger logger "~a" message))))))
      (is (equal output expected-result))))
  (values))
