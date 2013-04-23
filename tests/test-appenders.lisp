;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2012, Max Mikhanosha. All rights reserved.
;;;
;;; This file is licensed to You under the Apache License, Version 2.0
;;; (the "License"); you may not use this file except in compliance
;;; with the License.  You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package #:log4cl-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-suite test) 
  (defsuite* test-appenders))

(deftest test-appender-refcounts ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((a (make-instance 'console-appender))
          (logger (make-logger '(one two three))))
      (is (equal 0 (appender-logger-count a)))
      (add-appender *root-logger* a)
      (is (equal 1 (appender-logger-count a)))
      (add-appender *root-logger* a)
      (is (equal 1 (appender-logger-count a)))
      (add-appender logger a)
      (is (equal 2 (appender-logger-count a)))
      (remove-appender *root-logger* a)
      (remove-appender *root-logger* a)
      (is (equal 1 (appender-logger-count a)))
      (reset-logging-configuration)
      (is (equal 0 (appender-logger-count a))))))

(defclass bad-appender (counting-appender)
  ((once-only :initform t :initarg :once-only)
   (didit :initform nil))
  (:documentation "Appender that throws error when message with WARN
log level is printed"))

(defclass appender-retry-errors (bad-appender) ()
  (:documentation "BAD-APPENDER that retries on error"))

(defclass appender-ignore-errors (bad-appender) ()
  (:documentation "BAD-APPENDER that ignores errors"))

(defmethod appender-do-append ((appender bad-appender) logger level log-func)
  (declare (ignore logger log-func))
  (with-slots (once-only didit)
      appender
    (when (and (= level +log-level-warn+)
               (or (not once-only)
                   (not didit)))
      (setq didit t)
      (error "simulated appender error ~S" 'foobar)))
  (call-next-method))

(defmethod handle-appender-error ((appender appender-ignore-errors) condition)
  (declare (ignore condition))
  :ignore)
(defmethod handle-appender-error ((appender appender-retry-errors) condition)
  (declare (ignore condition))
  :retry)

(deftest test-counting-appender ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((a1 (make-instance 'counting-appender))
          (logger (make-logger '(one two three))))
      (add-appender logger a1)
      (log-config :i)
      (log-info :logger logger "hey")
      (is (equal 1 (appender-message-count a1)))
      (is (equal 1 (slot-value a1 'count)))
      (log-info :logger logger "hey2")
      (is (equal 2 (appender-message-count a1)))
      (is (equal 2 (slot-value a1 'count))))))

(deftest test-appender-errors ()
  "Verify that after appender suffers an error, no more stuff is
appended to it"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (remove-all-appenders +self-meta-logger+)
    (let ((a1 (make-instance 'counting-appender))
          (a2 (make-instance 'bad-appender))
          (logger (make-logger '(one two three))))
      (add-appender logger a1)
      (add-appender logger a2)
      (log-config :i)
      (log-info :logger logger "hey")
      (is (equal 1 (slot-value a1 'count)))
      (is (equal 1 (slot-value a2 'count)))
      (log-info :logger logger "hey again")
      (is (equal 2 (slot-value a1 'count)))
      (is (equal 2 (slot-value a2 'count)))

      (is (equal 0 (appender-error-count a1)))
      (is (equal 0 (appender-ignored-error-count a1)))
      (is (equal 0 (appender-error-count a2)))
      (is (equal 0 (appender-ignored-error-count a2)))

      ;; throws an error
      (log-warn :logger logger "its a warning")
      (is (equal 3 (slot-value a1 'count)))
      (is (equal 3 (slot-value a2 'count)))
      (is (equal 3 (appender-message-count a1)))
      (is (equal 2 (appender-message-count a2)))
      (is (typep (appender-last-error a2) 'error))

      (is (equal 0 (appender-error-count a1)))
      (is (equal 0 (appender-ignored-error-count a1)))
      (is (equal 1 (appender-error-count a2)))
      (is (equal 0 (appender-ignored-error-count a2)))

      (is (appender-enabled-p a1))
      (is (not (appender-enabled-p a2)))
      
      (log-info :logger logger "info once more")
      (is (equal 4 (slot-value a1 'count)))
      (is (equal 3 (slot-value a2 'count)))

      ;; clear the error
      (setf (appender-enabled-p a2) t)

      (log-info :logger logger "info again")
      (is (equal 5 (slot-value a1 'count)))
      (is (equal 4 (slot-value a2 'count)))

      (is (equal 5 (appender-message-count a1)))
      (is (equal 3 (appender-message-count a2))))))

(deftest test-appender-error-log ()
  "Verify that after appender suffers an error, it's logged to LOG4CL-IMPL.META logger"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((output
            (with-output-to-string (s)
              (let ((a1 (make-instance 'fixed-stream-appender :stream s))
                    (a2 (make-instance 'bad-appender))
                    (logger (make-logger '(one two three))))
                (add-appender (make-logger +self-meta-logger+) a1)
                (add-appender logger a2)
                (log-config :i)
                (log-info :logger logger "hey")
                (is (equal 1 (slot-value a2 'count)))
                (is (equal 1 (appender-message-count a2)))
                (finishes (log-warn :logger logger "its a warning"))
                ;; counting appender counts number of times
                ;; APPENDER-DO-APPEND is called, and message count
                ;; counts number of times in succeeded
                (is (equal 2 (slot-value a2 'count)))
                (is (not (appender-enabled-p a2)))
                (is (equal 1 (appender-error-count a2)))
                (is (equal 1 (appender-message-count a2)))
                ;; now since its disabled, count should not increase
                (log-info :logger logger "hey again")
                (is (equal 2 (slot-value a2 'count)))))))
      (is (plusp (length output)))
      (is (search "error" (string-downcase output)))
      (is (search "disabled" (string-downcase output)))
      (values))))

(deftest test-appender-error-retry ()
  "Verify that returning :RETRY from HANDLE-APPENDER-ERROR works"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((output
            (with-output-to-string (s)
              (let ((a1 (make-instance 'fixed-stream-appender :stream s))
                    (a2 (make-instance 'appender-retry-errors))
                    (logger (make-logger '(one two three))))
                (add-appender *root-logger* a1)
                ;; spy
                ;; (add-appender *root-logger* (make-instance 'console-appender))
                (add-appender logger a2)
                (log-config :i)
                (setf (logger-additivity logger) nil)

                (log-info :logger logger "hey")
                (is (equal 1 (slot-value a2 'count)))
                (is (equal 1 (appender-message-count a2)))
                (is (equal 0 (appender-error-count a2)))
                (is (equal 0 (appender-ignored-error-count a2)))
                
                ;; throws error, will be retried
                (finishes (log-warn :logger logger "its a warning"))

                ;; +1 for initial try, +1 for retry
                (is (equal 3 (slot-value a2 'count)))
                ;; only 1 was successful so +1
                (is (equal 2 (appender-message-count a2)))
                (is (equal 1 (appender-error-count a2)))
                (is (equal 0 (appender-ignored-error-count a2)))

                ;; back to usual
                (log-info :logger logger "hey again")
                (is (equal 4 (slot-value a2 'count)))
                (is (equal 3 (appender-message-count a2)))
                (is (equal 1 (appender-error-count a2)))
                (is (equal 0 (appender-ignored-error-count a2)))))))
      (is (zerop (length output))))))

(deftest test-appender-error-ignore ()
  "Verify that returning :RETRY from HANDLE-APPENDER-ERROR works"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((output
            (with-output-to-string (s)
              (let ((a1 (make-instance 'fixed-stream-appender :stream s))
                    (a2 (make-instance 'appender-ignore-errors))
                    (logger (make-logger '(one two three))))
                (add-appender *root-logger* a1)
                ;; spy
                ;; (add-appender *root-logger* (make-instance 'console-appender))
                (add-appender logger a2)
                (log-config :i)
                (setf (logger-additivity logger) nil)

                (log-info :logger logger "hey")
                (is (equal 1 (slot-value a2 'count)))
                (is (equal 1 (appender-message-count a2)))
                (is (equal 0 (appender-error-count a2)))
                (is (equal 0 (appender-ignored-error-count a2)))
                
                ;; throws error, will be retried
                (finishes (log-warn :logger logger "its a warning"))
                (is (appender-enabled-p a2))

                ;; +1 for initial try, +1 for retry
                (is (equal 2 (slot-value a2 'count)))
                (is (equal 1 (appender-message-count a2)))
                (is (equal 0 (appender-error-count a2)))
                (is (equal 1 (appender-ignored-error-count a2)))

                ;; back to usual
                (log-info :logger logger "hey again")
                (is (equal 3 (slot-value a2 'count)))
                (is (equal 2 (appender-message-count a2)))
                (is (equal 0 (appender-error-count a2)))
                (is (equal 1 (appender-ignored-error-count a2)))))))
      (is (zerop (length output))))))

(deftest test-appender-error-retry-no-forever-loop ()
  "Verify that after HANDLE-APPENDER-ERROR :RETRY can't lead to
forever loop."
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (remove-all-appenders +self-logger+)
    (let ((output
            (with-output-to-string (s)
              (let ((a1 (make-instance 'fixed-stream-appender :stream s))
                    (a2 (make-instance 'appender-retry-errors :once-only nil))
                    (logger (make-logger '(one two three))))
                (add-appender +self-meta-logger+ a1)
                (add-appender logger a2)
                (log-config :i)
                (setf (logger-additivity logger) nil)
                (log-info :logger logger "hey")
                (is (equal 1 (slot-value a2 'count)))
                (is (equal 1 (appender-message-count a2)))
                ;; Throw error, with :retry always returned
                (log-warn :logger logger "its a warning")
                ;; 1 was there + 3 retries 
                (is (equal 4 (slot-value a2 'count)))
                ;; none succeeded, and should have been auto disabled
                (is (equal 1 (appender-message-count a2)))
                (is (not (appender-enabled-p a2)))))))
      (is (plusp (length output)))
      (is (search "error" (string-downcase output)))
      (is (search "disabled" (string-downcase output))))
    (values)))

(defclass temp-appender-test-class (counting-appender this-console-appender) ())

(deftest test-temp-stream-appender ()
  "Verify that after TEMP-STREAM-APPENDER auto-removes itself on stream errors"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (remove-all-appenders +self-logger+)
    (let* ((logger (make-logger '(one two three)))
           a2
           (output
             (with-output-to-string (s)
               (with-output-to-string (ss) 
                 (let* ((a1 (make-instance 'fixed-stream-appender :stream s))
                        (*debug-io* ss))
                   (setq a2 (make-instance 'temp-appender-test-class))
                   (add-appender +self-meta-logger+ a1)
                   (log:config +self-meta-logger+ :info)
                   (add-appender logger a2)
                   (is (equal ss (appender-stream a2)))
                   (log-config :i)
                   (log-info :logger logger "hey")
                   (is (equal 1 (slot-value a2 'count)))
                   (is (equal 1 (appender-message-count a2)))))
               (log-info :logger logger "boo")
               (is (null (appender-loggers a2)))
               (is (equal 0 (appender-logger-count a2)))
               (is (equal 2 (slot-value a2 'count)))
               (is (equal 1 (appender-message-count a2))))))
      (is (plusp (length output)))
      (is (search "error" (string-downcase output)))
      (is (search "removed" (string-downcase output))))
    (values)))

(deftest test-self-appender-double-fault ()
  "Verify that error doing logging an error does not prevent temp appender from being removed"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (remove-all-appenders +self-logger+)
    (let* ((logger (make-logger '(one two three)))
           a2
           (output
             (with-output-to-string (s)
               (with-output-to-string (ss) 
                 (let* ((a1 (make-instance 'fixed-stream-appender :stream s))
                        (*debug-io* ss))
                   (setq a2 (make-instance 'temp-appender-test-class))
                   (add-appender +self-meta-logger+ a1)
                   (log:config +self-meta-logger+ :info)
                   (add-appender logger a2)
                   (is (equal ss (appender-stream a2)))
                   (log-config :i)
                   (log-info :logger logger "hey")
                   (is (equal 1 (slot-value a2 'count)))
                   (is (equal 1 (appender-message-count a2))))))))
      (log-info :logger logger "boo")
      (is (null (appender-loggers a2)))
      (is (equal 0 (appender-logger-count a2)))
      (is (equal 2 (slot-value a2 'count)))
      (is (equal 1 (appender-message-count a2)))
      (is (zerop (length output))))
    (values)))

(defvar unbound-var)

(defun function-with-error ()
  (1+ unbound-var))

(deftest test-appender-error-in-user-log-statement-is-rethrown ()
  "Test that when there is an condition is signaled from inside the
user log statement, its raised and does not disable the appender"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (remove-all-appenders +self-logger+)
    (let ((a1 (make-instance 'bad-appender)))
      (with-slots (last-error count)
          a1
        (add-appender *root-logger* a1)
        (log-config :d)
        (log-info "hey")
        (is (equal 0 (appender-error-count a1)))
        (is (equal 1 count))
        ;; throws error doing append, which gets handled
        (finishes (log-warn "hey")) 
        (is (equal 2 count))
        (is (equal 1 (appender-error-count a1)))
        (setf (appender-enabled-p a1) t
              (appender-last-error a1) nil)
        (log-info "hey")
        (is (equal 1 (appender-error-count a1)))
        (is (equal 3 count))
        (signals error (log-info "~s" (function-with-error)))
        (is (equal 4 count))
        (is (null (appender-last-error a1)))
        (is (equal 1 (appender-error-count a1)))))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defsuite* test-file-appenders))

(deftest test-normal-file-appender ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((fname (merge-pathnames (rand-filename) *tests-dir*))
           (a (make-instance 'file-appender :file fname))
           (logger (make-logger '(one two three))))
      (setf (logger-additivity logger) nil)
      (add-appender logger a)
      (log-config :i)
      (log-info :logger logger "Hello World")
      (is (appender-stream a))
      (is (probe-file fname))
      (remove-appender logger a)
      ;; verify it got closed
      (is (not (slot-boundp a 'stream)))
      (with-open-file (s fname)
        (is (equal (read-line s) "INFO - Hello World")))
      (delete-file fname))))

(deftest test-daily-file-appender-1 ()
  "Test the variable name-format"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((fname-base (merge-pathnames (rand-filename) *tests-dir*))
           (a (make-instance 'daily-file-appender :name-format
               (format nil "~a-%H-%M-%S.log" fname-base)
               :rollover-check-period 1))
           (logger (make-logger '(one two three))))
      (add-appender logger a)
      ;; (add-appender *root-logger* (make-instance 'console-appender))
      (log-config :d)
      (log-info :logger logger "Hey")
      (let ((fname1 (appender-filename a)))
        (log-sexp fname1)
        (sleep 1.2)
        (log-info :logger logger "Hey again")
        (let ((fname2 (appender-filename a)))
          (log-sexp fname2)
          (unwind-protect
               (progn
                 (is (not (equal fname1 fname2)))
                 (with-open-file (s fname1)
                   (is (equal (read-line s) "INFO - Hey")))
                 (remove-appender logger a)
                 ;; verify it got closed
                 (is (not (slot-boundp a 'stream)))
                 (with-open-file (s fname2)
                   (is (equal (read-line s) "INFO - Hey again"))))
            (delete-file fname1)
            (delete-file fname2)))))))

(deftest test-daily-file-appender-2 ()
  "Test the variable backup name format"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((fname (merge-pathnames (rand-filename) *tests-dir*))
           ;; fixed name, and formatted backup
           (a1 (make-instance 'daily-file-appender
                :name-format fname
                :backup-name-format (format nil "~a-%H-%M-%S.log" fname)
                :rollover-check-period 1))
           (logger (make-logger '(one two three))))
      (add-appender logger a1)
      ;; (add-appender *root-logger* (make-instance 'console-appender))
      (log-config :d)
      (log-info :logger logger "Hey")
      (let ((fname-bak (appender-next-backup-file a1)))
        (log-sexp fname-bak)
        (sleep 1.2)
        (log-info :logger logger "Hey again")
        (unwind-protect
             (progn
               (is (not (equal fname-bak fname)))
               (with-open-file (s fname-bak)
                 (is (equal (read-line s) "INFO - Hey")))
               (remove-appender logger a1)
               ;; verify it got closed
               (is (not (slot-boundp a1 'stream)))
               (with-open-file (s fname)
                 (is (equal (read-line s) "INFO - Hey again"))))
          (ignore-errors (delete-file fname-bak))
          (ignore-errors (delete-file fname)))))))

(deftest test-appender-flush-interval ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (handler-bind
        ;; CLISP apparently gives warning about reading the file
        ;; that is already open for output, automatically invoke
        ;; the restart  in this case
        ((simple-error
           (lambda (c)
             (when (and (search "open" (format nil "~a" c)
                                :test 'equalp)
                        (find-restart 'continue))
               (invoke-restart 'continue)))))
      (let* ((fname (merge-pathnames (rand-filename) *tests-dir*))
             (a (make-instance 'file-appender :file fname
                 ;; use defaults, since non-threaded lisp (ie CLISP) will have
                 ;; immediate-flush set to t by default
                 ))
             (logger (make-logger '(one two three))))
        (setf (logger-additivity logger) nil)
        (add-appender logger a)
        (log-config :i)
        (log-info :logger logger "Hello World 1")
        (is (appender-stream a))
        (is (probe-file fname))
        ;; first log statement will flush because last flush time was
        ;; initialized to zero
        (with-open-file (s fname)
          (is (equal (read-line s nil) "INFO - Hello World 1")))
        ;; this log statement should not flush
        (log-info :logger logger "Hello World 2")
        ;; verify it did not flush, (this will fail on non-threaded lisp
        ;; so only do it if its threaded lisp, and therefore immediate-flush
        ;; defaulted to NIL
        ;; Under CCL the flusher thread sometimes manages to race us 
        ;; (unless (slot-value a 'log4cl-impl::immediate-flush)
        ;;   (with-open-file (s fname)
        ;;     (is (read-line s nil))
        ;;     (is (not (read-line s nil)))))
        ;; Give auto-flusher chance to run
        (sleep 2)
        (with-open-file (s fname)
          (is (equal (read-line s nil) "INFO - Hello World 1"))
          ;; this log statement should not flush
          (is (equal (read-line s nil) "INFO - Hello World 2")))
        (remove-appender logger a)
        (delete-file fname)))))


(deftest test-daily-file-appender-3 ()
  "Test that when log file already exists, and we start writing to it, that it rolls over
based on the file modification time"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((fname (merge-pathnames (rand-filename) *tests-dir*))
           ;; fixed name, and formatted backup
           (a1 (make-instance 'daily-file-appender
                :name-format fname
                :backup-name-format (format nil "~a-%H-%M-%S.log" fname)
                ::rollover-check-period 1))
   (logger (make-logger '(one two three))))
      ;; create 
      (with-open-file (s fname :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format s "First line~%"))
      ;; test its there
      (with-open-file (s fname)
        (is (equal "First line" (read-line s))))
      ;; make sure its old enough for rollover
      (loop while (>= (file-write-date fname) (get-universal-time))
            do (sleep 0.5))
      (add-appender logger a1)
      ;; (add-appender *root-logger* (make-instance 'console-appender))
      (log-config :d)
      (log-info :logger logger "Hey")
      (let ((fname-bak (appender-last-backup-file a1)))
        (log-sexp fname-bak)
        (unwind-protect
             (progn
               (is (not (equal fname-bak fname)))
               (with-open-file (s fname-bak)
                 (is (equal "First line" (read-line s))))
               (remove-appender logger a1)
               ;; verify it got closed
               (is (not (slot-boundp a1 'stream)))
               (with-open-file (s fname)
                 (is (equal (read-line s) "INFO - Hey"))))
          (ignore-errors (delete-file fname-bak))
          (ignore-errors (delete-file fname)))))))
