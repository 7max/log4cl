(in-package :log4cl.test)

(in-suite test)

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
   (error-count :initform 0))
  (:documentation "Appender that throws error when message with WARN
log level is printed"))

(defclass bad-appender-ignore-errors (bad-appender) ()
  (:documentation "BAD-APPENDER that clears the error slot in its
HANDLE-APPENDER-ERROR method"))

(defmethod appender-do-append ((appender bad-appender) logger level log-func)
  (declare (ignore logger log-func))
  (with-slots (once-only error-count)
      appender
    (when (and (= level +log-level-warn+)
               (or (not once-only)
                   (zerop error-count)))
      (incf error-count)
      (error "Appender throws an error")))
  (call-next-method))

(defmethod handle-appender-error ((appender bad-appender-ignore-errors)
                                  condition)
  (declare (ignore condition))
  (setf (slot-value appender 'error) nil))

(deftest test-appender-errors ()
  "Verify that after appender suffers an error, no more stuff is
appended to it"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((a1 (make-instance 'counting-appender))
          (a2 (make-instance 'bad-appender))
          (logger (make-logger '(one two three))))
      (add-appender logger a1)
      (add-appender logger a2)
      (log-config :i)
      (log-info logger "hey")
      (is (equal 1 (slot-value a1 'count)))
      (is (equal 1 (slot-value a2 'count)))
      (log-info logger "hey again")
      (is (equal 2 (slot-value a1 'count)))
      (is (equal 2 (slot-value a2 'count)))
      (log-warn logger "its a warning")
      (is (equal 3 (slot-value a1 'count)))
      (is (equal 2 (slot-value a2 'count)))
      (log-info logger "info once more")
      (is (equal 4 (slot-value a1 'count)))
      (is (equal 2 (slot-value a2 'count)))
      ;; clear the error
      (setf (slot-value a2 'error) nil)
      (log-info logger "info again")
      (is (equal 5 (slot-value a1 'count)))
      (is (equal 3 (slot-value a2 'count))))))

(deftest test-appender-error-log ()
  "Verify that after appender suffers an error, it's logged"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((output
            (with-output-to-string (s)
              (let ((a1 (make-instance 'fixed-stream-appender :stream s))
                    (a2 (make-instance 'bad-appender))
                    (logger (make-logger '(one two three))))
                (add-appender *root-logger* a1)
                (add-appender logger a2)
                (log-config :i)
                (log-info logger "hey")
                (is (equal 1 (slot-value a2 'count)))
                (finishes (log-warn logger "its a warning"))
                (is (equal 1 (slot-value a2 'count)))
                (log-info logger "hey again")
                (is (equal 1 (slot-value a2 'count)))))))
      (is (plusp (length output)))
      (is (search "error" (string-downcase output)))
      (values))))

(deftest test-appender-error-retry ()
  "Verify that after HANDLE-APPENDER-ERROR clears the ERROR slot, the
log operation is retried"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((output
            (with-output-to-string (s)
              (let ((a1 (make-instance 'fixed-stream-appender :stream s))
                    (a2 (make-instance 'bad-appender-ignore-errors))
                    (logger (make-logger '(one two three))))
                (add-appender *root-logger* a1)
                ;; (add-appender *root-logger* (make-instance 'console-appender))
                (add-appender logger a2)
                (log-config :i)
                (setf (logger-additivity logger) nil)
                (log-info logger "hey")
                (is (equal 1 (slot-value a2 'count)))
                (finishes (log-warn logger "its a warning"))
                (is (equal 2 (slot-value a2 'count)))
                (log-info logger "hey again")
                (is (equal 3 (slot-value a2 'count)))))))
      (is (zerop (length output))))))

(deftest test-appender-error-retry-no-forever-loop ()
  "Verify that after HANDLE-APPENDER-ERROR clears the ERROR slot, the
log operation is retried and if it errors out again, no forever loop
is entered"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((output
            (with-output-to-string (s)
              (let ((a1 (make-instance 'fixed-stream-appender :stream s))
                    (a2 (make-instance 'bad-appender-ignore-errors :once-only nil))
                    (logger (make-logger '(one two three))))
                (add-appender *root-logger* a1)
                ;; (add-appender *root-logger* (make-instance 'console-appender))
                (add-appender logger a2)
                (log-config :i)
                (setf (logger-additivity logger) nil)
                (log-info logger "hey")
                (is (equal 1 (slot-value a2 'count)))
                (log-warn logger "its a warning")
                (is (equal 1 (slot-value a2 'count)))
                (log-info logger "hey again")
                (is (equal 1 (slot-value a2 'count)))))))
      (is (plusp (length output)))
      (is (search "error" (string-downcase output))))))

(defvar unbound-var)

(defun function-with-error ()
  (1+ unbound-var))

(deftest test-appender-error-in-user-log-statement-is-rethrown ()
  "Test that when there is an condition is signaled from inside the
user log statement, its raised and does not disable the appender"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((a1 (make-instance 'bad-appender)))
      (with-slots (error-count error count)
          a1
        (add-appender *root-logger* a1)
        (log-config :d)
        (log-info "hey")
        (is (equal 0 error-count))
        (is (equal 1 count))
        ;; throws error doing append, which gets handled
        (finishes (log-warn "hey")) 
        (is (equal 1 error-count))
        (is (equal 1 count))
        (setf error nil)
        (log-info "hey")
        (is (equal 1 error-count))
        (is (equal 2 count))
        (signals error (log-info "~s" (function-with-error)))
        (is (equal 3 count))
        (is (null error))
        (is (equal 1 error-count))))))


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
      (ignore-errors
       (handler-case
           (with-open-file (s file :direction :output :if-does-not-exist :create)
             (print "test" s)
             (setq ok dir))))
      (ignore-errors (delete-file file))
      ok)))

(defparameter *temp-dir*
  (or (can-create-file-p "/tmp")
      (can-create-file-p (asdf:getenv "TEMP"))
      (can-create-file-p (asdf:getenv "TMP"))
      (can-create-file-p ".")))

(defvar *tests-dir* nil)

(defsuite* test-file-appenders)

(deftest file-tests-figure-out-test-directory ()
  (setq *tests-dir* nil)
  (setq *tests-dir*
        (ensure-directories-exist
         (merge-pathnames (format nil "~a/" (rand-filename))
                          *temp-dir*))))

(deftest test-normal-file-appender ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((fname (merge-pathnames (rand-filename) *tests-dir*))
           (a (make-instance 'file-appender :filename fname)))
      (add-appender *root-logger* a)
      (log-config :i)
      (log-info "Hello World")
      (is (appender-stream a))
      (is (probe-file fname))
      (remove-appender *root-logger* a)
      ;; verify it got closed
      (is (not (slot-boundp a 'stream)))
      (with-open-file (s fname)
        (is (equal (read-line s) "INFO - Hello World")))
      (delete-file fname))))
