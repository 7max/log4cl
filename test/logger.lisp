(cl:defpackage :log4cl.test
  (:use :cl :log4cl :stefil)
  (:export :test :speed)
  (:shadow :speed))

(cl:defpackage :log4cl.test.dots
  (:use :cl :log4cl :stefil))

(in-package :log4cl.test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmethod naming-option ((pkg (eql (find-package :log4cl.test.dots)))
                            (option (eql :category-separator)))
    "."))

(in-root-suite)
(defsuite* test)

(deftest basics (logger)
  "Test some basic facts about the logger structure"
  (with-package-log-hierarchy
    (is (not (null logger)))
    (is (not (null (log4cl::logger-state logger))))
    (is (not (null (logger-category logger))))
    (is (eql (length (log4cl::logger-state logger)) log4cl::*hierarchy-max*))))

(deftest make-logger-by-list-of-categories ()
  "Test MAKE-LOGGER macro with static list of categories"
  (with-package-log-hierarchy
    (let ((logger (make-logger '(one two three four))))
      (basics logger)
      (is (equal (logger-category logger)
                 (concatenate 'string
                              (symbol-name 'one) ":"
                              (symbol-name 'two) ":"
                              (symbol-name 'three) ":"
                              (symbol-name 'four))))
      (is (equal (logger-name logger) (symbol-name 'four)))
      (is (eql (logger-depth logger) 4)))))


(deftest single-name ()
  "Test the logger name being correct when no separators are found in
  the name"
  (let ((logger (make-logger '(foobar))))
    (is (equal (logger-category logger) (symbol-name 'foobar)))
    (is (equal (logger-name logger) (symbol-name 'foobar)))))

(deftest reset-configuration-0 ()
  "Test that CLEAR-LOGGING-CONFIGURATION works and that
RESET-LOGGING-CONFIGURATION reset the logging system to a sane
state. Also tests that different hierarchies do not affect each other
configuration"
  ;; verify clear/reset only does so for current configuration (current-indentation)
  (with-log-hierarchy ('dummy)
    ;; clear deletes everything
    (clear-logging-configuration)
    (is (not (log-info)))
    (is (null (logger-appenders *root-logger*)))
    ;; reset provides sane defaults
    (reset-logging-configuration)
    (is (log-info))
    (is (not (log-debug)))
    (is (not (null (logger-appenders *root-logger*))))
    ;; do reset and clear in the different hierarchy
    (with-package-log-hierarchy
        (reset-logging-configuration)
      (is (log-info))
      (is (not (log-debug)))
      (clear-logging-configuration)
      (is (not (log-info)))
      (is (null (logger-appenders *root-logger*))))
    ;; see that original one is unchanged
    (is (log-info))
    (is (not (log-debug)))
    (is (not (null (logger-appenders *root-logger*))))))

(deftest produces-output ()
  "Test that default logging configuration produces correct output"
  (with-package-log-hierarchy
    (reset-logging-configuration)
    (is (equal (with-output-to-string (*debug-io*)
                 (log-info "Hello World!"))
               "INFO - Hello World!
"))))

(deftest verify-returns-same-logger ()
  "Test that MAKE-LOGGER returns singleton logger object every time"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((logger (make-logger '(one two three))))
      (is (eq logger (make-logger '(one two three))))
      (is (eq logger (make-logger logger)))
      (is (not (eq logger *root-logger*)))
      (clear-logging-configuration)
      (is (eq logger (make-logger '(one two three)))))))

(deftest logger-by-variable ()
  "Test logging macros to verify that we can bind logger into a
variable, and that logging macros are correctly handling this
situation"
  (with-package-log-hierarchy
    (reset-logging-configuration)
    (let ((logger (make-logger :foobar)))
      (is (log-info logger)))))

(deftest logger-by-expression ()
  "Test logging macros to verify that we can make a function returning
a logger, and that logging macros are correctly handling this
situation"
  (with-package-log-hierarchy
    (reset-logging-configuration)
    (flet ((get-my-logger ()
             (make-logger :foobar)))
      (let ((logger (get-my-logger)))
        (is (eq logger (get-my-logger)))
        (is (log-info (get-my-logger)))
        (is (not (log-debug (get-my-logger))))
        (setf (logger-log-level logger) :debug)
        (is (log-debug (get-my-logger)))))))

;;
;; Test in a different package, where logger category separator is dot
;; instead of new line

(in-package :log4cl.test.dots)
(in-root-suite)
(defsuite* test)

(deftest make-logger-by-list-of-categories ()
  "Test MAKE-LOGGER macro with static list of categories"
  (with-package-log-hierarchy
    (let ((logger (make-logger '(one two three four))))
      (log4cl.test::basics logger)
      (is (equal (logger-category logger)
                 (concatenate 'string
                              (symbol-name 'one) "."
                              (symbol-name 'two) "."
                              (symbol-name 'three) "."
                              (symbol-name 'four))))
      (is (equal (logger-name logger) (symbol-name 'four)))
      (is (eql (logger-depth logger) 4)))))

(deftest logger-name-via-dotted-keyword ()
  "Test that specifying logger name by a keyword containing dots is
correctly parsed into multiple loggers"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (is (null (logger-appenders *root-logger*)))
    (is (null (logger-log-level *root-logger*)))
    (is (eql +log-level-off+ (effective-log-level *root-logger*)))
    (let* ((logger (make-logger :one.two.three))
           (logger-one (make-logger :one))
           (logger-two (make-logger :one.two)))
      (is (eq (logger-parent logger) logger-two))
      (is (eq (logger-parent logger-two) logger-one))
      (is (null (logger-log-level logger)))
      (is (eql +log-level-off+ (effective-log-level logger)))
      ;; Add appender to the parent
      (add-appender logger-one (make-instance 'console-appender))
      ;; see that it got inherited
      (is (null (logger-appenders logger)))
      (is (not (null (effective-appenders logger)))))))

(deftest appender-additivity ()
  "Test appender additivity works"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((one (make-logger :one))
           (one-two (make-logger :one.two))
           (one-two-three (make-logger :one.two.three)))
      ;; Add appender to the root, and one-two
      (add-appender *root-logger* (make-instance 'console-appender))
      (add-appender one (make-instance 'console-appender))
      ;; see that it got inherited
      (is (null (logger-appenders one-two-three)))
      (is (not (null (effective-appenders one-two-three))))
      (is (equal 2 (length (effective-appenders one-two-three))))
      (is (equal 2 (length (effective-appenders one-two))))
      (is (equal 2 (length (effective-appenders one))))
      ;; now make  :one.two non-additive
      (setf (logger-additivity one-two) nil)
      (is (equal 0 (length (effective-appenders one-two-three))))
      (is (equal 0 (length (effective-appenders one-two))))
      (is (equal 2 (length (effective-appenders one))))
      ;; add logger to one-two
      (add-appender one-two (make-instance 'console-appender))
      (is (equal 1 (length (effective-appenders one-two-three))))
      (is (equal 1 (length (effective-appenders one-two))))
      (add-appender one-two (make-instance 'console-appender))
      (is (equal 2 (length (effective-appenders one-two-three))))
      (setf (logger-additivity one-two) t)
      (is (equal 4 (length (effective-appenders one-two-three)))))))

(deftest inherit-log-levels ()
  "Test log level inheritance"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((logger (make-logger :one.two.three))
          (parent (make-logger :one)))
      ;; verify no logging
      (is (eql +log-level-off+ (effective-log-level logger)))
      (is (null (log-info)))
      (is (null (log-info logger)))
      ;; now set root log level to info, and verify that
      ;; the levels are right
      (setf (logger-log-level parent) :info)
      (is (null (logger-log-level logger)))
      (is (eql +log-level-info+ (effective-log-level logger)))
      ;; debugging is still off because of no appenders
      (is (null (log-debug logger)))
      (is (null (log-info logger)))
      ;; add appender, verify debugging is now on
      (add-appender parent (make-instance 'console-appender))
      (is (log-info logger))
      (is (null (log-debug logger)))
      ;; turn debug on on :one.two.three logger
      (setf (logger-log-level logger) :debug)
      ;; verify the level
      (is (eql +log-level-debug+ (logger-log-level logger)))
      (is (eql +log-level-debug+ (effective-log-level logger)))
      ;; verify both debug and info are on for logger
      (is (log-debug logger))
      (is (log-info logger))
      ;; verify only info is on for root logger
      (is (null (log-debug :one)))
      (is (log-info :one))
      ;; and same for logger parent
      (is (null (log-debug :one.two)))
      (is (log-info :one.two))
      ;; set root logger off, verify that explicit setting on logger
      ;; is still in effect
      (setf (logger-log-level parent) :off)
      (is (null (log-debug)))
      (is (null (log-info)))
      (is (log-debug logger))
      (is (log-info logger)))))

(deftest make-logger-with-dotted-symbol-name ()
  (with-package-log-hierarchy
    (let ((logger (make-logger :one.two.three)))
      (log4cl.test::basics logger)
      (is (equal (logger-category logger)
                 (concatenate 'string
                              (package-name #.*package*)
                              "."
                              (symbol-name :one.two.three)))))))


;; Include the "dots" package test suite into main one
(in-package :log4cl.test)
(in-suite test)

(deftest dots ()
  (log4cl.test.dots::test))
