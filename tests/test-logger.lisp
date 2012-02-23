(cl:defpackage :log4cl-test
  (:use :cl :log4cl-impl :stefil)
  (:export :test :speed
           :handle-appender-error)
  (:shadow :speed))

(in-package #:log4cl-test)

(in-root-suite)
(defsuite* test)

(deftest basics (logger)
  "Test some basic facts about the logger structure"
  (with-package-log-hierarchy
    (is (not (null logger)))
    (is (not (null (log4cl-impl::logger-state logger))))
    (is (not (null (logger-category logger))))
    (is (eql (length (log4cl-impl::logger-state logger)) log4cl-impl::*hierarchy-max*))))

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
    (is (not (log-warn)))
    (is (null (logger-appenders *root-logger*)))
    ;; reset provides sane defaults
    (reset-logging-configuration)
    (is (log-warn))
    (is (not (log-debug)))
    (is (not (null (logger-appenders *root-logger*))))
    ;; do reset and clear in the different hierarchy
    (with-package-log-hierarchy
        (reset-logging-configuration)
      (is (log-warn))
      (is (not (log-debug)))
      (clear-logging-configuration)
      (is (not (log-warn)))
      (is (null (logger-appenders *root-logger*))))
    ;; see that original one is unchanged
    (is (log-warn))
    (is (not (log-debug)))
    (is (not (null (logger-appenders *root-logger*))))))

(deftest produces-output ()
  "Test that default logging configuration produces correct output"
  (with-package-log-hierarchy
    (reset-logging-configuration)
    (is (equal (with-output-to-string (*debug-io*)
                 (log-warn "Hello World!"))
               "WARN - Hello World!
"))))

(deftest produces-output-with-explicit-logger ()
  "Test that log statement with explicit logger produce output"
  (with-package-log-hierarchy
    (reset-logging-configuration)
    (is (equal (with-output-to-string (*debug-io*)
                 (log-warn (make-logger)  "Hello World!"))
               "WARN - Hello World!
"))
    (is (equal (with-output-to-string (*debug-io*)
                 (log-warn '(log4cl test foobar)  "Hello World!"))
               "WARN - Hello World!
"))
    (is (equal (with-output-to-string (*debug-io*)
                 (log-warn :foobar  "Hello World!"))
               "WARN - Hello World!
"))
    (is (equal (with-output-to-string (*debug-io*)
                 (log-warn 'foobar  "Hello World!"))
               "WARN - Hello World!
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
      (is (log-warn logger)))))

(deftest logger-by-expression ()
  "Test logging macros to verify that we can make a function returning
a logger, and that logging macros are correctly handling this
situation"
  (with-package-log-hierarchy
    (reset-logging-configuration)
    (log-info "Here1")))

(deftest test-counting-appender ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((a (make-instance 'counting-appender)))
      (add-appender *root-logger* a)
      (log-config :i)
      (log-info "hey")
      (is (equal 1 (slot-value a 'count)))
      (log-debug "moo")
      (is (equal 1 (slot-value a 'count)))
      (log-info "hey again")
      (is (equal 2 (slot-value a 'count))))))

