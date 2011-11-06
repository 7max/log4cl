(cl:defpackage :log4cl.test
  (:use :cl :log4cl :stefil)
  (:export :test :speed)
  (:shadow :speed))

(in-package :log4cl.test)

(in-root-suite)
(defsuite* test)

(deftest basics (&optional (logger *root-logger*))
  (with-package-log-hierarchy
    (is (not (null logger)))
    (is (not (null (log4cl::logger-state logger))))
    (is (not (null (logger-name logger))))
    (is (eql (length (log4cl::logger-state logger)) log4cl::*num-apps*))))

(deftest make-logger-0 ()
  (with-package-log-hierarchy
    (let ((logger (make-logger :one.two.three.four)))
      (basics logger)
      (is (equal (logger-name logger) "log4cl.test.one.two.three.four")))))

(deftest reset-configuration-0 ()
  ;; verify clear/reset only does so for current configuration
  (with-log-hierarchy ('dummy)
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

(deftest verify-returns-same-logger ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((logger (make-logger :one.two.three)))
      (is (eq logger (make-logger :one.two.three)))
      (is (eq logger (make-logger logger)))
      (is (not (eq logger *root-logger*)))
      (clear-logging-configuration)
      (is (eq logger (make-logger :one.two.three))))))

(deftest logger-by-variable ()
  "Test that we can refer to the logger as variable"
  (with-package-log-hierarchy
    (reset-logging-configuration)
    (let ((logger (make-logger :foo.bar)))
      (is (log-info logger)))))

(deftest inheritance-0 ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (is (null (logger-appenders *root-logger*)))
    (is (null (logger-log-level *root-logger*)))
    (is (eql +log-level-off+ (effective-log-level *root-logger*)))
    (let* ((logger (make-logger :one.two.three)))
      (is (null (logger-log-level logger)))
      (is (eql +log-level-off+ (effective-log-level logger)))
      ;; verify appender is added
      (add-appender *root-logger* (make-console-appender))
      (is (null (logger-appenders logger)))
      (is (not (null (effective-appenders logger)))))))

(deftest inherit-log-levels ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((logger (make-logger :one.two.three)))
      ;; verify no logging
      (is (eql +log-level-off+ (effective-log-level logger)))
      (is (null (log-info)))
      (is (null (log-info logger)))
      ;; now set root log level to info, and verify that
      ;; the levels are right
      (setf (logger-log-level *root-logger*) :info)
      (is (null (logger-log-level logger)))
      (is (eql +log-level-info+ (effective-log-level logger)))
      ;; debugging is still off because of no appenders
      (is (null (log-debug logger)))
      (is (null (log-info logger)))
      ;; add appender, verify debugging is now on
      (add-appender *root-logger* (make-console-appender))
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
      (is (null (log-debug)))
      (is (log-info))
      ;; and same for logger parent
      (is (null (log-debug :one.two)))
      (is (log-info :one.two))
      ;; set root logger off, verify that explicit setting on logger
      ;; is still in effect
      (setf (logger-log-level *root-logger*) :off)
      (is (null (log-debug)))
      (is (null (log-info)))
      (is (log-debug logger))
      (is (log-info logger)))))

(deftest test-stream-appender (&key (cnt 1))
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (add-appender *root-logger* (make-stream-appender
                                 :stream *debug-io*
                                 :layout (log4cl::make-simple-layout)))
    (log-config :info :reset)
    (dotimes (i cnt)
      (log-debug "iter=~d" i))))
