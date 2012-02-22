(in-package :log4cl.test)

(in-suite test)
(defsuite* test-property-configurator)

(deftest test-property-configurator-whitespace-and-comments ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((c (make-instance 'property-configurator)))
      (finishes (with-input-from-string (s "") (configure c s)))
      (finishes
        (with-input-from-string
            (s "  # comment
#
one.two=three
#")
          (configure c s)))
      (finishes
        (with-input-from-string (s " one.two  = three")
          (configure c s)))
      (signals property-parser-error
        (with-input-from-string (s "one") (configure c s))))))

(deftest test-property-configurator-1 ()
  (with-package-log-hierarchy
    (let ((config (make-instance 'property-configurator)))
      ;; repeat two times to verify that configurator can be used
      ;; multiple times
      (dotimes (cnt 2)
        (clear-logging-configuration)
        ;; verify (clear-log-configuration) cleared everything
        (is (equal (effective-log-level (make-logger)) +log-level-off+))
        (is (equal 0 (length (effective-appenders (make-logger)))))
        ;; configure
        (with-input-from-string
            (s "log4cl:rootLogger=DEBUG, A1
                log4cl:appender:A1=console-appender")
          (configure
           config s))
        ;; see that changes were made
        (is (equal (effective-log-level (make-logger)) +log-level-debug+))
        (is (equal 1 (length (effective-appenders (make-logger)))))
        (is (typep (first (effective-appenders (make-logger))) 'console-appender))))))

(deftest test-property-configurator-whitespace-and-separator ()
  ;; test using different separator, and that whitespace works
  (with-package-log-hierarchy
    (let ((config (make-instance 'property-configurator)))
      (let ((logger (make-logger '(one two three))))
        (clear-logging-configuration)
        ;; verify (clear-log-configuration) cleared everything
        (is (equal (effective-log-level logger) +log-level-off+))
        (is (equal 0 (length (effective-appenders logger))))
        ;; parse
        (with-input-from-string
            (s "separator =.
              log4cl.logger.one.two.three = DEBUG, A1
              log4cl.appender.A1 = console-appender")
          (configure config s))
        ;; see that changes were made
        (is (equal (effective-log-level logger) +log-level-debug+))
        (is (equal 1 (length (effective-appenders logger))))))))

(deftest test-property-configurator-appender-lower-case ()
  "Verify that specifying appender name in lower case works"
  (with-package-log-hierarchy
    (let ((config (make-instance 'property-configurator)))
      (let ((logger (make-logger '(one two three))))
        (clear-logging-configuration)
        (with-input-from-string
            (s " log4cl:logger:one:two:three = DEBUG, a1
                 log4cl:appender:a1 = Console-Appender")
          (configure config s))
        ;; see that changes were made
        (is (equal (effective-log-level logger) +log-level-debug+))
        (is (equal 1 (length (effective-appenders logger))))))))

(deftest test-property-configurator-boolean-property ()
  (with-package-log-hierarchy
    (let ((config (make-instance 'property-configurator)))
      ;; test giving appender properties works
      (dolist (val '("true" "yes" "on" "t"))
        (clear-logging-configuration)
        (with-input-from-string
            (s (format nil "log4cl:logger:log4cl.test = DEBUG, A1
                            log4cl:appender:A1:immediate-flush = ~a
                            log4cl:appender:A1 = console-appender" val))
          (configure config s))
        (is (equal (effective-log-level (make-logger)) +log-level-debug+))
        (is (equal 1 (length (effective-appenders (make-logger)))))
        (let ((appender (first (effective-appenders (make-logger)))))
          (is (equal t (slot-value appender 'log4cl::immediate-flush)))))
      (dolist (val '("off" "false" "nil" ""))
        (clear-logging-configuration)
        (finishes
          (with-input-from-string
              (s (format nil "log4cl:logger:log4cl.test = DEBUG, A1
                              log4cl:appender:A1:immediate-flush = ~a
                              log4cl:appender:A1 = console-appender" val))
            (configure config s)))
        (let ((appender (first (effective-appenders (make-logger)))))
          (is (equal nil (slot-value appender 'log4cl::immediate-flush))))))))

(deftest test-property-configurator-number-property ()
  (with-package-log-hierarchy
    (let ((config (make-instance 'property-configurator)))
      ;; test giving appender properties works
      (clear-logging-configuration)
      (finishes
        (with-input-from-string
            (s "log4cl:logger:log4cl.test=DEBUG, A1
             	log4cl:appender:A1:flush-interval=123
                log4cl:appender:A1=console-appender")
          (configure config s)))
      (is (equal (effective-log-level (make-logger)) +log-level-debug+))
      (is (equal 1 (length (effective-appenders (make-logger)))))
      (let ((appender (first (effective-appenders (make-logger)))))
        (is (equal 123 (slot-value appender 'log4cl::flush-interval)))))))

(deftest test-property-configurator-errors ()
  (with-package-log-hierarchy
    (let* ((config (make-instance 'property-configurator))
           remembered-error)
      (macrolet ((remember-error (&body body)
                   `(handler-bind ((serious-condition
                                     (lambda (c)
                                       (setq remembered-error c))))
                      (setq remembered-error nil)
                      ,@body)))
        ;; Test invalid numeric property
        (signals property-parser-error
          (remember-error
           (with-input-from-string
               (s "log4cl:logger:log4cl.test=DEBUG, A1
                   log4cl:appender:A1:flush-interval=blah
                   log4cl:appender:A1=console-appender")
             (configure config s))))
        ;; Check that it logged error with the right line number
        (is (search "line 2" (format nil "~A" remembered-error)))
        (clear-logging-configuration)
        ;; Test with non-existing property
        (signals property-parser-error
          (remember-error
           (with-input-from-string
               (s "log4cl:logger:log4cl.test=DEBUG, A1
                   log4cl:appender:A1=console-appender
                   log4cl:appender:A1:non-existent-property=whatever")
             (configure config s))))
        ;; Check that it logged error with the right line number
        (is (search "line 3" (format nil "~A" remembered-error)))
        (clear-logging-configuration)
        ;; Test with non-existent appender
        (signals property-parser-error
          (remember-error
           (with-input-from-string
               (s "log4cl:logger:log4cl.test=DEBUG, A2
                   log4cl:appender:A1=console-appender
                   # comment
                   log4cl:appender:A1:non-existent-property=whatever")
             (configure config s))))
        ;; Check that it logged error with the right line number
        (is (search "line 1" (format nil "~A" remembered-error)))
        ;; Test with non-existing class
        (signals property-parser-error
          (remember-error
           (with-input-from-string
               (s "log4cl:logger:log4cl.test=DEBUG, A1
                   # comment
                   log4cl:appender:A1 = no such class
                   log4cl:appender:A1:non-existent-property=whatever")
             (configure config s))))
        ;; Check that it logged error with the right line number
        (is (search "line 3" (format nil "~A" remembered-error)))))))


(deftest test-property-configurator-with-daily-file-appender ()
  "A copy of the TEST-DAILY-FILE-APPENDER-1 but with configuration
done via property configurator, rather then directly"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((fname-base (merge-pathnames (rand-filename) *tests-dir*))
           (name-format (format nil "~a-%H-%M-%S.log" fname-base))
           (config (make-instance 'property-configurator))
           (logger (make-logger '(blah crap baz))))
      (finishes 
        (with-input-from-string
            (s (format
                nil "log4cl:logger:blah:crap:baz=INFO, DAILY
                     log4cl:appender:DAILY=daily-file-appender
                     log4cl:appender:DAILY:rollover-check-period=1
                     log4cl:appender:DAILY:name-format=~a" name-format))
          (configure config s)))
      (is (equal 1 (length (logger-appenders logger))))
      (is (log-info logger))
      (log-info logger "Hey")
      (let* ((a (first (logger-appenders logger)))
             (fname1 (appender-filename a)))
        (sleep 1.2)
        (log-info logger "Hey again")
        (let ((fname2 (appender-filename a)))
          (log-sexp fname1 fname2)
          (unwind-protect
               (progn
                 (is (not (equal fname1 fname2)))
                 (with-open-file (s fname1)
                   (is (equal (read-line s) "INFO - Hey")))
                 (remove-all-appenders logger)
                 ;; verify it got closed
                 (with-open-file (s fname2)
                   (is (equal (read-line s) "INFO - Hey again"))))
            (ignore-errors (delete-file fname1))
            (ignore-errors (delete-file fname2))))))))

(deftest test-property-configurator-with-pattern-layout ()
  "A copy of previous test, but we add a pattern layout"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((fname-base (merge-pathnames (rand-filename) *tests-dir*))
           (name-format (format nil "~a-%H-%M-%S.log" fname-base))
           (config (make-instance 'property-configurator))
           (logger (make-logger '(bar baz))))
      (finishes 
        (with-input-from-string
            (s
             (format
              nil
              "log4cl:logger:bar:baz=DEBUG, DAILY
               log4cl:appender:DAILY:layout=pattern-layout
               log4cl:appender:DAILY:layout:conversion-pattern=%p %c %m
               log4cl:appender:DAILY=daily-file-appender
               log4cl:appender:DAILY:rollover-check-period=1
               log4cl:appender:DAILY:name-format=~a" name-format))
          (configure config s)))
      (is (equal 1 (length (logger-appenders logger))))
      (is (log-debug logger))
      (log-info logger "Hey")
      (let* ((a (first (logger-appenders logger)))
             (fname1 (appender-filename a)))
        (sleep 1.2)
        (log-debug logger "Hey again")
        (let ((fname2 (appender-filename a)))
          (unwind-protect
               (progn
                 (is (not (equal fname1 fname2)))
                 (with-open-file (s fname1)
                   (is (equal (read-line s)
                              (format nil "INFO ~s:~s Hey"
                                      'bar
                                      'baz))))
                 (remove-all-appenders logger)
                 ;; verify it got closed
                 (with-open-file (s fname2)
                   (is (equal (read-line s) 
                              (format nil "DEBUG ~s:~s Hey again"
                                      'bar
                                      'baz)))))
            (ignore-errors (delete-file fname1))
            (ignore-errors (delete-file fname2))))))))

