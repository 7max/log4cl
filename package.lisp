(cl:in-package #:log4cl-impl)

(macrolet ((log4cl-defpackage ()
             (labels ((reexport-from (name names)
                        `((:import-from ,name ,@names)
                          (:export ,@names))))
               `(defpackage #:log4cl
                  (:use)
                  (:nicknames #:log)
                  ,@(reexport-from
                     '#:log4cl-impl
                     '(;; class names
                       #:fixed-stream-appender
                       #:console-appender
                       #:file-appender
                       #:daily-file-appender
                       #:property-configurator
                       #:simple-layout
                       #:pattern-layout
                       #:with-log-hierarchy
                       #:with-package-log-hierarchy
                       #:clear-logging-configuration
                       #:reset-logging-configuration
                       ;; utility stuff
                       #:make-logger
                       #:configure
                       #:add-appender
                       #:remove-appender
                       #:remove-all-appenders
                       #:logger-additivity
                       #:logger-category
                       #:logger-name
                       #:logger-parent))
                  (:import-from :cl #:in-package)
                  (:shadow #:sexp #:expr #:config #:make ,@+log-level-symbols+)
                  (:export #:sexp #:expr #:config #:make ,@+log-level-symbols+)
                  ;; one letter logging macro forwarders
                  (:shadow #:f #:e #:w #:i #:d #:u1 #:u2 #:u3 #:u4 #:t #:u5 #:u6 #:u7 #:u8 #:u9 #:c #:s)
                  (:export #:f #:e #:w #:i #:d #:u1 #:u2 #:u3 #:u4 #:t #:u5 #:u6 #:u7 #:u8 #:u9 #:c #:s)))))
  (log4cl-defpackage))

(defmacro forward-logging-macro (name from-name)
  `(defmacro ,name (&rest args)
     ,(documentation from-name 'function)
     `(,',from-name ,@args)))

(defmacro forward-levels (levels)
  (let ((defs
          (loop for level in levels
                as macro-name = (intern (symbol-name level) :log4cl)
                as forward-name = (or (find-symbol (format nil "~A-~A"
                                                           (symbol-name 'log)
                                                           (symbol-name level))
                                                   :log4cl-impl)
                                      (error "Unable to find logging macro for ~S" level))
                collect `(forward-logging-macro ,macro-name ,forward-name))))
    `(progn
       ,@defs)))


(forward-levels #.+log-level-macro-symbols+)
(forward-levels (#:sexp #:config))

;; make (log:expr) same as (log:sexp) and (log:make) shortcut for (log:make-logger)
(forward-logging-macro log:expr log4cl-impl:log-sexp)
(forward-logging-macro log:make log4cl-impl:make-logger)

;; one letter logging macros
(forward-logging-macro log:f log4cl-impl:log-fatal)
(forward-logging-macro log:e log4cl-impl:log-error)
(forward-logging-macro log:w log4cl-impl:log-warn)
(forward-logging-macro log:i log4cl-impl:log-info)
(forward-logging-macro log:d log4cl-impl:log-debug)
(forward-logging-macro log:u1 log4cl-impl:log-user1)
(forward-logging-macro log:u2 log4cl-impl:log-user2)
(forward-logging-macro log:u3 log4cl-impl:log-user3)
(forward-logging-macro log:u4 log4cl-impl:log-user4)
(forward-logging-macro log:t log4cl-impl:log-trace)
(forward-logging-macro log:u5 log4cl-impl:log-user5)
(forward-logging-macro log:u6 log4cl-impl:log-user6)
(forward-logging-macro log:u7 log4cl-impl:log-user7)
(forward-logging-macro log:u8 log4cl-impl:log-user8)
(forward-logging-macro log:u9 log4cl-impl:log-user9)
(forward-logging-macro log:c log4cl-impl:log-config)
(forward-logging-macro log:s log4cl-impl:log-sexp)

