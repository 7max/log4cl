(in-package :log4cl-impl)
;;
;; Define the log levels. Similar to Log4J, except that we add extra
;; nice log levels named "user1" through "user9" with "trace" log
;; level in-between user4 and user5
;;
;; Reasoning behind extra levels is: 
;;
;;  1. Unlike log4j the design of log4cl allows for mulitple log levels
;;     to be enabled simultaneously.
;;     
(defconstant +log-level-unset+  16)
(defconstant +log-level-user9+  15)
(defconstant +log-level-user8+  14)
(defconstant +log-level-user7+  13)
(defconstant +log-level-user6+  12)
(defconstant +log-level-user5+  11)
(defconstant +log-level-trace+  10)
(defconstant +log-level-user4+  9)
(defconstant +log-level-user3+  8)
(defconstant +log-level-user2+  7)
(defconstant +log-level-user1+  6)
(defconstant +log-level-debug+  5)
(defconstant +log-level-info+   4)
(defconstant +log-level-warn+   3)
(defconstant +log-level-error+  2)
(defconstant +log-level-fatal+  1)
(defconstant +log-level-off+    0)
(defconstant +min-log-level+ +log-level-fatal+)
(defconstant +max-log-level+ +log-level-user9+)

;; For converting log levels from string
(defparameter +log-level-from-letter+ "OFEWID1234T56789U")
(defparameter +log-level-from-string+ 
  '("OFF" "FATAL" "ERROR" "WARN" "INFO"
    "DEBUG" "USER1" "USER2" "USER3" "USER4" "TRACE"
    "USER5" "USER6" "USER7" "USER8" "USER9" "UNSET"))

;; For converting level to string
(defparameter +log-level-to-string+
  (coerce '("OFF" "FATAL" "ERROR" "WARN" "INFO" "DEBUG" 
            "USER1" "USER2" "USER3" "USER4" "TRACE" "USER5" "USER6"
            "USER7" "USER8" "USER9")
          'simple-vector))

(defparameter +log-level-to-lc-string+
  (map 'simple-vector #'string-downcase +log-level-to-string+))

(defvar *log-indent* 0
  "Indent level can be used to indent logging info, is printed by %I
pattern format")

(defvar *ndc-context* nil
  "Value that is printed by %x pattern format")

(defvar *log-event-time* nil
  "Value of (GET-UNIVERSAL-TIME) for the current log event")

(defvar *inside-user-log-function* nil
  "True when we are inside of user log function, used to distinguish
errors that are signaled the log statement itself, vs errors in layout
or appender.")

(define-condition log4cl-error (simple-error program-error) ()
  (:documentation "Base class for all LOG4CL errors"))

(defun log4cl-error (message &rest args)
  (error 'log4cl-error
         :format-control message
         :format-arguments args))
