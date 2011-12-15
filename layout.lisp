(in-package #:log4cl)

(defstruct layout)
(defstruct (simple-layout (:include layout)))
(defstruct (default-layout (:include layout)))

(declaim (inline write-log-level))

(defun write-log-level (level stream)
  "Write the log level name to the stream"
  ;; for some reason using ~[ in format generates tons
  ;; of garbage on SBCL for me, so just do it manually
  (case level
    (1 (write-string "FATAL" stream))
    (2 (write-string "ERROR" stream))
    (3 (write-string "WARN" stream))
    (4 (write-string "INFO" stream))
    (5 (write-string "DEBUG" stream))
    (6 (write-string "TRACE" stream))
    (7 (write-string "USER1" stream))
    (8 (write-string "USER2" stream))
    (9 (write-string "USER3" stream))
    (10 (write-string "USER4" stream))
    (11 (write-string "USER5" stream))
    (12 (write-string "USER6" stream))
    (13 (write-string "USER7" stream))
    (14 (write-string "USER8" stream))
    (15 (write-string "USER9" stream))))

(defmethod layout-to-stream ((layout default-layout)
			     stream
			     level
			     category
			     log-func)
  "Format the user log statement with the default layout"
  (declare (type stream stream)
           (type fixnum level)
           (type string category)
           (type function log-func))
  (write-log-level level stream)
  (loop repeat *log-indent* do (write-string  "  " stream))
  (when (plusp (length category))
    (write-string " " stream)
    (write-string category stream))
  (write-string ": " stream)
  (funcall log-func stream)
  (terpri stream)
  (values))

(defmethod layout-to-stream ((layout simple-layout)
			     stream
			     level
			     category
			     log-func)
  "Format the user log statement with the simple layout"
  (declare (type stream stream)
           (type fixnum level)
           (type string category)
           (type function log-func))
  (write-log-level level stream)
  (write-string " - " stream)
  (funcall log-func stream)
  (terpri stream)
  (values))

