(in-package #:cl-log)

(defstruct layout)

(defgeneric layout-to-stream (layout stream level category
				     log-func)
  (:documentation
   "Prints the log message to the specified stream. The user-format and
user-args arguments are suitable for using in ~? (indirect format) directive
of the `format' function."))

(defstruct (simple-layout (:include layout)))
(defstruct (default-layout (:include layout)))

(defstruct appender
  (lock (make-lock))
  (layout (make-default-layout) :type layout))

(defgeneric appender-do-append (appender level category log-func)
  (:documentation
   "Writes the log message into the appender. To simplify writing new
appenders all the work on dealing with layouts had been abstracted into
`appender-append-to-stream' function. So when writing new appender
the recommended form for this method is:

  (defmethod appender-do-append ((this custom-appender) ...)
    (appender-append-to-stream ((some-custom-stream) ... rest of args ...))
    (values))

Return value of this function is ignored"))

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

(declaim (inline write-log-level))

(defmethod layout-to-stream ((layout default-layout)
			     (stream stream)
			     (level integer)
			     (category string)
			     (log-func function))
  "Format the user log statement with the simple layout"
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
			     (stream stream)
			     (level integer)
			     (category string)
			     (log-func function))
  "Format the user log statement with the simple layout"
  (write-log-level level stream)
  (write-string " - " stream)
  (funcall log-func stream)
  (terpri stream)
  (values))
  
(defun append-to-stream (stream appender level category log-func)
  "Helper function for appenders. Does all the work for using
the appender's layout to output to the specified stream"
  (declare (type stream stream)
	   (type appender appender)
	   (type fixnum level)
	   (type string category)
	   (type function  log-func))
  (layout-to-stream (appender-layout appender)
		    stream level category log-func)
  (values))
