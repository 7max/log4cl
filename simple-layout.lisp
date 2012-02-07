(in-package #:log4cl)

(defclass simple-layout (layout) ()
  (:documentation
   "Simple layout outputs log level and user message separated by
dash. For example: INFO - user log message"))

(declaim (inline write-log-level))

(defun write-log-level (level stream)
  "Print the log LEVEL's name to the STREAM"
  (write-string (log-level-to-string level) stream)
  (values))

(defmethod layout-to-stream ((layout simple-layout)
			     stream
                             logger
			     level
                             log-func)
  "Format the log message with the simple layout"
  (declare (ignore logger))
  (write-log-level level stream)
  (write-string " - " stream)
  (call-user-log-message log-func stream)
  (terpri stream)
  (values))

