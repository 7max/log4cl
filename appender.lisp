(in-package #:log4cl)

(defclass appender ()
  ((lock :initform (make-lock))
   (layout :initform (make-instance 'default-layout)
           :initarg :layout))
  (:documentation "Appender is the destination of log messages"))

(defclass stream-appender (appender)
  ((stream :initarg :stream))
  (:documentation "Appender that writes message to the specified stream"))

(defclass console-appender (stream-appender)
  ((stream :initform *debug-io*)))

(defmethod appender-do-append ((this stream-appender)
			       level
			       category
			       log-func)
  (with-slots (lock layout stream)
      this
    (with-lock-held (lock)
      (layout-to-stream layout stream level category log-func)
      #+clisp (finish-output *debug-io*)))
  (values))
