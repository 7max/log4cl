(in-package #:log4cl)

(defstruct appender
  (lock (make-lock))
  (layout (make-default-layout) :type layout))

(defun append-to-stream (stream appender level category log-func)
  "Helper function for appenders. Does all the work for using the
appender's layout to output to the specified stream"
  (declare (type stream stream)
	   (type appender appender)
	   (type fixnum level)
	   (type string category)
	   (type function  log-func))
  (layout-to-stream (appender-layout appender)
		    stream level category log-func)
  (values))

(defstruct (console-appender
	     (:include appender
		       (layout (make-default-layout)))))

(defmethod appender-do-append ((this console-appender)
			       (level integer)
			       (category string)
			       (log-func function))
  (with-lock-held ((appender-lock this))
    (append-to-stream *debug-io* this level category log-func)
    #+clisp (finish-output *debug-io*))
  (values))


(defstruct (stream-appender
            (:include appender
             (layout (make-default-layout))))
  (stream *debug-io* :type stream))

(defmethod appender-do-append ((this stream-appender)
			       (level integer)
			       (category string)
			       (log-func function))
  (with-lock-held ((appender-lock this))
    (append-to-stream (stream-appender-stream this)
                      this level category log-func)
    #+clisp (finish-output *debug-io*))
  (values))
