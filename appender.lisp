(in-package #:log4cl)

(defmethod appender-do-append ((this stream-appender)
                               logger
			       level
                               log-func)
  (with-slots (lock layout stream)
      this
    (with-lock-held (lock)
      (layout-to-stream layout stream logger level log-func)
      #+clisp (finish-output *debug-io*)))
  (values))
