(in-package #:log4cl)

(defmethod appender-do-append :around
    ((this serialized-appender) logger level log-func)
  (with-lock-held ((slot-value this 'lock))
    (call-next-method)))

(defmethod appender-do-append ((this stream-appender) logger level log-func)
  (layout-to-stream (slot-value this 'layout) (appender-stream this)
                    logger level log-func)
  (values))

;; Save one generic function dispatch by accessing STREAM slot directly
(defmethod appender-do-append ((this fixed-stream-appender)
                               logger
			       level
                               log-func)
  (layout-to-stream (slot-value this 'layout)
                    (slot-value this 'stream)
                    logger level log-func)
  (values))

(defmethod appender-stream ((this console-appender))
  "Returns *DEBUG-IO*"
  *debug-io*)
