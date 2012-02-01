(in-package #:log4cl)

(defclass appender ()
  ((layout :initform (make-instance 'simple-layout)
           :initarg :layout))
  (:documentation "Appender is log message sink, and is responsible
for physically delivering the log message, somewhere. The formatting
of message is done by layout.

Appenders can be called from multiple threads and are responsible for
serializing access to any resources."))

(defclass serialized-appender (appender)
  ((lock :initform (make-lock)))
  (:documentation "Appender that serializes itself using a lock"))

(defclass stream-appender (serialized-appender)
  ((immediate-flush  :initform NIL :initarg :immediate-flush
                     :type boolean)
   (flush-interval   :initform 1 :initarg :immediate-flush
                     :type fixnum)
   (last-flush-time  :initform 0 :type unsigned-byte))
  (:documentation "Appender that writes message to stream returned by
  APPENDER-STREAM generic function.

Properties:

  - IMMEDIATE-FLUSH When non-NIL will call FINISH-OUTPUT after every
    log message

  - FLUSH-INTERVAL When set, will only flush if previous flush was
    that many seconds ago. Will only be used if IMMEDIATE-FLUSH is NIL"))

(defgeneric appender-stream (appender) 
  (:documentation "Should return the stream to which appender will write log messages"))

(defclass fixed-stream-appender (stream-appender)
  ((stream :initarg :stream :accessor appender-stream))
  (:documentation "Appender that writes message to the stream specified in STREAM slot"))

(defclass console-appender (stream-appender) () 
  (:documentation "A stream appender that writes messages to
*debug-io* stream.  The *debug-io* is late-binding, that is its the
value of that variable in the thread and at the moment of log message
being written.  If instead you want an appender that would write log
messages to the *debug-io* stream active when appender was created,
use FIXED-STREAM-APPENDER class"))

(defgeneric appender-do-append (appender logger level log-func)
  (:documentation
   "Writes the log message into the appender. Text of the log message
is specified indirectly via LOG-FUNC argument, which will be a
function that accepts a stream, and writes the text of log message to
it.

  This function should first figure out or obtain the stream to write
the log message to, and then call the LAYOUT-TO-STREAM function to have
layout do actual formatting.

If appender destination is ultimately not a stream, then it can
obtain the full text of the log message by calling LAYOUT-TO-STREAM
inside of WITH-OUTPUT-TO-STRING

Example:

 (defmethod appender-do-append ((self custom-appender) logger level log-func)
   (let ((stream (custom-appender-destination)))
     (layout-to-stream (slot-value self 'layout)
                       stream logger level log-func))
   (values))

Return value of this function is ignored"))

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
