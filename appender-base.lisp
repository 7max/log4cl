(in-package :log4cl)

;; Base APPENDER class and generics that logging core needs to know
;; about

(defclass appender ()
  ((layout :initform (make-instance 'simple-layout)
           :initarg :layout)
   (error :initform nil :accessor appender-error)
   (logger-count :initform 0 :accessor appender-logger-count
                 :type (integer 0 most-positive-fixnum)))
  (:documentation "Appender is log message sink, and is responsible
for physically delivering the log message, somewhere. The formatting
of message is done by layout.

Appenders can be called from multiple threads and are responsible for
serializing access to any resources.

Appender will not be appended into if its ERROR slot is non-nil.

ERROR slot will be automatically set to the condition object, if a
condition was raised while writing to the appender"))

(defgeneric appender-added (logger appender)
  (:documentation "Called when appender is added to a logger. Default
method is used to keep logger count, and if re-implemented
the (CALL-NEXT-METHOD) needs to be called."))

(defgeneric appender-removed (logger appender)
  (:documentation "Called when appender is removed from a logger
logger. Default method is used to keep logger refcount, and calls
CLOSE-APPENDER when it reaches zero. If re-implemented
the (CALL-NEXT-METHOD) needs to be called"))

(defgeneric close-appender (appender)
  (:documentation "Called when appender refcount reaches zero after
being positive. Should close any streams or files that appender had
opened."))

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

(defgeneric handle-appender-error (appender condition)
  (:documentation "Called when a condition is raised doing writing to
  the appender by APPENDER-DO-APPEND call.

  Before this method is called, the ERROR slot of the appender is set
  to CONDITION, thus preventing this appender from being used
  recursively. Thus its safe to use log statements from inside methods
  implementing this function

  Default method will log the condition.

  If this method resets the ERROR slot of the appender back to NIL, it
  is assumed that method had fixed whatever was wrong the appender,
  and the APPENDER-DO-APPEND will be attempted again"))
