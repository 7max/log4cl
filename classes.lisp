;;;
;;; Generic functions used to extend log4cl. All generic functions
;;; specialize on the current package, allowing packages extend or
;;; customize log4cl without affecting other packages

(in-package :log4cl)

(defgeneric log-level-from-object (obj package)
  (:documentation "Should return numeric log level from the user
representation, can be specialized per-package to have custom log
level names. Default implementation converts object to string and
parses \"fatal\" \"debug\" and so on. Called by MAKE-LOG-LEVEL
function"))

(defgeneric naming-option (package option)
  (:documentation "Return the automatic logger naming option
for the specified package. Valid options are:

   :CATEGORY-SEPARATOR - character or string that separates category names
                         default method returns colon
   :CATEGORY-CASE      - When determining automatic logger name from a symbol
                         how to handle case. Valid values are:

                         :READTABLE  use current readtable
                         :UPCASE     convert to upper case
                         :DOWNCASE   convert to lower case
                         :INVERT     invert, as inverted readtables do"))

(defgeneric resolve-logger-form (package env args)
  (:documentation "Is called by all logging macros such as to figure
out the logger to log into. PACKAGE and ENV are the current value of
*PACKAGE* and the macro environment of the logging macro, and ARG and
MORE-ARGS are its arguments.

Returns two values, first being either a logger, or a form that when
evaluated will return a logger, and second value being list of
arguments to be passed to the format statement that will log the
message.

When second value returned is NIL, then logging macro will not log any
message but will rather expand into a non-nil value if logger is
active and has appenders.

"))

(defgeneric resolve-default-logger-form (package env args)
  (:documentation "Is called by RESOLVE-LOGGER-FORM when logging macro
arguments do not specify the logger to log into. See
RESOLVE-LOGGER-FORM for return values"))


(defgeneric layout-to-stream (layout stream logger level log-func)
  (:documentation
   "Prints the log message to the specified stream. log message can is
specified indirectly by LOG-FUNC argument, which is a callable object
that accepts a stream and writes log message to it"))

(defclass layout () ()
  (:documentation "Abstract layout class"))

(defclass simple-layout (layout) ()
  (:documentation
   "Simple layout that log messages like so: \"<level> - <log message>\\n\"

For example:

INFO - test message
"))

(defclass default-layout (layout) ()
  (:documentation "Default layout that prints category, log level and the message.

Example:

 (log4cl.test) DEBUG - test message"))

(defclass appender ()
  ((layout :initform (make-instance 'default-layout)
           :initarg :layout))
  (:documentation "Appender is log message sink, and is responsible
for physically delivering the log message, somewhere. The formatting
of message is done by layout.

Appenders can be called from multiple threads and are responsible for
serializing access to any resources."))

(defclass serialized-appender (appender)
  ((lock :initform (make-lock)))
  (:documentation "Appender that serializes itself using a lock"))

(defclass stream-appender (serialized-appender) () 
  (:documentation "Appender that writes message to stream returned by APPENDER-STREAM function"))

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


