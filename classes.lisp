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

(defgeneric appender-do-append (appender logger level log-func)
  (:documentation
   "Writes the log message into the appender. Text of the log message
is specified indirectly via LOG-FUNC argument, which will be a
function that accepts a stream, and writes the text of log message to
it. To simplify writing new appenders all the work on dealing with
layouts had been abstracted into `appender-append-to-stream'
function. So when writing new appender the recommended form for this
method is:

 (defmethod appender-do-append ((this custom-appender) ...)
    (appender-append-to-stream ((some-custom-stream) ... rest of args ...))
    (values))

Return value of this function is ignored"))

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
  ((lock :initform (make-lock))
   (layout :initform (make-instance 'default-layout)
           :initarg :layout))
  (:documentation "Appender is the destination of log messages"))

(defclass stream-appender (appender)
  ((stream :initarg :stream))
  (:documentation "Appender that writes message to the specified stream"))

(defclass console-appender (stream-appender)
  ((stream :initform *debug-io*)))

