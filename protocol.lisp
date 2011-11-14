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

