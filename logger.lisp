;;;
;;; Global variables, logger and logger-state structures, and utility functions
;;;
(in-package :log4cl)

(defvar *hierarchy-max* 1
  "Number of hierarchies registered with the log4cl library. Each
hierarchy has independent configuration with regards to log levels and
appenders")

(defvar *hierarchy* 0
  "Active hierarchy index. All logging functions use logger state
indexed by this variable. Can be assigned directly or ")

(defvar *log-indent* 0
  "Indent level can be used to indent logging info")

(defvar *name-to-hierarchy* (let ((table (make-hash-table)))
                              (setf (gethash :default table) 0)
                              table)
  "EQL hash table mapping hierarchy identifier to hierarchy index")

(defvar *hierarchy-lock*
  (make-recursive-lock "hierarchy-lock")
  "Global lock for changing logging configuration")

(declaim (special *root-logger*)
         (type logger *root-logger*)
	 (type fixnum  *hierarchy-max* *hierarchy* *log-indent*)
	 (type hash-table *name-to-hierarchy*)
         (inline is-enabled-for current-state hierarchy-index))

(defstruct logger-state
  "Structure containng logger internal state"
  ;; list of appenders attached to this logger
  (appenders nil :type list)
  ;; explicit log level for this logger if its set
  (level nil :type (or null fixnum))
  ;; If true then log level and appenders are inherited from
  ;; parent logger
  (additivity t :type boolean)
  ;; performance hack, for each log level we set a bit in mask, if a
  ;; log message with said level will reach any appenders either in
  ;; this logger or it's parents
  ;;
  ;; update-logger-mask function recalculates this value for parent
  ;; and child loggers
  (mask 0 :type fixnum))

(defstruct (logger (:constructor create-logger)
                   (:print-function 
                    (lambda  (logger stream depth)
                      (declare (ignore depth))
                      (let ((*print-circle* nil))
                        (print-unreadable-object (logger stream)
                          (princ "LOGGER " stream)
                          (princ (if (zerop (length (logger-name logger))) "+ROOT+"
                                     (logger-name logger)) stream))))))
  ;; logger name in root.parent-logger.logger format
  (name      nil :type string)
  ;; parent logger, only nil for the root logger
  (parent    nil :type (or null logger))
  ;; child loggers
  (children  nil :type (or null hash-table))
  ;; per-hierarchy configuration
  (state (map-into (make-array *hierarchy-max*) #'make-logger-state)
   :type (simple-array logger-state *)))

;;
;; Define the log levels. Similar to Log4J, except that we add extra
;; nice log levels named "user1" through "user9" with "trace" log
;; level in-between user4 and user5
;;
;; Reasoning behind extra levels is: 
;;
;;  1. Unlike log4j where the design allows multiple log levels to be
;;     active simultaneously (altho currently this is not used)
;;
;;  2. Does not introduce any more complexity then needed.
;;
(defconstant +log-level-unset+  16)
(defconstant +log-level-user9+  15)
(defconstant +log-level-user8+  14)
(defconstant +log-level-user7+  13)
(defconstant +log-level-user6+  12)
(defconstant +log-level-user5+  11)
(defconstant +log-level-trace+  10)
(defconstant +log-level-user4+  9)
(defconstant +log-level-user3+  8)
(defconstant +log-level-user2+  7)
(defconstant +log-level-user1+  6)
(defconstant +log-level-debug+  5)
(defconstant +log-level-info+   4)
(defconstant +log-level-warn+   3)
(defconstant +log-level-error+  2)
(defconstant +log-level-fatal+  1)
(defconstant +log-level-off+    0)
(defconstant +min-log-level+ +log-level-fatal+)
(defconstant +max-log-level+ +log-level-user9+)

;; For converting log levels from string
(defparameter +log-level-from-letter+ "OFEWID1234T56789U")
(defparameter +log-level-from-string+ 
  '("OFF" "FATAL" "ERROR" "WARN" "INFO"
    "DEBUG" "USER1" "USER2" "USER3" "USER4" "TRACE"
    "USER5" "USER6" "USER7" "USER8" "USER9" "UNSET"))

(defun make-log-level (arg)
  "Translate a more human readable log level into one of the log
level constants, by calling LOG-LEVEL-FROM-OBJECT on ARG and current
value of *PACKAGE* "
  (log-level-from-object arg *package*))

(defmethod resolve-logger-form (package env args)
  "- When first element of args is NIL or a constant string, calls
  RESOLVE-DEFAULT-LOGGER-FORM that will try to obtain logger name from
  the environment

- When first argument is a :KEYWORD, returns logger named <keyword>

- When first argument is a quoted symbol, returns logger named
  <current-package>.<symbol>

- Otherwise returns the form `(GET-LOGGER ,(FIRST ARGS) ,@(REST ARGS))'"
  (cond
    ((or (null args)
         (stringp (first args)))
     (resolve-default-logger-form package env args))
    ((keywordp (first args))
     (values (get-logger (logger-name-from-symbol (first args) env))
             (rest args)))
    ((constantp (first args))
     (let ((value (eval (first args))))
       (if (symbolp value)
           (get-logger (logger-name-from-symbol value env))
           (values (first args) (rest args)))))
    (t
     (values (first args) (rest args)))))

(defun effective-log-level (logger)
  "Return logger's own log level (if set) or the one it
had inherited from parent"
  (declare (type (or null logger) logger))
  (if (null logger) +log-level-off+
      (let* ((state (current-state logger))
	     (level (logger-state-level state)))
	(or level
	    (effective-log-level (logger-parent logger))))))

(defun have-appenders-for-level (logger level)
  "Return non-NIL if logging with LEVEL will actually
reach any appenders"
  (declare (type logger logger) (type fixnum level))
  ;; Note: below code actually walk parent chain twice but since its
  ;; cached anyway, don't optimize unless becomes a problem
  (labels ((have-appenders (logger)
	     (when logger
	       (or (logger-state-appenders (current-state logger))
		   (have-appenders (logger-parent logger))))))
    (let* ((logger-level (effective-log-level logger)))
      (and (>= logger-level level)
	   (have-appenders logger)))))

(defun map-logger-children (function logger)
  "Apply the function to all of logger's children (but not their
descedants)"
  (let ((child-hash (logger-children logger)))
    (when child-hash
      (maphash (lambda (name logger)
                 (declare (ignore name))
                 (funcall function logger))
               child-hash))))

(defun adjust-logger (logger)
  "Recalculate LOGGER mask by finding out which log levels have
reachable appenders. "
  (labels ((doit (logger)
             (let ((state (current-state logger))
                   (mask 0))
               (declare (type fixnum mask))
               (loop
                  for level from +min-log-level+ upto +max-log-level+
                  if (have-appenders-for-level logger level)
                  do (setf mask (logior mask (ash 1 level))))
               (setf (logger-state-mask state) mask)
               (map-logger-children #'doit logger))))
    (doit logger))
  (values))

(defun get-logger (name)
  "Get the logger with a given category name. Logger is created
if it does not exist"
  (declare (type (or string null) name))
  (cond ((or (null name)
             (zerop (length name))) *root-logger*)
        (t (setq name (string-downcase name))
           (let* ((dot-pos (position #\. name :from-end t))
                  (parent (if (null dot-pos) *root-logger*
                              (get-logger (subseq name 0 dot-pos))))
                  (logger (let ((child-hash (logger-children parent)))
                            (when child-hash
                              (gethash name child-hash)))))
             (unless logger
               (setq logger (create-logger :name name :parent parent))
               (unless (logger-children parent)
                 (setf (logger-children parent) (make-hash-table :test #'equal)))
               (setf (gethash name (logger-children parent)) logger)
               (dotimes (*hierarchy* *hierarchy-max*)
                 (adjust-logger logger)))
             logger))))

(defun current-state (logger)
  (svref (logger-state logger) *hierarchy*))

(defun is-enabled-for (logger level)
  "Returns t if log level is enabled for the logger in the
context of the current application."
  (declare (type logger logger) (type fixnum level))
  (let* ((state (current-state logger))
	 (mask (logger-state-mask state)))
    (declare (type fixnum mask))
    (not (zerop (logand (the fixnum (ash 1 level)) mask)))))

(defun expand-log-with-level (env level args)
  "Returns a FORM that is used as an expansion of log-nnnnn macros"
  (declare (type fixnum level)
	   (type list args))
  (multiple-value-bind (logger-form args)
      (resolve-logger-form *package* env args)
    (let* ((logger-symbol (gensym "logger"))
           (log-stmt (gensym "log-stmt")))
      (if args 
          `(let ((,logger-symbol ,logger-form))
             #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
             (when
                 (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                   (is-enabled-for ,logger-symbol ,level))
               (flet ((,log-stmt (stream)
                        (declare (type stream stream))
                        (format stream ,@args)))
                 (declare (dynamic-extent #',log-stmt))
                 (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                   (log-with-logger ,logger-symbol ,level #',log-stmt))))
             (values))
          ;; null args means its being used for checking if level is enabled
          ;; in the (when (log-debug) ... complicated debug ... ) way
          `(let ((,logger-symbol ,logger-form))
             (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
               (is-enabled-for ,logger-symbol ,level)))))))

(defun substr (seq from &optional (to (length seq)))
  (declare (fixnum from to)
           (vector seq))
  (make-array (- to from) :element-type (array-element-type seq)
                          :displaced-to seq :displaced-index-offset from))

(defun logger-name-from-symbol (symbol env)
  "Return a logger name from a symbol."
  (declare (type keyword symbol) (ignore env))
  (format nil "~(~a.~a~)" (shortest-package-name *package*)
          (symbol-name symbol)))

(defun log-with-logger (logger level log-func)
  "Submit message to logger appenders, and its parent logger"
  (labels ((log-to-logger-appenders (logger orig-logger level log-func)
	     (let* ((state (current-state logger))
		    (appenders
		     (logger-state-appenders state)))
	       (dolist (appender appenders)
		 (appender-do-append appender level (logger-name orig-logger)
				     log-func)))
	     (let ((parent (logger-parent logger)))
	       (when  parent
		 (log-to-logger-appenders parent orig-logger level log-func)))
	     (values)))
    (log-to-logger-appenders logger logger level log-func)
    (values)))

(defun logger-log-level (logger)
  "Return the logger own log level or NIL if unset. Please note that
by default loggers inherit their parent logger log level, see
EFFECTIVE-LOG-LEVEL"
  (declare (type logger logger))
  (logger-state-level
   (current-state logger)))

(defun logger-appenders (logger)
  "Return the list of logger's own appenders. Please note that by
default loggers inherit their parent logger appenders, see
EFFECTIVE-APPENDERS"
  (declare (type logger logger))
  (logger-state-appenders (current-state logger)))

(defun effective-appenders (logger)
  "Return the list of all appenders that logger output could possible go to,
including inherited one"
  (loop for tmp = logger then (logger-parent tmp)
        while tmp
        append (logger-state-appenders (current-state tmp))))

(defun (setf logger-log-level) (level logger)
  "Set logger log level. Returns logger own log level"
  (declare (type logger logger))
  (nth-value 1 (set-log-level logger level)))

(defun set-log-level (logger level &optional (adjust-p t))
  "Set the log level of a logger. Log level is passed to
MAKE-LOG-LEVEL to determine canonical log level. ADJUST-P controls if
logger effective log level needs to be recalculated, the caller should
NIL doing bulk operations that change the level of many loggers, as to
avoid overhead.

Returns if log level had changed as the 1st value and new level as the
second value."
  (declare (type logger logger))
  (let* ((level (make-log-level level))
         (state (current-state logger))
         (old-level (logger-state-level state))
         (new-level (when (/= level +log-level-unset+) level)))
    (declare (type fixnum level)
             (type logger-state state)
             (type (or null fixnum) old-level new-level))
    (unless (eql old-level new-level)
      (setf (logger-state-level state) new-level)
      (when adjust-p
        (adjust-logger logger))
      (values t new-level))))

(defun add-appender (logger appender)
  (declare (type logger logger) (type appender appender))
  (let* ((state (current-state logger)))
    (unless (member appender (logger-state-appenders state))
      (push appender (logger-state-appenders state))
      (adjust-logger logger)))
  (values))

(defun %hierarchy-index (name)
  (when (stringp name)
    (setq name (intern name)))
  (let ((index (gethash name *name-to-hierarchy*)))
    (unless index
      (with-recursive-lock-held (*hierarchy-lock*)
        (adjust-all-loggers-state (1+ *hierarchy-max*))
        (setf index *hierarchy-max*)
        (setf (gethash name *name-to-hierarchy*) index)
        (incf *hierarchy-max*)))
    index))

(defun hierarchy-index (hierarchy)
  "Return the hierarchy index for the specified hierarchy. Hierarchy
must be already a number or a unique identifier suitable for comparing
using EQL. If hierarchy is a string, it will be interned in the current
package"
  (if (numberp hierarchy) hierarchy
      (%hierarchy-index hierarchy)))

(defun adjust-all-loggers-state (new-len)
  (labels ((doit (logger)
             (declare (type logger logger))
             (let ((tmp (adjust-array (logger-state logger)
                                      new-len
                                      :element-type 'logger-state)))
               (setf (svref tmp (1- new-len)) (make-logger-state)
                     (logger-state logger) tmp)
               (map-logger-children #'doit logger))))
    (doit *root-logger*))
  (values))


(defun clear-logging-configuration ()
  "Delete all loggers"
  (labels ((reset (logger)
             (setf (svref (logger-state logger) *hierarchy*)
                   (make-logger-state))
             (map-logger-children #'reset logger)))
    (reset *root-logger*))
  (values))

(defun reset-logging-configuration ()
  "Clear the logging configuration in the current hierarchy, and
configure root logger with INFO log level and a simple console
appender"
  (clear-logging-configuration)
  (add-appender *root-logger* (make-console-appender))
  (setf (logger-log-level *root-logger*) +log-level-info+))

(defun create-root-logger ()
  (let ((root (create-logger :name "")))
    (adjust-logger root)
    root))

(defvar *root-logger*
  (create-root-logger)
  "The one and only root logger")

