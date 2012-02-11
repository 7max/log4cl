(cl:in-package :log4cl)

(defmethod package-log-category ((pkg (eql *package*)) categories explitic-p)
  (if explitic-p categories
      `(log4cl self ,@categories)))

(defvar *self-log-level* +log-level-warn+
  "Log level for the self logger, that (CLEAR-LOGGING-CONFIGURATION) will use")

(defvar +self-logger+
  (let ((logger (make-logger '(:log4cl :self))))
    (setf (logger-additivity logger) nil)
    (setf (logger-log-level logger) *self-log-level*)
    logger))
