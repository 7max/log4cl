(cl:in-package :log4cl)

(defmethod package-log-category ((pkg (eql *package*)) categories explitic-p)
  "Make default logger in the log4cl package to be log4cl:self"
  (if explitic-p categories
      `(log4cl self ,@categories)))

(defvar *self-log-config* '(:sane :warn :own :two-line))

(defvar +self-logger+
  (let ((logger (make-logger '(:log4cl :self))))
    (setf (logger-additivity logger) nil)
    logger))
