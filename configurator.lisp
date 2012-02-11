(in-package :log4cl)

(defun clear-logging-configuration ()
  "Delete all loggers configuration, leaving only LOG4CL.SELF"
  (labels ((reset (logger)
             (remove-all-appenders logger)
             (setf (svref (logger-state logger) *hierarchy*)
                   (make-logger-state))
             (map-logger-children #'reset logger)))
    (reset *root-logger*)
    (set-additivity +self-logger+ nil)
    (add-appender +self-logger+ (make-instance 'console-appender))
    (setf (logger-log-level +self-logger+) *self-log-level*))
  (values))

(defun reset-logging-configuration ()
  "Clear the logging configuration in the current hierarchy, and
configure root logger with INFO log level and a simple console
appender"
  (clear-logging-configuration)
  (add-appender *root-logger* (make-instance 'console-appender))
  (setf (logger-log-level *root-logger*) +log-level-warn+)
  (log-info "Logging configuration was reset to sane defaults"))

(defun log-config (&rest args)
  "User friendly way of configuring logger hierachy. 

  Synopsis:
 
  ;; set log level of a root logger
  (log-config :info)

  ;; set root logger to :info and unset per-logger level of all
  ;; children
  (log-config :reset)

  ;; do both at the same time
  (log-config :info :reset)

  ;; NOTE :reset has to be the last option in the list
  
  ;; set a logger named cl-user.mylogger to debug
  (log-config :mylogger :debug)
   
  ;; set a logger named cl-user.somefunction to debug
  (log-config somefunction :debug)
  
  ;; set a logger named full.logger.name to debug
  (log-config \"full.logge.name\"  debug)

  ;; set all loggers matching glob
  (log-config :.*suffix :debug)

  ;; set all loggers matching glob
  (log-config :.*(get|set)-.*line :debug)
" 
  (let (do-reset loggers 
         (num-updated 0)
         (num-reset 0)
         log-level
         had-log-level)
    (declare (boolean do-reset)
             (fixnum num-updated num-reset))
    (setf args (reverse args))
    (when (eq :reset (car args))
      (pop args)
      (setf do-reset t))
    (when (and args  
               (typep (first args) '(or string symbol)) 
               (not (search "*" (string (first args)))))
      (setf had-log-level t)
      (setf log-level (make-log-level (pop args))))
    (dolist (arg args)
      (cond 
        ((or (stringp arg) (symbolp arg))
         (push (loggers-from-string (string arg))
               loggers))
        ((logger-p arg)
         (push arg loggers))
        (t (error "~s must be either symbol, string or a logger" arg))))
    (when (null loggers)
      (push *root-logger* loggers)
      (or had-log-level
          (setf log-level (make-log-level :error))))
    (labels ((doit (logger)
               (cond ((null logger))
                     ((consp logger)
                      (doit (car logger))
                      (doit (cdr logger)))
                     (t 
                      ;; reset children first without adjusting
                      (when do-reset
                        (labels ((doit (logger)
                                   (when (set-log-level logger +log-level-unset+ nil)
                                     (incf num-reset))
                                   (map-logger-children #'doit logger)))
                          (map-logger-children #'doit logger)))
                      ;; adjusts logger and descendants
                      (when (set-log-level logger log-level)
                        (incf num-updated))))))
      (doit loggers)
      (values num-updated num-reset))))


(defun loggers-from-string (regexp)
  (let ((regexp (create-scanner regexp :case-insensitive-mode t))
        loggers)
    (labels ((doit (logger)
               (when (scan regexp (logger-category logger))
                 (push logger loggers))
               (map-logger-children #'doit logger)))
      (doit *root-logger*))
    (nreverse loggers)))

