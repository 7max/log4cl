(in-package :log4cl)

(defun clear-logging-configuration ()
  "Delete all loggers configuration, leaving only LOG4CL.SELF"
  (labels ((reset (logger)
             (remove-all-appenders logger)
             (setf (svref (logger-state logger) *hierarchy*)
                   (make-logger-state))
             (map-logger-children #'reset logger)))
    (reset *root-logger*)
    (when *self-log-config*
      (apply 'log-config +self-logger+ *self-log-config*)))
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
  "User friendly way of configuring loggers. General syntax is:

    (LOG-CONFIG [LOGGER-IDENTIFIER] OPTION1 OPTION2...)

Logger can be one of:

- Logger instance ie result of (make-logger) expansion, or any form
  that returns a logger.

- A list of logger categories, basically a shortcut for (MAKE-LOGGER
  '(CAT1 CAT2 CAT3))

Valid options can be:

|-------------+---------------------------------------------------------------|
| :INFO       | Or any other keyword identifying a log level, which can be    |
| :DEBUG      | shortened to its shortestnunambiguous prefix, such as :D      |
|-------------+---------------------------------------------------------------|
| :CLEAR      | Removes log level and appenders from any child loggers,       |
|             | appenders are not removed from non-additive loggers           |
|-------------+---------------------------------------------------------------|
| :ALL        | Changes :CLEAR to remove appenders from non-additive          |
|             | loggers                                                       |
|-------------+---------------------------------------------------------------|
| :SANE       | Removes logger appenders, adds console appender with          |
|             | pattern layout that makes messages look like this:            |
|             |                                                               |
|             | :     [11:22:25] INFO  {category.name} - message              |
|-------------+---------------------------------------------------------------|
| :OWN        | For :SANE and :DAILY makes logger non-additive                |
|             | otherwise additive flag will be set                           |
|-------------+---------------------------------------------------------------|
| :DAILY FILE | Adds file appender logging to the named file, which will      |
|             | be rolled over every midnight into FILE.YYYYMMDD; Removes any |
|             | other other appenders that subclass FILE-APPENDER-BASE from   |
|             | the logger.  If :SANE is also specified, all logger appenders |
|             | are removed, but console appender is not added                |
|-------------+---------------------------------------------------------------|
| :CONSOLE    | Forces adding of console appender if both :SANE and :DAILY    |
|             | were specified                                                |
|-------------+---------------------------------------------------------------|
| :PATTERN    | For :SANE option uses specified conversion pattern instead    |
| STRING      | of default one                                                |
|-------------+---------------------------------------------------------------|
| :TWOLINE    | Changes default pattern layout to print user log message      |
|             | log message on 2nd line after the headers                     |
|-------------+---------------------------------------------------------------|

Examples:

  - (LOG-CONFIG :D) :: Changes root logger level to debug

  - (LOG-CONFIG :SANE) :: Changes root logger level to info, removes its
    appenders, adds console appender with pattern layout

  - (LOG-CONFIG :SANE :WARN :CLEAR :ALL) :: Changes root logger level to
    warnings, removes its appenders, adds console appender with
    pattern layout; then resets all child loggers log levels, and
    removes their appenders.

  - (LOG-CONFIG (MAKE-LOGGER :FOOBAR) :SANE :OWN :D :DAILY
    \"debug.log\") :: Configures the specified logger with debug log
    level, logging into file debug.log which will be rolled over
    daily, and makes it non-additive ie any messages will not be
    propagated to logger parents.
" 
  (let ((logger nil)
        sane clear all own daily pattern 
        twoline level layout console
        appenders)
    (cond ((logger-p (car args))
           (setq logger (pop args)))
          ((consp (car args))
           (setq logger (get-logger-internal
                         (pop args)
                         (naming-option *package* :category-separator)
                         (naming-option *package* :category-case))))
          (t (setq logger *root-logger*)))
    (loop
      (let ((arg (or (pop args) (return))))
        (case arg
          (:sane (setq sane t))
          (:clear (setq clear t))
          (:all (setq all t))
          (:own (setq own t))
          (:twoline (setq twoline t))
          (:console (setq console t))
          (:daily
           (setq daily (or (pop args) (error ":DAILY missing argument"))))
          (:pattern
           (setq pattern (or (pop args) (error ":PATTERN missing argument"))))
          (t (if level (error "Only one log level can be specified")
                 (setq level (log-level-from-object arg *package*)))))))
    (or level sane clear daily
        (error "A log level or one of :SANE :CLEAR :DAILY must be specified"))
    (when (or level sane)
      (set-log-level logger (or level +log-level-info+) nil))
    (when clear
      (map-logger-children (lambda (l)
                             (set-log-level l +log-level-unset+ nil)
                             (unless (and (logger-additivity l) (not all))
                               (remove-all-appenders-internal l nil)))
                           logger))
    (when (or daily sane)
      (let ((default-pattern "[%d{%H:%M:%S}] [%P] <%c{}{}{:downcase}> - %m%n")
            (twoline-pattern "[%d{%H:%M:%S}] [%P] <%c{}{}{:downcase}>%n  *%I{>} %m%n"))
        (setq layout (make-instance 'pattern-layout
                      :conversion-pattern
                      (or pattern
                          (if twoline twoline-pattern
                              default-pattern)))))
      (cond
        (daily (if sane (remove-all-appenders-internal logger nil)
                   (dolist (a (logger-appenders logger))
                     (when (typep a 'file-appender-base)
                       (remove-appender-internal logger a nil))))
               (push (make-instance 'daily-file-appender
                      :name-format daily
                      :backup-name-format (format nil "~a.%Y%m%d" daily)
                      :layout layout)
                     appenders))
        (t (remove-all-appenders-internal logger nil)))
      
      (when (and sane (or (not daily) console))
        (push (make-instance 'console-appender :layout layout)
              appenders))
      (dolist (a appenders)
        (add-appender-internal logger a nil))
      (set-additivity logger (not own) nil))
    ;; finally recalculate reach-ability
    (adjust-logger logger)))



(defun loggers-from-string (regexp)
  (let ((regexp (create-scanner regexp :case-insensitive-mode t))
        loggers)
    (labels ((doit (logger)
               (when (scan regexp (logger-category logger))
                 (push logger loggers))
               (map-logger-children #'doit logger)))
      (doit *root-logger*))
    (nreverse loggers)))


