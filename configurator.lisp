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

If not specified, default logger will be root logger

Valid options can be:

|-------------+---------------------------------------------------------------|
| :INFO       | Or any other keyword identifying a log level, which can be    |
| :DEBUG      | shortened to its shortest unambiguous prefix, such as :D      |
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
| :SELF       | Used for debugging LOG4CL itself. Instead of root logger,     |
|             | make default logger LOG4CL:SELF and remember all arguments    |
|             | in the variable *SELF-LOG-CONFIG*, so that they are restored  |
|             | even on (CLEAR-LOGGING-CONFIGURATION). Automatically assumes  |
|             | :OWN making the LOG4CL:SELF logger non-additive               |
|-------------+---------------------------------------------------------------|
| :PROPERTIES | Configure with PROPERTY-CONFIGURATOR by parsing specified     |
| FILE        | properties file                                               |
|-------------+---------------------------------------------------------------|
| :WATCH      | Used with :PROPERTIES, uses watcher thread to check           |
|             | properites file modification time, and reloads if it changes  |
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
        orig-args
        self appenders)
    (cond ((logger-p (car args))
           (setq logger (pop args)))
          ((consp (car args))
           (setq logger (get-logger-internal
                         (pop args)
                         (naming-option *package* :category-separator)
                         (naming-option *package* :category-case))))
          ((member :self args)
           (setq logger (make-logger '(log4cl self))
                 self t)
           (setq args (remove :self args))))
    (setq logger (or logger *root-logger*))
    (unless (setq orig-args args)
      (return-from log-config (show-logger-settings logger)))
    (loop
      (let ((arg (or (pop args) (return))))
        (case arg
          (:self ; still in the arglist means 1st arg was a logger
           (log4cl-error "Specifying a logger is incompatible with :SELF"))
          (:sane (setq sane t))
          (:clear (setq clear t))
          (:all (setq all t))
          (:own (setq own t))
          ((:twoline :two-line) (setq twoline t))
          (:console (setq console t))
          (:daily
           (setq daily (or (pop args)
                           (log4cl-error ":DAILY missing argument"))))
          (:pattern
           (setq pattern (or (pop args)
                             (log4cl-error ":PATTERN missing argument"))))
          (t (let ((lvl (handler-case (log-level-from-object arg *package*)
                          (log4cl-error ()))))
               (cond ((and level lvl)
                      (log4cl-error "Only one log level can be specified"))
                     (lvl (setq level lvl))
                     ((keywordp arg)
                      (log4cl-error "Invalid LOG-CONFIG keyword ~s" arg))
                     (t (log4cl-error
                         "Don't know what do with argument ~S" arg))))))))
    (or logger (setq logger *root-logger*))
    (or level sane clear daily
        (log4cl-error "A log level or one of :SANE :CLEAR :DAILY must be specified"))
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
            (twoline-pattern "[%d{%H:%M:%S}] [%-5P] <%c{}{}{:downcase}>%n  *%I{>} %m%n"))
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
    (when self
      ;; This is special adhoc case of configuring the LOG4CL:SELF
      (let ((config (cons :own
                          ;; we don't remember these
                          (remove-if (lambda (x)
                                       (member x '(:own :self :clear :all)))
                                     orig-args))))
        ;; if specified new appenders, simply remember new configuration
        (if (or sane daily) (setq *self-log-config* config)
            ;; otherwise merge specified config and the remembered one
            ;; This is so (log-config :self :d) would still do same
            ;; thing as (log-config :self :d :sane), if old value of
            ;; *self-log-config* contained :sane
            (let (tmp doit)
              (when (setq tmp (member :sane *self-log-config*))
                (push :sane config)
                (setq doit t))
              (when (setq tmp (member :daily *self-log-config*))
                (setq config (append config (subseq tmp 0 2)))
                (setq doit t))
              (when (and (setq tmp (member :pattern *self-log-config*))
                         (not pattern))
                (setq config (append config (subseq tmp 0 2)))
                (setq doit t))
              (when (and (setq tmp (member :twoline *self-log-config*))
                         (not pattern))
                (setq config (cons :twoline (remove :twoline config))))
              (when doit (setq *self-log-config* config))))))
    ;; finally recalculate reach-ability
    (adjust-logger logger)))

(defun show-logger-settings (logger)
  "Print logger settings and its children to *STANDARD-OUTPUT*

Example output:

+ROOT, WARN
 |
 +-#<CONSOLE-APPENDER>
 |     with #<PATTERN-LAYOUT>
 |              :pattern [%P] %c %m%n
 |     :immediate-flush: nil
 |     :flush-interval: 1
 +-LOG4CL
   |
   +-SELF (non-additive), DEBUG
   | |  :config (:SANE, :OWN :TWOLINE :D)
   | |
   | +- #<CONSOLE-APPENDER 0x123123>,
   | |     with #<PATTERN-LAYOUT>
   | |              :pattern [%P] %c %m%n
   | |     :immediate-flush: nil
   | |     :flush-interval: 1
   | +- #<DAILY-ROLLING-APPENDER 0x12345>
   |       with #<SIMPLE-LAYOUT 1234>
   |       :immediate-flush NIL
   |       :flush-interval: 1
   | 
   +-OTHER, DEBUG
"
  (flet ((interesting-logger-p (logger)
           ;; return if logger is worth mentioning, its only
           ;; interesting if its a root logger, is non-additive or has
           ;; custom log level or appenders
           (or (eq logger *root-logger*)
               (logger-appenders logger)
               (not (logger-additivity logger))
               (logger-log-level logger))))
    (logger-name logger)))

