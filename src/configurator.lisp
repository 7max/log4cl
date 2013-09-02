;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2012, Max Mikhanosha. All rights reserved.
;;;
;;; This file is licensed to You under the Apache License, Version 2.0
;;; (the "License"); you may not use this file except in compliance
;;; with the License.  You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;;
;;; Contains (log-config) function and default logging initialization
;;; 
(in-package #:log4cl)

(defun log-config (&rest args)
  "Very DWIM oriented friendly way of configuring loggers and appenders.

    (LOG:CONFIG [LOGGER-IDENTIFIER] OPTION1 OPTION2...)

LOGGER-IDENTIFIER is optional and defaults to the root logger. It can be
one of the following:

- Logger instance ie result of (LOG:CATEGORY) expansion, or any other form
  that returns a logger.

- A list of logger categories, basically a shortcut for (LOG:CATEGORY
  '(CAT1 CAT2 CAT3)). An error will be given if logger does not exist. If you want
  to ensure logger is created, even if it did not exist before, use
  (LOG:CONFIG (LOG:CATEGORY ...) ...)

Without any options (LOG:CONFIG) displays current configuration

---------------|---------------------------------------------------------------
  Option       | Description
---------------|---------------------------------------------------------------
                               MAIN OPTIONS.
---------------|---------------------------------------------------------------
 :INFO         | Or any other keyword identifying a log level, which can be    
 :DEBUG        | shortened to its shortest unambiguous prefix, such as :D.
               | Changes the logger level to that level.
---------------|---------------------------------------------------------------
 :SANE         | Removes logger appenders, then arranges for the logging output
               | to be always logged to dynamic value of *TERMINAL-IO*
               |                                                               
               | If used with :DAILY then console appender is not added, unless
               | :CONSOLE, :THIS-CONSOLE or :TRICKY-CONSOLE is also specified.
               |
               | The pattern layout added is affected by many of the pattern
               | options below.
---------------|---------------------------------------------------------------
                           APPENDER OPTIONS
---------------|---------------------------------------------------------------
 :CONSOLE      | Adds CONSOLE-APPENDER to the logger. Console appender logs
               | into current console as indicated by *TERMINAL-IO* stream
---------------|---------------------------------------------------------------
 :THIS-CONSOLE | Adds THIS-CONSOLE-APPENDER to the logger. It captures the
  (or :THIS)   | current value of *TERMINAL-IO* by recursively resolving any
               | synonym or two-way streams, and continues to log the
               | remembered value, even if it goes out dynamic scope.
               |
               | On any stream errors it will auto-remove itself.
---------------|---------------------------------------------------------------
:TRICKY-CONSOLE| Adds TRICKY-CONSOLE-APPENDER which acts exactly like 
 (or :TRICKY)  | THIS-CONSOLE appender above, but it checks if dynamic value
               | of *TERMINAL-IO* resolves to the same stream and ignores the
               | log message if it does.
               |
               | When debugging multi-threaded code from REPL, this results in
               | REPL thread logging to REPL, while threads with some other
               | value of *TERMINAL-IO* stream will output to both.
               |
               | As a shortcut, if THIS-CONSOLE is specified and global console
               | appender already exists, it will add TRICKY-CONSOLE instead.
---------------|---------------------------------------------------------------
 :SANE2        | Shortcut for :SANE :TRICKY (mnemonic two -> multiple threads)
---------------|---------------------------------------------------------------
 :STREAM stream| Changes the stream used for above two dedicated stream
               | appenders.
---------------|---------------------------------------------------------------
 :DAILY FILE   | Adds file appender logging to the named file, which will be
               | re-opened every day, with old log file renamed to FILE.%Y%m%d.
               |
               | Removes any other file based appenders from the logger.
               |
               | FILE can also contain %Y%m%d like pattern, expansion of which
               | will determine when new log file would be opened.
---------------|---------------------------------------------------------------
 :FILTER level | Makes all appenders added by above keywords drop messages
               | below specified level.
---------------|---------------------------------------------------------------

                            LAYOUT OPTIONS

  General note about layout options, if any appender options are specified
  the layout options only affect PATTERN-LAYOUT for new appenders created
  by LOG:CONFIG command

  But if no appender options are specified, but layout options are, LOG:CONFIG
  will change the pattern layout on all console based appenders that output
  current *TERMINAL-IO*
---------------|---------------------------------------------------------------
 :PATTERN      | For any new appenders added, specifies the conversion pattern
               | for the PATTERN-LAYOUT. If not given, default pattern will be
               | used, modified by below options
---------------|---------------------------------------------------------------
 :PRETTY       | Add {pretty} option to the pattern to force pretty printing
 :NOPRETTY     | Add {nopretty} option to the pattern to force no pretty printing
               | without one of these, global value is in effect
---------------|---------------------------------------------------------------
 :TIME/:NOTIME | Include time into default pattern, default is :TIME
---------------|---------------------------------------------------------------
 :FILE or      | Include file name into default pattern, :FILE2 uses alternate
 :FILE2        | position for the file (in front of the package).
 :NOFILE       | :NOFILE does not show the file
---------------|---------------------------------------------------------------
 :THREAD       | Include thread name into default pattern, it will be after the
  [<n>[<n2>]]  | time, in [%t] format. If :THERAD argument is followed by one
               | or two numbers, they will be used as min/max field width.
---------------|---------------------------------------------------------------
 :NDC          | Include NDC context into default pattern, with optional min/max
  [<n>[<n2>]]  | field width flags. When used together with :THREAD NDC will be
               | printed after the thread name, separated by dash, like this
               | example: \"[threadname-ndc]\"; it will not be shown if unbound
               |
               | Without :THREAD, its shown in its own square brackets, with
               | entire construct not shown if unbound.
---------------|---------------------------------------------------------------
 :NOPACKAGE    | Add {nopackage} option to the pattern (binds orig package at
 :PACKAGE      | the site of the log statement. PACKAGE binds keyword package,
               | so everything is printed with package prefix
---------------|---------------------------------------------------------------
  :TWOLINE     | Changes pattern layout to print hard newline before actual log
 (or :2LINE)   | message. Only makes sense with NOPRETTY or when logging into
               | files.
               | 
               | Pretty printing does better job at line splitting then forced
               | two line layout, with short log statements placed on a single
               | line and longer ones wrapping.
---------------|---------------------------------------------------------------
                   ASSORTED OTHER OPTIONS
---------------|---------------------------------------------------------------
 :PROPERTIES   | Configure with PROPERTY-CONFIGURATOR by parsing specified     
 FILE          | properties file                                               
---------------|---------------------------------------------------------------
 :WATCH        | Used with :PROPERTIES, uses watcher thread to check           
               | properties file modification time, and reloads if it changes  
---------------|---------------------------------------------------------------
 :IMMEDIATE-   | Used with :SANE, :DAILY or :CONSOLE to create new appenders
  FLUSH        | with :IMMEDIATE-FLUSH T option, which prevents automatic
               | startup of hierarchy watcher thread, which is used for
               | auto-flushing. 
---------------|---------------------------------------------------------------
 :NOADDITIVE   | Makes logger non-additive (does not propagate to parent)
 :OWN          | Shortcut for non-additive (usually has appenders on its own)
 :ADDITIVE     | Sets additive flag back to T.
---------------|---------------------------------------------------------------
 :CLEAR        | Reset the child in the hierarchy, by unsetting their log level
               | and/or removing appenders from them. Without any other flags
               | unset the log levels.
               |
 :LEVELS       | Clears the log levels, this is the default
 :APPENDERS    | Removes any appenders, levels will not be cleared unless
               | :LEVELS is also specified
               |
 :ALL          | Normally :CLEAR does not touch non-additive loggers, that is
               | the ones that don't pass messages to parents. This flag forces
               | clearing of non-additive loggers 
               |
               | Note that this option does not change the logger being acted
               | upon, just its children. See next option
---------------|---------------------------------------------------------------
 :REMOVE <num> | Removes specific appender from the logger. Numbers are 1-based,
               | and are same ones displayed by LOG:CONFIG without arguments
---------------|---------------------------------------------------------------
 :SELF         | Configures the LOG4CL logger, which can be used to debug
               | Log4CL itself. Normally all other LOG:CONFIG hides the
               | it from view.
---------------|---------------------------------------------------------------
 :FORCE-ADD    | Normally if you specify :CONSOLE :THIS-CONSOLE or
               | :TRICKY-CONSOLE without :SANE (which clears out existing
               | appenders), an error will be given if there are any standard
               | console appenders that already log to *TERMINAL-IO* or :STREAM
               | argument.
               |
               | This is to prevent from creating duplicate output.
               |
               | Adding :FORCE-ADD flag skips the above check, and allows you
               | to add new console appender regardless.
---------------|---------------------------------------------------------------
 :BACKUP       | Used together with :DAILY, specifies the :BACKUP-NAME-FORMAT,
               | see docstring for the DAILY-FILE-APPENDER class.
               |
               | For example specifying a DAILY <file> :BACKUP NIL will always
               | log to statically named FILE without rolling.
               | 
               | Defaults to NIL if FILE contains percent character or
               | FILE.%Y%m%d otherwise.
---------------|---------------------------------------------------------------

Examples:

* (LOG:CONFIG :D) -- Changes root logger level to debug

* (LOG:CONFIG :SANE) -- Drops all console appenders, change level
  to INFO, makes all output appear on current console

* (LOG:CONFIG :SANE2) -- As above, but copy all other threads output
  to current terminal.

* (LOG:CONFIG :PRETTY :THREAD) - changes active console appenders
  layout to force pretty printing and add thread info.

* (LOG:CONFIG :NOPRETTY :PACKAGE) - changes layout to force no
  pretty printing, and restoring original *PACKAGE* when printing
  
* (LOG:CONFIG :WARN :CLEAR) -- Changes root logger level to warnings,
  and unset child logger levels.

* (LOG:CONFIG '(PACKAGE) :OWN :DEBUG :DAILY \"package-log.%Y%m%d\")

  Configures the logger PACKAGE with debug log level, logging into
  the file \"package-log.20130510\" which will be rolled over daily;
  makes logger non-additive so messages will not be propagated to
  logger parents. (To see them on console, remove the :OWN flag, or
  add :CONSOLE to the command)

  Note: in above example if package name is dotted, you need to
  specify the split category list, so if your package name is
  COM.EXAMPLE.FOO logger categories will be '(COM EXAMPLE FOO)
"
 
  (let ((logger nil)
        sane clear all own noown daily pattern
        backup had-backup
        file file2 nofile
        time notime
        level layout console
        oneline twoline
        orig-args
        self appenders
        filter
        immediate-flush
        properties watch
        this-console tricky-console global-console
        pretty nopretty
        package nopackage
        thread ndc
        stream
        pattern-specified-p
        appender-specified-p
        force-add
        clear-levels clear-appenders
        remove
        stream-by-itself
        logger-specified-how)
    (declare (type (or null stream) stream))
    (cond ((logger-p (car args))
           (setq logger (pop args)
                 logger-specified-how 'logger))
          ((consp (car args))
           (setq logger
                 (let ((cats (pop args))) 
                   (setq logger-specified-how 'cats)
                   (or (instantiate-logger *package* cats t nil) 
                       (log4cl-error "~@<Logger named ~S not found. If you want to create it, ~
                       use (~A:~A (~A:~A ~S) ...) instead~:@>"
                                     cats
                                     '#:log '#:config
                                     '#:log '#:logger
                                     cats)))))
          ((member :self args)
           (setq logger +self-logger+ self t
                 logger-specified-how 'self 
                 args (remove :self args))))
    (or logger (setq logger *root-logger*
                     logger-specified-how nil))
    (unless (setq orig-args args)
      (return-from log-config (show-logger-settings logger)))
    (loop
      (let ((arg (or (pop args) (return))))
        (case arg
          (:self     ; still in the arglist means 1st arg was a logger
           (log4cl-error "Specifying a logger is incompatible with :SELF"))
          (:sane (setq sane t global-console t))
          (:sane2 (setq sane t global-console t tricky-console t))
          ((:clear :reset) (setq clear t))
          (:levels (setq clear-levels t))
          (:appenders (setq clear-appenders t))
          (:all (setq all t))
          ((:own :nonadditive :noadditive) (setq own t noown nil))
          (:additive (setq own nil noown t))
          (:file (setq file t file2 nil nofile nil))
          (:file2 (setq file nil file2 t nofile nil))
          (:nofile (setq file nil file2 nil nofile t))
          (:time (setq time t notime nil))
          (:notime (setq time nil notime t))
          (:immediate-flush (setq immediate-flush t))
          ((:twoline :two-line :2line) (setq oneline nil twoline t))
          ((:oneline :one-line :1line) (setq oneline t twoline nil))
          (:console (setq console t global-console t))
          (:thread
           (setq thread 
                 (let* ((min (when (integerp (car args)) (pop args)))
                        (max (when (integerp (car args)) (pop args))))
                   (declare (type (or null integer) min) 
                            (type (or null (integer 1)) max))
                   (if (or min max) (list min max) t))))
          (:ndc
           (setq ndc 
                 (let* ((min (when (integerp (car args)) (pop args)))
                        (max (when (integerp (car args)) (pop args))))
                   (declare (type (or null integer) min) 
                            (type (or null (integer 1)) max))
                   (if (or min max) (list min max) t))))
          (:pretty (setq pretty t nopretty nil))
          (:nopretty (setq nopretty t pretty nil))
          (:package (setq package t nopackage nil))
          (:nopackage (setq nopackage t package nil))
          ((:this-console :this)
           (setq console t
                 this-console t
                 tricky-console nil
                 global-console nil))
          ((:tricky-console :tricky)
           (setq console t
                 tricky-console t
                 this-console nil))
          (:force-add (setq force-add t))
          (:watch (setq watch t))
          (:backup
           (setq backup (if args (pop args)
                            (log4cl-error ":BACKUP missing argument"))
                 had-backup t))
          (:daily
           (setq daily (or (pop args)
                           (log4cl-error ":DAILY missing argument"))))
          (:remove
           (setq remove (or (pop args)
                            (log4cl-error ":REMOVE missing argument"))))
          (:properties
           (setq properties (or (pop args)
                                (log4cl-error ":PROPERTIES missing argument"))))
          (:stream
           (setq stream (or (pop args)
                            (log4cl-error ":STREAM missing argument"))
                 console t))
          (:pattern
           (setq pattern (or (pop args)
                             (log4cl-error ":PATTERN missing argument"))))
          (:filter
           (setq filter (or (pop args)
                            (log4cl-error ":FILTER missing argument")))
           (when filter
             (setq filter (make-log-level filter))))
          (t (let ((lvl (handler-case (log-level-from-object arg *package*)
                          (log4cl-error ()))))
               (cond ((and level lvl)
                      (log4cl-error "Only one log level can be specified"))
                     (lvl (setq level lvl))
                     ((keywordp arg)
                      (log4cl-error "Invalid LOG:CONFIG keyword ~s" arg))
                     (t (log4cl-error
                         "Don't know what do with argument ~S" arg))))))))
    (when stream 
      (if (and (not this-console) (not tricky-console))
          (setq tricky-console t))
      (when (and (not sane) (not daily))
        (setq stream-by-itself t)))
    (setq
     pattern-specified-p (or pattern oneline twoline thread
                             ndc file file2 nofile time notime
                             pretty nopretty
                             package nopackage)
     appender-specified-p (or daily sane console))

    (or level remove sane clear daily properties own noown console pattern-specified-p
        (log4cl-error "Bad combination of options"))
    (or (not properties)
        (not (or sane daily console level pattern-specified-p))
        (log4cl-error ":PROPERTIES can't be used with other options"))
    (or (not remove)
        (not (or sane daily console level pattern-specified-p))
        (log4cl-error ":REMOVE can't be used with other options"))
    (if (and pattern
             (or twoline oneline file file2 nofile time notime
                 thread ndc pretty nopretty
                 package nopackage))
        (error ":PATTERN isn't compatible with built-in pattern selection flags"))
    ;; If we are asked to screw with appenders, and given an source-file logger
    ;; object, do it on the parent.. Reason is that without automatic naming
    ;; support, or at toplevel (log:category) will return not package logger,
    ;; but package.<source> one
    (when (and (eq logger-specified-how 'logger)
               (typep logger 'source-file-logger)
               (or pattern-specified-p appender-specified-p remove))
      (setq logger (logger-parent logger))
      (assert (not (typep logger 'source-file-logger))))
    (when remove
      (let ((list (logger-appenders logger)))
        (if (<= 1 remove (length list))
            (remove-appender logger (nth (1- remove) list))
            (log4cl-error "Bad appender number ~d" remove)))
      (return-from log-config))
    (when (or appender-specified-p pattern-specified-p)
      (setq layout (make-instance 'pattern-layout
                    :conversion-pattern
                    (or pattern (figure-out-pattern
                                 :oneline (if (or oneline twoline) oneline t)
                                 :twoline (if (or oneline twoline) twoline nil)
                                 :time (if (or time notime) time t)
                                 :file (if (or file file2 nofile) file t)
                                 :file2 (if (or file file2 nofile) file2 nil)
                                 :pretty pretty
                                 :nopretty nopretty
                                 :package package
                                 :nopackage nopackage
                                 :thread thread
                                 :ndc ndc)))))
    (when (and own (eq logger *root-logger*))
      (error "Can't set root logger non-additive"))
    (when level
      (set-log-level logger level nil))
    (when clear
      (labels ((map-descendants-ignoring-self (function logger) 
                 (let ((child-hash (%logger-child-hash logger)))
                   (when child-hash
                     (maphash (lambda (name logger)
                                (declare (ignore name))
                                (when (or self
                                          (not (eq logger +self-logger+))) 
                                  (funcall function logger) 
                                  (unless (typep logger 'source-file-logger) 
                                    (map-descendants-ignoring-self function logger))))
                              child-hash))))) 
        (map-descendants-ignoring-self
         (lambda (l)
           (when (or (logger-additivity l) all)
             (when (or clear-levels (not clear-appenders)) 
               (set-log-level l +log-level-unset+ nil))
             (when clear-appenders 
               (unless (eq l +self-meta-logger+) 
                 (remove-all-appenders-internal l nil)))))
         logger)))
    ;; kind of separate from appender stuff
    (when (or own noown)
      (set-additivity logger (not own) nil))
    (when (or daily sane console) 
      (if sane
          ;; Only remove all appenders if :clear was also given,
          ;; otherwise don't touch file appenders
          (if (and clear clear-appenders)
              (remove-all-appenders-internal logger nil) 
              (dolist (a (logger-appenders logger))
                (unless (typep a 'file-appender-base)
                  (remove-appender-internal logger a nil)))))
      ;; create daily appender
      (when daily
        (dolist (a (logger-appenders logger))
          (when (typep a 'file-appender-base)
            (remove-appender-internal logger a nil)))
        (push (make-instance 'daily-file-appender
               :name-format daily
               :backup-name-format
               (if had-backup backup
                   (unless (position #\% (if (pathnamep daily) (format nil "~a" daily)
                                             daily))
                     (format nil "~a.%Y%m%d" daily)))
               :filter filter
               :layout layout)
              appenders))
      ;; Add new console appender, only in these situations
      ;; 
      ;; a) :console or :this-console is explicitly used
      ;; b) :sane is specified and :daily not
      (when (or (and sane (not daily))
                console)
        (let* (have-this-console-p
               have-global-console-p
               have-tricky-console-p
               (stream (resolve-stream (or stream *global-console*)))
               (global-stream (resolve-stream *global-console*))
               (global-stream-same-p (eq stream global-stream))
               dups
               s1 s2 s3)
          (dolist (a (logger-appenders logger))
            (when (appender-enabled-p a)
              (typecase a
                (console-appender (setq have-global-console-p t)
                 (push a dups))
                (tricky-console-appender
                 (when (eq stream (appender-stream a))
                   (setq have-tricky-console-p t)
                   (push a dups)))
                (this-console-appender
                 (when (eq stream (appender-stream a))
                   (setq have-this-console-p t)
                   (push a dups))))))
          (when (not force-add)
            ;; if user tries to add this-console, and we have global
            ;; one add tricky one instead
            (when (and (or have-global-console-p global-console)
                       this-console
                       global-stream-same-p)
              (setq this-console nil
                    tricky-console t))
            ;; duplicate check
            (when (or (and global-console
                           (or have-global-console-p
                               (and have-this-console-p
                                    global-stream-same-p))
                           (setq s1 t))
                      (and this-console
                           (or (and have-global-console-p global-stream-same-p)
                               have-this-console-p)
                           (setq s2 t))
                      (and tricky-console
                           (or have-tricky-console-p
                               have-this-console-p)
                           (setq s3 t)))
              ;; Allow :stream *somewhere* :thread
              ;; to change pattern just on that stream
              (if (and stream-by-itself
                       pattern-specified-p
                       s3 (not s1) (not s2)) 
                  (setq appender-specified-p nil)
                  (error "~@<Already logging to the same stream with ~4I~_~S ~%Specify either :SANE or :FORCE-ADD~:>"
                         dups))))
          (unless s3 
            (when global-console
              (push (make-instance 'console-appender
                     :filter filter
                     :layout layout)
                    appenders))
            (when (or tricky-console this-console) 
              (push
               (if tricky-console
                   (make-instance 'tricky-console-appender
                    :stream stream
                    :filter filter
                    :layout layout)
                   (make-instance 'this-console-appender
                    :stream stream
                    :filter filter
                    :layout layout))
               appenders)))))
      ;; now add all of them to the logger
      (dolist (a appenders)
        (when immediate-flush
          (setf (slot-value a 'immediate-flush) t))
        (add-appender-internal logger a nil)))
    (when properties
      (configure (make-instance 'property-configurator) properties
                 :auto-reload watch))
    ;; When pattern options are specified, but not the option to add
    ;; new appender, we assume user wants to change layout for
    ;; existing console appenders
    (when (and pattern-specified-p (or (not appender-specified-p)
                                       stream-by-itself))
      (let* (didit
             (stream (resolve-stream (or stream *global-console*)))
             (global-stream (resolve-stream *global-console*))
             (global-stream-same-p (eq stream global-stream)))
        (dolist (a (logger-appenders logger))
          ;; Only change global console appenders, or specific console
          ;; appender that log to console at log site
          (when (appender-enabled-p a) 
            (when (or (and global-stream-same-p (typep a 'console-appender))
                      (and (typep a '(or this-console-appender
                                      tricky-console-appender))
                           (eq (appender-stream a) stream)))
              ;; Assumes slot assignments atomic, but so is everything
              ;; else in log4cl
              (setf (appender-layout a) layout)
              (setq didit t))))
        (unless didit
          (error "Did not find any appenders to change."))))
    (when self
      ;; This is special adhoc case of configuring the LOG4CL-iMPL:SELF.  We need
      ;; special processing, because we want self-logging to survive
      ;; the (clear-logging-configuration), which is done doing tests
      (let ((config
              (cons
               :own
               ;; we don't remember these
               (remove-if (lambda (x)
                            (member x '(:own :self :clear :all :twoline :two-line
                                        :2line :oneline :one-line :1line
                                        :file :file2 :nofile
                                        :time :notime
                                        :pretty :nopretty
                                        :package :nopackage
                                        :thread :ndc)))
                          orig-args))))
        ;; If anything non-default was specified, remember
        (let ((lst '(:sane :daily :console :this-console :tricky-console))) 
          (if (null (intersection lst config))
              (dolist (elem (reverse lst))
                (when (member elem *self-log-config*)
                  (push elem config)))))
        (and twoline (push :twoline config)) 
        (and file (push :file config)) 
        (and file2 (push :file2 config)) 
        (and nofile (push :nofile config)) 
        (and notime (push :notime config))
        (and time (push :time config))
        (and pretty (push :pretty config))
        (and nopretty (push :nopretty config))
        (and package (push :package config))
        (and nopackage (push :nopackage config))
        (and thread (push :thread config))
        (and ndc (push :ndc config))
        (when (and (null level)
                   (logger-log-level logger))
          (push (aref +log-level-to-keyword+
                      (logger-log-level logger))
                config))
        (setq *self-log-config* config)))
    ;; finally recalculate reach-ability
    (adjust-logger logger)))

(defun clear-logging-configuration ()
  "Delete all loggers configuration, leaving only LOG4CL-IMPL"
  (labels ((reset (logger)
             (remove-all-appenders logger)
             (setf (svref (%logger-state logger) *hierarchy*)
                   (make-logger-state))
             (map-logger-children #'reset logger)))
    (reset *root-logger*)
    (when *self-log-config*
      (apply 'log-config +self-logger+ *self-log-config*))
    (add-appender +self-meta-logger+ (make-instance 'console-appender
                                      :layout (make-instance 'simple-layout)
                                      :immediate-flush t))
    (log-config +self-meta-logger+ :own))
  (values))

(defun reset-logging-configuration ()
  "Clear the logging configuration in the current hierarchy, and
configure root logger with INFO log level and a simple console
appender"
  (clear-logging-configuration)
  (add-appender *root-logger* (make-instance 'console-appender))
  (setf (logger-log-level *root-logger*) +log-level-warn+)
  (log-info "Logging configuration was reset to sane defaults"))


(defparameter *default-patterns*
  '((:oneline t :time t :file t :pattern
     "%&%<%I%;<;;>;-5p [%D{%H:%M:%S}]%t %g{}{}{:downcase}%:; ;F (%C{}{ }{:downcase})%2.2N - %:_%m%>%n")
    (:oneline t :time t :file2 t :pattern
     "%&%<%I%;<;;>;-5p [%D{%H:%M:%S}]%t %:;;; / ;F%g{}{}{:downcase}::(%C{}{ }{:downcase})%2.2N - %:_%m%>%n")
    (:oneline t :time nil :file t :pattern
     "%&%<%I%;<;;>;-5p%t %g{}{}{:downcase}%:; ;F (%C{}{ }{:downcase})%2.2N - %:_%m%>%n")
    (:oneline t :time nil :file2 t :pattern
     "%&%<%I%;<;;>;-5p%t %:;;; / ;F%g{}{}{:downcase}::(%C{}{ }{:downcase})%2.2N - %:_%m%>%n")
    (:oneline t :time t :pattern
     "%&%<%I%;<;;>;-5p [%D{%H:%M:%S}]%t %g{}{}{:downcase} (%C{}{ }{:downcase})%2.2N - %:_%m%>%n")
    (:oneline t :time nil :pattern
     "%&%<%I%;<;;>;-5p%t %g{}{}{:downcase} (%C{}{ }{:downcase})%2.2N - %:_%m%>%n")
    (:twoline t :time t :file t :pattern
     "%&%<%I%;<;;>;-5p [%D{%H:%M:%S}]%t %g{}{}{:downcase}%:; ;F (%C{}{ }{:downcase})%2.2N%:n* %m%>%n")
    (:twoline t :time t :file2 t :pattern
     "%&%<%I%;<;;>;-5p [%D{%H:%M:%S}]%t %:;;; / ;F%g{}{}{:downcase}::(%C{}{ }{:downcase})%2.2N%:n* %m%>%n")
    (:twoline t :time nil :file t :pattern
     "%&%<%I%;<;;>;-5p%t %g{}{}{:downcase}%:; ;F (%C{}{ }{:downcase})%2.2N%:n* %m%>%n")
    (:twoline t :time nil :file2 t :pattern
     "%&%<%I%;<;;>;-5p%t %:;;; / ;F%g{}{}{:downcase}::(%C{}{ }{:downcase})%2.2N%:n* %m%>%n")
    (:twoline t :time t :pattern
     "%&%<%I%;<;;>;-5p [%D{%H:%M:%S}]%t %g{}{}{:downcase} (%C{}{ }{:downcase})%2.2N%:n* %m%>%n")
    (:twoline t :time nil :pattern
     "%&%<%I%;<;;>;-5p%t %g{}{}{:downcase} (%C{}{ }{:downcase})%2.2N%:n* %m%>%n")))

(defun replace-in-string (s x y)
  (let ((n (search x s)))
    (if (not n) s
      (concatenate
       'string
       (subseq s 0 n)
       y
       (subseq s (+ n (length x)))))))

(defun figure-out-pattern (&rest args)
  (let ((pat (find-if (lambda (elem)
                        (every (lambda (prop)
                                 (eql (getf args prop)
                                      (getf elem prop)))
                               '(:oneline :twoline :time :file :file2)))
                      *default-patterns*)))
    (let ((pat (getf (or pat (first *default-patterns*)) :pattern))
          w1 w2
          thread-width-fmt
          ndc-width-fmt)
      (flet (
             ;; search replace once in pattern
             (s/ (x y)
               (setq pat (replace-in-string pat x y)))
             ;; generate %<min.max> part of pattern if field width was
             ;; used with :thread or :ndc
             (figure-width (x)
               (if (consp x)
                   (destructuring-bind (&optional min max) x
                     (format nil "~@[~d~]~@[.~d~]" min max)) 
                   "")))
        (cond ((getf args :pretty)
               (s/ "%<" "%<{pretty}"))
              ((getf args :nopretty) 
               (s/ "%<" "%<{nopretty}")))
        (cond ((getf args :package)
               (s/ "%<" "%<{package}"))
              ((getf args :nopackage) 
               (s/ "%<" "%<{nopackage}")))
        (cond
          ;; figure out min/max
          ((and (setq w1 (getf args :thread))
                (setq w2 (getf args :ndc)))
           (setq thread-width-fmt (figure-width w1)
                 ndc-width-fmt (figure-width w2))
           (s/ "%t"
               (format nil "%; [;~At%:;-;~Ax]"
                       thread-width-fmt
                       ndc-width-fmt)))
          ((setq w1 (getf args :thread))
           (s/ "%t" (format nil "%:; [;;];~At"
                            (figure-width w1))))
          ((setq w1 (getf args :ndc))
           (s/ "%t" (format nil "%:; [;;];~Ax"
                            (figure-width w1))))
          (t (s/ "%t" "")))
        pat))))


(defun appender-extra-print-properties (a)
  "Return a list of (PROPNAME VALUE) extra display properties to print when
displaying an appender. Some of the properties are included only
conditionally, such as last error or error count"
  (with-slots (message-count
               error-count
               ignored-error-count
               filter
               last-error
               last-ignored-error) a 
    (append
     `((:message-count ,message-count))
     (when (plusp error-count) `((:error-count ,error-count)))
     (when (plusp ignored-error-count) `((:ignored-error-count ,ignored-error-count)))
     (when filter `((:filter ,(aref +log-level-to-keyword+ filter))))
     (when last-error `((:last-error ,last-error)))
     (when last-ignored-error `((:last-ignored-error ,last-ignored-error))))))

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
   | +-#<CONSOLE-APPENDER 0x123123>,
   | |     with #<PATTERN-LAYOUT>
   | |              :pattern [%P] %c %m%n
   | |     :immediate-flush: nil
   | |     :flush-interval: 1
   | +-#<DAILY-ROLLING-APPENDER 0x12345>
   |       with #<SIMPLE-LAYOUT 1234>
   |       :immediate-flush NIL
   |       :flush-interval: 1
   | 
   +-OTHER, DEBUG
"
  (let ((indents '())
        (interesting-cache (make-hash-table)))
    (labels ((interesting-logger-p (l)
               ;; return if logger is worth mentioning, its only
               ;; interesting if its a root logger, is non-additive or
               ;; has custom log level or appenders
               (multiple-value-bind (result present)
                   (gethash l interesting-cache)
                 (if present result
                     (setf (gethash l interesting-cache)
                           (or (eq l logger)
                               (and (not (eq l +self-logger+))
                                    (or 
                                     (logger-appenders l) 
                                     (not (logger-additivity l)) 
                                     (logger-log-level l) 
                                     (and (not (typep l 'source-file-logger))
                                          (some #'interesting-logger-p (logger-children l))))))))))
             (print-indent (&optional node-p)
               (dolist (elem (reverse indents))
                 (let* ((lastp (eq elem (car indents)))
                        (width (car elem))
                        (nodes-left (cdr elem)))
                   (loop repeat (1- width)
                         do (write-char #\Space))
                   (write-char (if (and lastp node-p)
                                   #\+
                                   ;; only continue drawing vertical line down
                                   ;; if there are more child nodes to be printed
                                   ;; at this level
                                   (if (plusp nodes-left)
                                       #\|
                                       #\Space)))))
               ;; Each time we print a child node, we decrement
               ;; number of child nodes left at this level, so that
               ;; we know when to stop drawing the vertical line linking
               ;; to more nodes of the same level
               (when node-p
                 (when indents
                   (assert (plusp (cdar indents)))
                   (decf (cdar indents)))))
             (print-one-logger (l)
               (let* ((lvl (logger-log-level l))
                      (appenders (logger-appenders l))
                      (additivity (logger-additivity l))
                      (children
                        (unless (typep l 'source-file-logger)
                          (remove-if-not #'interesting-logger-p (logger-children l))))
                      (cnt-nodes (+ (length appenders)
                                    (length children))))
                 (print-indent t)
                 (cond (indents
                        (format t "-~A" (logger-name l))
                        (push (cons 2 cnt-nodes) indents))
                       ;; start vertical line at column 0 for root node
                       (t (format t "~A" (logger-name l))
                          (push (cons 1 cnt-nodes) indents)))
                 (unless additivity
                   (write-string " non-additive"))
                 (when lvl
                   (format t ", ~A" (log-level-to-string lvl)))
                 (terpri)
                 (when appenders
                   (loop for num from 1
                         for a in appenders
                         do (print-one-appender a num)))
                 (when (some #'interesting-logger-p children)
                   (dolist (l children)
                     (when (interesting-logger-p l)
                       (print-indent) (terpri)
                       (print-one-logger l))))
                 (pop indents)))
             (print-properties (obj &optional extra-props)
               (let* ((prop-alist (property-alist obj))
                      (print-list (append
                                   (loop for (initarg slot nil) in prop-alist
                                         collect (list initarg (slot-value obj slot)))
                                   extra-props))
                      (name-width (loop for prop in print-list maximize
                                           (length (format nil "~s" (first prop)))))
                      (indent (with-output-to-string (*standard-output*)
                                (print-indent)))
                      (*print-pretty* t))
                 (loop
                   for (propname value) in print-list
                   do (pprint-logical-block (*standard-output* nil :per-line-prefix indent)
                        (pprint-indent :block 0) 
                        (write propname :case :downcase) 
                        (pprint-tab :section-relative 1 (1+ name-width))
                        (pprint-indent :block 2)
                        (pprint-newline :fill)
                        (write value))
                   do (terpri))))
             (print-one-appender (a num)
               ;; empty line for spacing
               (print-indent) (terpri)
               ;; now the appender node
               (print-indent t) (format t "-(~d)-~A" num a)
               (unless (appender-enabled-p a)
                 (write-string " disabled"))
               (terpri)
               ;; indent appender attributes and layout under the appender,
               ;; don't draw the tree for them
               (push (cons 5 0) indents)
               (print-layout (slot-value a 'layout))
               (print-properties
                a (appender-extra-print-properties a))
               (pop indents))
             (print-layout (layout)
               (print-indent)
               (format t "with ~A~%" layout)
               (push (cons 5 0) indents)
               (print-properties layout)
               (pop indents)))
      (print-one-logger logger)
      (let* ((global-stream (resolve-stream *debug-io*))
             (appenders (effective-appenders logger)))
        (unless (some (lambda (a)
                        (typecase a
                          (console-appender t)
                          ((or this-console-appender tricky-console-appender)
                           (eq (appender-stream a) global-stream))))
                      appenders)
          (format t "~%~@<Warning: No appenders can reach current ~9I~_~<~A: ~:_~A~:>~:>~%"
                  (list '*debug-io* global-stream)))))
    (values)))

;; do default configuration
(let ((done nil)) 
  (defun perform-default-init ()
    (unless done
      (setq done t)
      (clear-logging-configuration)
      (log-config :i :sane2 :immediate-flush))))

(perform-default-init)

;;;
;;; Logging configuration quick save / restore
;;; 

(defclass configuration-element ()
  ((logger :initarg :logger :type logger
           :initform (error "Required argument ~s missing" :logger)
           :reader logger-of)
   (level :initarg :level
          :type keyword
          :initform (error "Required argument ~s missing" :level)
          :reader level-of))
  (:documentation "Holds logger and its log level"))

(defclass configuration ()
  ((name :type atom :initarg :name 
         :initform (error "Required argument ~s missing" :name)
         :accessor name-of)
   (elements :initarg :elements
             :accessor elements-of
             :initform (remember-logging-configuration)))
  (:documentation "Used to remember log levels for a set of loggers"))

(defun remember-logging-configuration (&optional (logger *root-logger*))
  "Utility method to make a list of logging configuration starting from LOGGER. 
Returns a list of CONFIGURATION-ELEMENT objects"
  (flet ((make-element (logger level)
           (make-instance 'configuration-element
            :logger logger
            :level (if level (aref +log-level-to-keyword+ level) :unset))))
    (cons (make-element logger (logger-log-level logger)) 
          (loop for logger in (logger-descendants logger)
                for level = (logger-log-level logger)
                if level collect (make-element logger level)))))

(defun make-logger-configuration-load-form (logger)
  "Different version of loggers load-form, that does not
remember the file name. This allows saved logging configuration
to be restored, even if loggers had moved to a different file,
without overwriting their file with its value when configuration
was saved."
  (let ((pkg-start (logger-pkg-idx-start logger))
        (pkg-end (logger-pkg-idx-end logger))) 
    (if (typep logger 'source-file-logger) 
        ;; for source-file-loggers, that represent the entire file we
        ;; still remember the file
        `(%get-logger ',(logger-categories logger)
                      ,(%logger-category-separator logger)
                      nil nil t
                      ,(logger-file logger)
                      ,(when (plusp pkg-start) (1- pkg-start))
                      ,(when (plusp pkg-end) (1- pkg-end)) t)
        ;; but not for regular loggers
        `(%get-logger ',(logger-categories logger)
                      ,(%logger-category-separator logger)
                      nil nil t
                      nil
                      ,(when (plusp pkg-start) (1- pkg-start))
                      ,(when (plusp pkg-end) (1- pkg-end))
                      ,(typep logger 'source-file-logger)))))

(defmethod print-object ((elem configuration-element) stream)
  (with-slots (logger level) elem
    (if (not *print-readably*)
        (print-unreadable-object (elem stream :type t)
          (princ (if (%logger-parent logger) (%logger-category logger)
                     "+ROOT+") stream)
          (princ #\Space stream)
          (prin1 level stream))
        (format stream  "#.~S"
                `(make-instance 'configuration-element
                  :logger
                  ,(make-logger-configuration-load-form logger)
                  :level ,level)))))

(defmethod print-object ((cnf configuration) stream)
  (with-slots (name elements) cnf
    (if (not *print-readably*)
        (print-unreadable-object (cnf stream :type t)
          (format stream "~S (~d)"
                  name (count :unset elements :key #'level-of :test-not #'eq)))
        (format stream  "#.~S"
                `(make-instance 'configuration :name ',name :elements ',elements)))))

(defvar *configurations* nil
  "List of all LOGGER-CONFIGURATION objects")

(defvar *max-configurations* 30
  "Maximum number of configurations in *CONFIGURATIONS* list")

(defvar *save-configurations-to-file* t
  "When non-NIL SAVE will also write configurations to a
*CONFIGURATIONS-FILE*")

(defvar *configurations-file* ".log4cl-configurations.lisp-expr"
  "The file where configurations will be saved. Is merged with result
of USER-HOMEDIR-PATHNAME")

(defun same-configuration-p (c1 c2)
  "Compare two logging configurations and return T if they have
exactly same loggers and levels"
  (declare (type configuration c1 c2))
  (or (eq c1 c2)
      (not (flet ((test (e1 e2) 
                    (and (eq (logger-of e1) (logger-of e2))
                         (eq (level-of e1) (level-of e2)))))
             (set-exclusive-or (elements-of c1) (elements-of c2) :test #'test)))))

(defun save (&optional name)
  "Save current logging configuration into configuration list.

NAME -- specifies the name of this logging configuration, if NAME is not
specified, one is automatically provided as \"Saved on <timestamp>\".

If its equivalent to some other configuration, save it only if it had
a different name, otherwise lift the older equivalent configuration to
the top of the list.

When *SAVE-CONFIGURATIONS-TO-FILE* is T (default) the configuration
list list will also be saved to a file
\".log4cl-configurations.lisp-expr\" in user home directory. File name
can be customized by changing *CONFIGURATIONS-FILE* variable"
  (save-configuration
   (make-instance 'configuration
    :name (or name
              (with-output-to-string (s)
                (format-time s "Saved on %Y-%m-%d %H:%M:%S"
                             (get-universal-time) nil))))
   name))

(defun apply-logging-configuration (cnf)
  "Restores logging configuration"
  (let ((root (logger-of (first (elements-of cnf)))))
    (map-logger-descendants (lambda (logger)
                              (set-log-level logger +log-level-unset+ nil))
                            root)
    (mapc (lambda (elem)
            (set-log-level (logger-of elem) (level-of elem) nil))
          (elements-of cnf))
    (adjust-logger root)))

(defun make-autosave-configuration ()
  (make-instance 'configuration
   :name (with-output-to-string (s)
           (format-time s "Autosave on %Y-%m-%d %H:%M:%S"
                        (get-universal-time) nil))))

(defun maybe-restore-configurations ()
  "If configurations list empty and *CONFIGURATIONS-FILE* exists in
home directory restore configuration list from it"
  (when (and (null *configurations*)
             (probe-file (merge-pathnames *configurations-file*
                                          (user-homedir-pathname))))
    (setq *configurations* (read-configurations-from-file))))

(defun restore (&optional configuration from-end)
  "Restore logging configuration CONFIGURATION, which can be a name,
a CONFIGURATION instance, or a number indicating Nth (zero
based) configuration in the *CONFIGURATIONS* list. NIL is treated as
zero.

When searching for the 



Before restoring the configuration, the current logging configuration
is automatically saved under the name \"Autosave <timestamp>\", unless
an equivalent configuration is already in the list

If CONFIGURATION is NIL restores first element of *CONFIGURATIONS* that is
not equivalent to the current configuration. So successive (RESTORE) will
swap last two configurations"

  (maybe-restore-configurations)
  
  (let* ((current (make-autosave-configuration))
         (current-dup (find current *configurations* :test #'same-configuration-p))
         (cnf 
           (cond ((null configuration)
                  ;; restore the first configuration not equivalent to the current one
                  (or (find current *configurations* :test-not #'same-configuration-p
                                                     :from-end from-end)
                      (error (if *configurations*
                                 "All stored configurations are equivalent to the current one"
                                 "There are no stored configurations to restore"))))
                 ((typep configuration 'configuration)
                  configuration)
                 ((integerp configuration)
                  (or (<= 0 configuration (length *configurations*))
                      (error "Invalid configuration index ~D" configuration))
                  (or (find current *configurations* :test-not #'same-configuration-p
                                                     :start configuration
                                                     :from-end from-end)
                      (error "All configurations starting from ~D are equivalent to the current one"
                             configuration)))
                 (t (or
                     ;; first try to find the named configuration that is different from current
                     (find configuration *configurations*
                           :test (lambda (name cnf)
                                   (and (equal (name-of cnf) name)
                                        (not (same-configuration-p cnf current))))
                           :from-end from-end)
                     ;; next just use name
                     (find configuration *configurations* :key #'name-of :test #'equal
                                                          :from-end from-end)
                     (error "Logging configuration ~S not found" configuration)))))
         (same-as-current-p (same-configuration-p cnf current)))
    ;; (log-sexp save save-dup (find save *configurations* :test #'same-configuration-p))
    (cond (same-as-current-p
           ;; simply move to the top
           (setq *configurations* (remove cnf *configurations*))
           (setq *configurations* (remove current-dup *configurations*))
           (push cnf *configurations*))
          (t 
           ;; When we have equivalent configuration to the current one,
           ;; move it to the top rather then overwriting it with "Autosave" entry
           (when current-dup
             (setq *configurations* (remove current-dup *configurations*))
             (setq current current-dup))
           (setf *configurations* (remove cnf *configurations*)) 
           (push current *configurations*) 
           (push cnf *configurations*) 
           (apply-logging-configuration cnf))) 
    (trim-configuration-list '*configurations*) 
    (when *save-configurations-to-file*
      (save-configurations-to-file)) 
    (first *configurations*)))

(defun trim-configuration-list (&optional (var '*configurations*))
  "Trim the list in global variable VAR to at most
*MAX-CONFIGURATIONS* elements"
  (let ((value (symbol-value var))) 
    (set var
         (subseq value 0 (min *max-configurations* (length value))))))

(defun auto-named-p (cnf)
  "Test if logging configuration name was auto-generated"
  (let ((name (name-of cnf)))
    (and (stringp name)
         (or (eql 0 (search "Autosave on" name)) 
             (eql 0 (search "Saved on" name))))))

(defun save-configuration (cnf had-name-p)
  "Save CNF logging configuration into *CONFIGURATIONS* list. If its equivalent
to some other configuration, save it only if it had a different name, otherwise
lift the older equivalent configuration to the top of the list"
  (declare (type configuration cnf))
  (let ((old (find cnf *configurations* :test #'same-configuration-p)))
    (cond
      ;; If new configuration has user-given name, and equivalent old
      ;; configuration has same name, or its name was auto-generated,
      ;; then remove the old
      ((and old had-name-p
            (or (equal (name-of cnf) (name-of old))
                (auto-named-p old)))
       (setq *configurations* (remove old *configurations*))
       (push cnf *configurations*))
      ;; When new configuration is auto-named, and older equivalent exists,
      ;; raise the older one in the list instead
      ((and old (not had-name-p))
       (setq *configurations* (remove old *configurations*))
       (push old *configurations*))
      (t (push cnf *configurations*)))
    (trim-configuration-list '*configurations*)
    (when *save-configurations-to-file*
      (save-configurations-to-file))
    (first *configurations*)))

(defun save-configurations-to-file (&optional (file *configurations-file*))
  (let ((file (merge-pathnames file (user-homedir-pathname)))
        (*package* (find-package :cl)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write *configurations* :stream out :length nil :readably t :circle nil :pretty nil))))

(defun read-configurations-from-file (&optional (file *configurations-file*))
  (let ((file (merge-pathnames file (user-homedir-pathname)))
        (*package* (find-package :cl))
        (*read-eval* t))
    (with-open-file (input file)
      (read input))))

(defun all-configurations ()
  "Returns the *CONFIGURATIONS* list"
  *configurations*)

(defun list-configurations (&optional (stream *standard-output*))
  "Prints the *CONFIGURATIONS* list"
  (maybe-restore-configurations)
  (loop for cnt from 0 
        for cnf in *configurations*
        do (format stream "~4:<~d.~> ~A~%" cnt cnf)))

(defmacro package-options (&whole args
                           &key package category-case category-separator
                                shortest-nickname
                                expr-print-format 
                                expr-log-level
                                old-logging-macros
                           &allow-other-keys)
  "Set custom options for expansion of logging macros in a specified
package. 

  PACKAGE - the package options are being set for, defaults to
  *PACKAGE*. No references to the package itself will be retained,
  instead options are keyed by package name string


  CATEGORY-CASE - Determining how logger naming converts symbols to in
  the category name.

    Valid values are: 
    - NIL        :  As printed by PRINC (ie affected by active *READTABLE*)
    - :UPCASE    :  Convert to upper case
    - :DOWNCASE  :  Convert to lower case
    - :INVERT    :  Invert in the same way inverted READTABLE-CASE does it
    - :PRESERVE  :  Do not change

  Note that pattern layout offers similar facility that changes how
  logger category is printed on the output side


  SHORTEST-NICKNAME  - When T (default), the shortest of package name or
  any of its nicknames will be used as logger category, otherwise official
  package name will be used.

  CATEGORY-SEPARATOR - String that separates logging categories, defaults to dot.

  EXPR-PRINT-FORMAT - The FORMAT control string, for two arguments
  used to print expressions, first argument is quoted expression form,
  and second argument is value. Default is \"~W=~W~^ ~:_\". If
  format string contains ~:> directive (terminate pretty printing block),
  then corresponding format argument will be a (NAME VALUE) list

  EXPR-LOG-LEVEL - the log level for the (LOG:EXPR) macro. Default
  is :DEBUG.

  OLD-LOGGING-MACROS - If set, log statement without constant format
  string such as (LOG:DEBUG a b c d) will be interpreted as logging to
  logger stored in variable A with format string B and more format
  arguments, instead of treating them as (LOG:EXPR a b c d)
  "
  ;; Lambda list only for Slime-doc 
  (declare (ignorable package category-case category-separator
                      expr-print-format shortest-nickname
                      expr-log-level
                      old-logging-macros))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%set-package-options ,@(rest args))))

(defun %set-package-options (&key (package *package*)
                                  (category-case nil category-casep)
                                  category-separator
                                  expr-print-format
                                  (shortest-nickname t shortest-nicknamep)
                                  expr-log-level
                                  (old-logging-macros nil old-logging-macrosp))
  "Processes the naming configuration options"
  (let* ((nc (find-or-create-naming-configuration (find-package package) t))
         (*naming-configuration* nc))
    (prog1 nc 
      (when category-casep (setf (%category-case nc) category-case)) 
      (when category-separator (setf (%category-separator nc) category-separator)) 
      (when expr-print-format (setf (%expr-print-format nc) expr-print-format))
      (when shortest-nicknamep (setf (%use-shortest-nickname nc) shortest-nickname))
      (when expr-log-level (setf (%expr-log-level nc) (make-log-level expr-log-level)))
      (when old-logging-macrosp (setf (%old-logging-macros nc) old-logging-macros)))))
