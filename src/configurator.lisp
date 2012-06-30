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
(in-package #:log4cl-impl)

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

  Option     | Description
-------------|---------------------------------------------------------------
 :INFO       | Or any other keyword identifying a log level, which can be    
 :DEBUG      | shortened to its shortest unambiguous prefix, such as :D      
-------------|---------------------------------------------------------------
 :CLEAR      | Removes log level and appenders from any child loggers,       
             | appenders are not removed from non-additive loggers           
-------------|---------------------------------------------------------------
 :ALL        | Changes :CLEAR to remove appenders from non-additive          
             | loggers                                                       
-------------|---------------------------------------------------------------
 :SANE       | Removes logger appenders, adds console appender with          
             | pattern layout that makes messages look like this:            
             |                                                               
             | :     [11:22:25] INFO  {category.name} - message              
-------------|---------------------------------------------------------------
 :OWN        | For :SANE and :DAILY makes logger non-additive                
             | otherwise additive flag will be set                           
-------------|---------------------------------------------------------------
 :DAILY FILE | Adds file appender logging to the named file, which will      
             | be rolled over every midnight into FILE.YYYYMMDD; Removes any 
             | other other appenders that subclass FILE-APPENDER-BASE from   
             | the logger.  If :SANE is also specified, all logger appenders 
             | are removed, but console appender is not added                
-------------|---------------------------------------------------------------
 :CONSOLE    | Forces adding of console appender if :DAILY was specified     
-------------|---------------------------------------------------------------
 :PATTERN    | For :SANE option uses specified conversion pattern instead    
 STRING      | of default one                                                
-------------|---------------------------------------------------------------
 :TWOLINE    | Changes default pattern layout to print user log message      
             | log message on 2nd line after the headers                     
-------------|---------------------------------------------------------------
 :SELF       | Used for debugging LOG4CL itself. Instead of root logger,     
             | make default logger LOG4CL:SELF and remember all arguments    
             | in the variable *SELF-LOG-CONFIG*, so that they are restored  
             | even on (CLEAR-LOGGING-CONFIGURATION). Automatically assumes  
             | :OWN making the LOG4CL-IMPL:SELF logger non-additive          
-------------|---------------------------------------------------------------
 :PROPERTIES | Configure with PROPERTY-CONFIGURATOR by parsing specified     
 FILE        | properties file                                               
-------------|---------------------------------------------------------------
 :WATCH      | Used with :PROPERTIES, uses watcher thread to check           
             | properites file modification time, and reloads if it changes  
-------------|---------------------------------------------------------------
 :IMMEDIATE- | Used with :SANE, :DAILY or :CONSOLE to create new appenders
  FLUSH      | with :IMMEDIATE-FLUSH T option, which prevents automatic
             | startup of hierarchy watcher thread, which is used for
             | auto-flushing. 
-------------|---------------------------------------------------------------

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
        self appenders
        immediate-flush
        properties watch)
    (cond ((logger-p (car args))
           (setq logger (pop args)))
          ((consp (car args))
           (setq logger (get-logger-internal
                         (pop args)
                         (naming-option *package* :category-separator)
                         (naming-option *package* :category-case))))
          ((member :self args)
           (setq logger (make-logger '(log4cl-impl self))
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
          (:immediate-flush (setq immediate-flush t))
          ((:twoline :two-line) (setq twoline t))
          (:console (setq console t))
          (:watch (setq watch t))
          (:daily
           (setq daily (or (pop args)
                           (log4cl-error ":DAILY missing argument"))))
          (:properties
           (setq properties (or (pop args)
                                (log4cl-error ":PROPERTIES missing argument"))))
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
    (or level sane clear daily properties own
        (log4cl-error "A log level or one of :SANE :CLEAR :OWN :DAILY or :PROPERTIES must be specified"))
    (or (not properties)
        (not (or sane daily pattern console level))
        (log4cl-error ":PROPERTIES can't be used with :SANE :DAILY :PATTERN or log level"))
    (when level
      (set-log-level logger level nil))
    (when clear
      (map-logger-descendants
       (lambda (l)
         (set-log-level l +log-level-unset+ nil)
         (when (or (logger-additivity l) all)
           (remove-all-appenders-internal l nil)))
       logger))
    (when own
      (set-additivity logger nil nil))
    (when (or daily sane)
      (let ((default-pattern "[%D{%H:%M:%S}] [%P] <%c{}{}{:downcase}> - %m%n")
            (twoline-pattern "[%D{%H:%M:%S}] [%-5P] <%c{}{}{:downcase}>%n  *%I{>} %m%n"))
        (setq layout (make-instance 'pattern-layout
                      :conversion-pattern
                      (or pattern
                          (if twoline twoline-pattern
                              default-pattern)))))
      (cond
        (daily (if sane (remove-all-appenders-internal logger nil)
                   (dolist (a (logger-appenders logger))
                     (when (or (typep a 'file-appender-base)
                               (and console (typep a 'console-appender)))
                       (remove-appender-internal logger a nil))))
               (push (make-instance 'daily-file-appender
                      :name-format daily
                      :backup-name-format (format nil "~a.%Y%m%d" daily)
                      :layout layout)
                     appenders))
        (t (remove-all-appenders-internal logger nil)))
      (when (or console (and sane (not daily)))
        (push (make-instance 'console-appender :layout layout)
              appenders))
      (dolist (a appenders)
        (when immediate-flush
          (setf (slot-value a 'immediate-flush) t))
        (add-appender-internal logger a nil))
      (set-additivity logger (not own) nil))
    (when properties
      (configure (make-instance 'property-configurator) properties
                 :auto-reload watch))
    (when self
      ;; This is special adhoc case of configuring the LOG4CL-iMPL:SELF
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
                               (logger-appenders l)
                               (not (logger-additivity l))
                               (logger-log-level l)
                               (some #'interesting-logger-p (logger-children l)))))))
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
                      (children (remove-if-not #'interesting-logger-p (logger-children l)))
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
                   (write-string " (non-additive)"))
                 (when lvl
                   (format t ", ~A" (log-level-to-string lvl)))
                 (terpri)
                 (when appenders
                   (dolist (a appenders)
                     (print-one-appender a)))
                 (when (some #'interesting-logger-p children)
                   (dolist (l children)
                     (when (interesting-logger-p l)
                       (print-indent) (terpri)
                       (print-one-logger l))))
                 (pop indents)))
             (print-properties (obj)
               (let* ((props (property-alist obj))
                      (name-width (loop for prop in props maximize
                                           (length (format nil "~s" (car prop))))))
                 (loop for (initarg slot nil) in props
                       do (print-indent)
                       do (format t "~v<~(~s~)~;~> ~s~%"
                                  name-width
                                  initarg
                                  (slot-value obj slot)))))
             (print-one-appender (a)
               ;; empty line for spacing
               (print-indent) (terpri)
               ;; now the appender node
               (print-indent t) (format t "-~A~%" a) 
               ;; indent appender attributes and layout under the appender,
               ;; don't draw the tree for them
               (push (cons 5 0) indents)
               (print-layout (slot-value a 'layout))
               (print-properties a)
               (pop indents))
             (print-layout (layout)
               (print-indent)
               (format t "with ~A~%" layout)
               (push (cons 5 0) indents)
               (print-properties layout)
               (pop indents)))
      (print-one-logger logger))
    (values)))

;; do default configuration
(defvar *default-init-done-p* nil)

(defun perform-default-init ()
  (unless *default-init-done-p*
    (setq *default-init-done-p* t)
    (clear-logging-configuration)
    (log-config :i :sane :immediate-flush)))

(perform-default-init)

;;;
;;; Logging configuration quick save / restore
;;; 

(defclass logging-configuration-element ()
  ((logger :initarg :logger :type logger
           :initform (error "Required argument ~s missing" :logger)
           :reader logger-of)
   (level :initarg :level
          :type keyword
          :initform (error "Required argument ~s missing" :level)
          :reader level-of))
  (:documentation "Holds logger and its log level"))

(defclass logging-configuration ()
  ((name :type atom :initarg :name 
         :initform (error "Required argument ~s missing" :name)
         :accessor name-of)
   (elements :initarg :elements
             :accessor elements-of
             :initform (remember-logging-configuration)))
  (:documentation "Used to remember log levels for a set of loggers"))

(defun remember-logging-configuration (&optional (logger *root-logger*))
  (flet ((make-element (logger level)
           (make-instance 'logging-configuration-element
            :logger logger
            :level (if level (aref +log-level-to-keyword+ level) :unset))))
    (cons (make-element logger (logger-log-level logger)) 
          (loop for logger in (logger-descendants logger)
                for level = (logger-log-level logger)
                if level collect (make-element logger level)))))

(defmethod print-object ((elem logging-configuration-element) stream)
  (with-slots (logger level) elem
    (if (not *print-readably*)
        (print-unreadable-object (elem stream :type t)
          (princ (if (logger-parent logger) (logger-category logger)
                     "+ROOT+") stream)
          (princ #\Space stream)
          (prin1 level stream))
        (format stream  "#.~S"
                `(make-instance 'logging-configuration-element
                  :logger
                  (get-logger-internal ',(logger-categories logger)
                                       ,(logger-category-separator logger)
                                       nil)
                  :level ,level)))))

(defmethod print-object ((cnf logging-configuration) stream)
  (with-slots (name elements) cnf
    (if (not *print-readably*)
        (print-unreadable-object (cnf stream :type t)
          (format stream "~S (~d)"
                  name (count :unset elements :key #'level-of :test-not #'eq)))
        (format stream  "#.~S"
                `(make-instance 'logging-configuration
                  :name ',name
                  :elements ',elements)))))


(defvar *configurations* nil
  "List of unique LOGGER-CONFIGURATION objects")

(defvar *max-configurations* 30
  "Maximum number of configurations in *CONFIGURATIONS* list.")

(defvar *default-logging-configuration-scope* :root
  "Default scope for (log:save). Should be one of
  :ROOT -- save configuration starting from root logger
  :CONTEXT -- save configuration starting from default logger as made by (LOG:MAKE)
  <logger> -- logger object or a form that when evaluated returns a logger")

(defvar *save-configurations-to-file* t
  "When non-NIL SAVE-CONFIGURATION function will also write
configurations to a *CONFIGURATIONS-FILE*")

(defvar *configurations-file* ".log4cl-configurations.lisp-expr")

(defun logging-configuration= (c1 c2)
  "Compare two logging configurations and return T if they have
exactly same loggers and levels"
  (declare (type logging-configuration c1 c2))
  (or (eq c1 c2)
      (not (flet ((test (e1 e2) 
                    (and (eq (logger-of e1) (logger-of e2))
                         (eq (level-of e1) (level-of e2)))))
             (set-exclusive-or (elements-of c1) (elements-of c2) :test #'test)))))

(defmacro save (&optional name (scope *default-logging-configuration-scope*))
  "Save current logging configuration into *CONFIGURATIONS* list

NAME--specifies the name of this logging configuration, if NAME is not
specified, one is automatically provided as \"Saved on <timestamp>\".

SCOPE--Specifies the logger to start from. When :ROOT start from root logger
and when :CONTEXT start from the default logger as returned by (LOG:MAKE).
Default can be specified in *DEFAULT-LOGGING-CONFIGURATION-SCOPE*

The configuration will be pushed on top of *CONFIGURATIONS* list,
which then will be trimmed to *MAX-CONFIGURATIONS* elements.

When *SAVE-CONFIGURATIONS-TO-FILE* is T (default) the *CONFIGURATIONS*
list will also be saved to a file \".log4cl-configurations.lisp-expr\" in user
home directory. File name can be customized via *CONFIGURATIONS-FILE*"
  (unless name
    (setq name (with-output-to-string (s)
                 (format-time s "Saved on %Y-%m-%d %H:%M:%S" (get-universal-time) nil))))
  (let* ((logger-expr (cond
                        ((member scope '(:context t)) `(make-logger))
                        ((eql scope :root) '*root-logger*)
                        (t scope))))
    `(save-configuration
      (make-instance 'logging-configuration :name ,name
       :elements (remember-logging-configuration ,logger-expr)))))


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

(defun make-current-configuration ()
  (make-instance 'logging-configuration
                   :name (with-output-to-string (s)
                           (format-time s "Autosave on %Y-%m-%d %H:%M:%S"
                                        (get-universal-time) nil))))

(defun restore (&optional configuration)
  "Restore logging configuration CONFIGURATION, which can be a name,
a LOGGING-CONFIGURATION instance, or a number indicating Nth configuration
in the *CONFIGURATIONS* list.

Before restoring the configuration, the current logging configuration
is automatically saved under the name \"Autosave <timestamp>\", unless
an equivalent configuration is already in the list

If CONFIGURATION is NIL restores first element of *CONFIGURATIONS* that is
not equivalent to the current configuration. So successive (RESTORE) will
swap last two configurations"
  (when (and (null *configurations*)
             (probe-file (merge-pathnames *configurations-file*
                                          (user-homedir-pathname))))
    (setq *configurations* (read-configurations-from-file)))
  
  (let* ((current (make-current-configuration))
         (current-dup (find current *configurations* :test #'logging-configuration=))
         (cnf 
           (cond ((null configuration)
                  ;; restore the first configuration not equivalent to the current one
                  (or (find current *configurations* :test-not #'logging-configuration=)
                      (error (if *configurations*
                                 "All stored configurations are equivalent to the current one"
                                 "There are no stored configurations to restore"))))
                 ((typep configuration 'logging-configuration)
                  configuration)
                 ((integerp configuration)
                  (or (<= 0 configuration (length *configurations*))
                      (error "Invalid configuration index ~D" configuration))
                  (or (find current *configurations* :test-not #'logging-configuration=
                                                     :start configuration)
                      (error "All configurations starting from ~D are equivalent to the current one"
                             configuration)))
                 (t (or (find configuration *configurations* :key #'name-of :test #'equal)
                        (error "Logging configuration ~S not found" configuration)))))
         (same-as-current-p (logging-configuration= cnf current)))
    ;; (log-sexp save save-dup (find save *configurations* :test #'logging-configuration=))
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

(defun save-configuration (cnf)
  "Save CNF logging configuration into *CONFIGURATIONS* list,
overwriting the previous one by the same name."
  (declare (type logging-configuration cnf))
  (let ((old (find (name-of cnf) *configurations* :key #'name-of :test #'equal)))
    ;; only keep configuration with the same name if they are different
    (when (and old (logging-configuration= old cnf))
      (setq *configurations* (remove old *configurations*))))
  (push cnf *configurations*)
  (trim-configuration-list '*configurations*)
  (when *save-configurations-to-file*
    (save-configurations-to-file))
  (first *configurations*))

(defun save-configurations-to-file (&optional (file *configurations-file*))
  (let ((file (merge-pathnames file (user-homedir-pathname))))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write *configurations* :stream out :length nil :readably t :circle nil :pretty nil))))

(defun read-configurations-from-file (&optional (file *configurations-file*))
  (let ((file (merge-pathnames file (user-homedir-pathname))))
    (with-open-file (input file)
      (read input))))


