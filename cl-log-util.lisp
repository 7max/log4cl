
(in-package :cl-log)

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

(defparameter +log-level-from-letter+ "OFEWID1234T56789U")
(defparameter +log-level-from-string+ 
  '("OFF" "FATAL" "ERROR" "WARN" "INFO"
    "DEBUG" "USER1" "USER2" "USER3" "USER4" "TRACE"
    "USER5" "USER6" "USER7" "USER8" "USER9" "UNSET"))


(defun make-log-level (arg)
  "Translate a more human readable log level into one of the above
  constants. Accepted formats can be:

  Symbol or string which name matches one level, e.g: :debug, :info,
  DEBUG, USER1, :err \"off\"

  1-character long symbol or string, used as a shortcut. All standard
  levels can be uniquely identified by their first
  character: (o)ff (f)atal (e)rror (w)arn (i)nfo (d)ebug (t)race (u)nset,

  1 character digit 1 through 9 identyfing user1 through user9 levels." 
  (cond ((null arg)
         +log-level-unset+)
        ((symbolp arg)
         (make-log-level (symbol-name arg)))
        ((stringp arg)
         (let ((len (length arg))
               match)
           (if (= 1 len)
               (setf match (position (char-upcase (char arg 0))
                                     +log-level-from-letter+))
               (let* ((name (string-upcase arg))
                      (len (length name)))
                 (loop 
                    for level from 0
                    for level-name in +log-level-from-string+
                    for tmp = (mismatch name level-name)
                    if (or (null tmp)
                           (= tmp len))
                    do (if match
                           (error "~s matches more then one log level" arg)
                           (setf match level)))))
           (or match 
               (error "~s does not match any log levels" arg))))
        ((and (numberp arg)
              (>= arg +min-log-level+)
              (<= arg +log-level-unset+))
         arg)
        (t (error "~s does not match any log levels" arg))))

(defvar *default-logger-name* nil 
  "Logging macros will use this value for the default logger name
if logger is not specified. Very useful in combination with
cl-log-demacs-integration and cl-log-cl-def-integration, allowing
loggers to be automatically named after the surrounding block
that they appear in.

Value can also be a function-p in which case it will be called
with the parameter (env) from the macro-environment and is expected
to return string.")

(defvar *log-indent* 0
  "Indent level can be used to indent logging info")

(declaim (type fixnum *log-indent*))

(defmacro log-indented (&body body)
  `(let ((*log-indent* (1+ *log-indent*)))
     (log-trace "start")
     (prog1 
         (let ((*log-indent* (1+ *log-indent*)))
           ,@body)
       (log-trace "end"))))

(defun get-package-and-block-name (env)
  (cond 
    ((stringp  *default-logger-name*) *default-logger-name*)
    ((and *default-logger-name* 
          (typep *default-logger-name* '(or function symbol)))
     (funcall *default-logger-name* env))
    (t nil)))

;; design
;;
;;  1. Log messages can be written to a logger object. There can be
;;     many logger objecs in a system. Each logger is identified by a
;;     name.
;;
;;  2. Messages written to a logger are actually written to logger's
;;     appenders.  The appender is an object that actually does
;;     something with the log message
;;
;;  3. Loggers are organized into a hierarchical structure, whereby child
;;     logger inherit settings from the parent logger. When parent logger
;;     with name "parent" have a child logger with name "child" then the 
;;     child logger can be refferred to as "parent.child"
;;
;;  4. There can be multiple independent configurations of the logger
;;     hierachy. So for example two web apps using the same library
;;     can have library.somelogger configured differently.
;;
;;     By default theri is a single configuration. The current
;;     configuration is referenced via a shared global variable
;;
;;  5. Easy of usage and configuring of loggig system are given the 
;;     highest priority. 
;;
;;  
;;     Example usage:
;;
;;   (defvar logger (make-logger "this.is.a.logger)
;;   (auto-logger-options (
;;
;;   (log-debug logger "test")
;;   (log-debug-vars var1 var2 var3)
;;   (log-debug "~d ~d ~d" var1 var2 var3)
;;   (log-debug)
;;
;;   ;;; configuraiton from REPL
;;
;;   (log *.* debug)
;;   (log *.* info)
;;   (log *do-something* debug)
;;   (log *some-logger-name



;;
;; internal variables
;;
(declaim (special *root-logger* *num-apps* *log-app-number* *apps*)
	 (type logger *root-logger*)
	 (type fixnum  *num-apps* *log-app-number*)
	 (type hash-table *apps*))

(defstruct logger-app-data
  "Structure containng logger internal state"
  ;; list of appenders attached to this logger
  (appenders nil :type list)
  ;; log level for this logger, if nil then inherited
  ;; from the parent logger
  (level nil :type (or null fixnum))
  ;; performance hack, for each log level we set a bit
  ;; in mask, if a log message with said level will
  ;; reach any appenders either in this logger or it's parents
  ;;
  ;; adjust-logger function recalculates
  ;; this value for parent and child loggers, its
  ;; called automatically by set-log-level, add-appender
  ;; and logger creation functions
  (mask 0 :type fixnum))

(defstruct 
    (logger 
      (:constructor create-logger)
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
  ;; per-app configuration
  (app-data
   (make-array *num-apps* :element-type 'logger-app-data
	       :initial-element (make-logger-app-data))
   :type (simple-array logger-app-data *)))

(defvar *num-apps* 1
  "Number of applications registered with the cl-log library")

(defvar *log-app-number* 0
  "Current appplication number. All logging function use per-app
state indexed by this number")

(defun inherited-level (logger)
  (declare (type (or null logger) logger))
  (if (null logger) +log-level-off+
      (let* ((appdata (current-appdata logger))
	     (level (logger-app-data-level appdata)))
	(or level
	    (inherited-level (logger-parent logger))))))

(defun have-appenders-for-level (logger level)
  (declare (type logger logger) (type fixnum level))
  (labels ((have-appenders (logger)
	     (when logger
	       (or (logger-app-data-appenders (current-appdata logger))
		   (have-appenders (logger-parent logger))))))
    (let* ((logger-level (inherited-level logger)))
      (and (>= logger-level level)
	   (have-appenders logger)))))

(defun map-logger-children (function logger)
  "Apply the function to all of logger's children (but not their descedants)" 
  (let ((child-hash (logger-children logger)))
    (when child-hash
      (maphash (lambda (name logger)
                 (declare (ignore name))
                 (funcall function logger))
               child-hash))))

(defun adjust-logger (logger)
  (labels ((doit (logger)
             (let ((appdata (current-appdata logger))
                   (mask 0))
               (declare (type fixnum mask))
               (loop
                  for level from +min-log-level+ upto +max-log-level+
                  if (have-appenders-for-level logger level)
                  do (setf mask (logior mask (ash 1 level))))
               (setf (logger-app-data-mask appdata) mask)
               (map-logger-children #'doit logger))))
    (doit logger))
  (values))

(defun shortest-package-name (package)
  "Return the shortest name or nickname of the package"
  (let ((name (package-name package)))
    (dolist (nickname (package-nicknames package))
      (when (< (length nickname) (length name))
        (setq name nickname)))
    name))


#+sbcl 
(defun include-block-debug-name? (debug-name)
  "Figures out if we should include the debug-name into the stack of nested blocks.. 
Should return the symbol to use.

For now SBCL seems to use:
  SYMBOL => normal defun block
  (LABELS SYMBOL) => inside of labels function
  (FLET SYMBOL)   => inside of flet function
  (LAMBDA (arglist) => inside of anonymous lambda
  (SB-PCL::FAST-METHOD SYMBOL ...) for defmethod
  (SB-PCL::VARARGS-ENTRY (SB-PCL::FAST-METHOD SYMBOL )) for defmethod with &rest parametwer
  (SB-C::HAIRY-ARG-PROCESSOR SYMBOL) => for functions with complex lambda lists

In all of the above cases except LAMBDA we simply return SYMBOL, for LAMBDA we return the word LAMBDA
and NIL for anything else.

Example: As a result of this default logger name for SBCL for the
following form:

   (defmethod foo ()
     (labels ((bar ()
                (funcall (lambda ()
                           (flet ((baz ()
                                    (log-info \"test\")))
                             (baz))))))
       (bar)))

will be: package.foo.bar.lambda.baz

"
  (if (symbolp debug-name)
      (when (and (not (member debug-name '(sb-c::.anonymous. 
                                           sb-thread::with-mutex-thunk)))
                 (not (scan "(?i)^cleanup-fun-" (symbol-name debug-name))))
        debug-name)
      (case (first debug-name)
        (labels (include-block-debug-name? (second debug-name)))
        (flet (include-block-debug-name? (second debug-name)))
        (lambda 'lambda)
        (SB-PCL::FAST-METHOD (second debug-name))
        (SB-C::HAIRY-ARG-PROCESSOR (include-block-debug-name? (second debug-name)))
        (SB-C::VARARGS-ENTRY (include-block-debug-name? (second debug-name))))))



;; (defvar tmp)

#+sbcl
(defun sbcl-get-package-and-block-name  (env)
  "Should return a string"
  ;; (setf tmp env)
  (flet ((ensure-string (atom)
	   (cond ((symbolp atom) (symbol-name atom))
		 ((stringp atom) atom)
		 (t (princ-to-string atom)))))
    (let* ((names
            (loop
               as lambda = (sb-c::lexenv-lambda env)
               then (sb-c::lambda-parent lambda)
               while lambda
               as debug-name = (include-block-debug-name? (sb-c::leaf-debug-name lambda))
               if debug-name collect debug-name))
           (name (reduce
                  (lambda (name1 name2)
                    (concatenate 'string (ensure-string name1)
                                 "." (ensure-string name2)))
                  (append (list (shortest-package-name *package*))
                          (nreverse names)))))
      (values name))))

#+sbcl 
(setq *default-logger-name* #'sbcl-get-package-and-block-name)

;; appender interface
(defgeneric do-append (appender logger level message))


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
                (dotimes (*log-app-number* *num-apps*)
                  (adjust-logger logger)))
              logger))))

#|(defun make-logger (name)
  "Get the logger with a given category name. Logger is created
if it does not exist"
  (declare (type string name))
  (setq name (string-downcase name))
  (flet ((split-string (string char)
	   (loop 
	      for start = 0 then (1+ next)
	      as next = (position char string :start start)
	      collect (subseq string start next)
	      until (null next)))
	 (find-closest-parent (names)
	   (loop 
	      for i from 0 upto (1- (length names))
	      for parent = *root-logger* then 
		(or (gethash (subseq names 0 i) 
			     (logger-children parent)) parent)
	      finally (return parent)))
	 (find-children (logger prefix)
	   (loop 
	      for names being each hash-key of (logger-children logger)
	      using (hash-value child)
	      if (equal prefix (subseq names 0 (length prefix)))
	      collect (cons child names)))
	 (reroot-children (children old-parent new-parent)
	   (loop for (child . names) in children do 
		(progn
		  (remhash names (logger-children old-parent))
		  (setf (gethash names (logger-children new-parent)) 
			child
			(logger-parent child) 
			new-parent)))))
    (let* ((names (split-string name #\.))
	   (parent (find-closest-parent names))
	   (logger (gethash names (logger-children parent))))
      (or logger 
	  (let* ((new-logger (create-logger :name name
					    :parent parent))
		 (new-children (find-children parent names)))
	    (setf (gethash names (logger-children parent)) new-logger)
	    (reroot-children new-children parent new-logger)
	    (adjust-logger parent)
	    new-logger)))))
|#
			     
(defmethod make-load-form ((log logger) &optional env)
  "Creates the logger when a logger constant is being loaded
from a compiled file"
  (declare (ignore env))
  `(get-logger ,(logger-name log)))

(declaim (inline is-enabled-for current-appdata))

(defun current-appdata (logger)
  (svref (logger-app-data logger) *log-app-number*))

(defun is-enabled-for (logger level)
  "Returns t if log level is enabled for the logger in the
context of the current application."
  (declare (type logger logger) (type fixnum level))
  (let* ((app-data (current-appdata logger))
	 (mask (logger-app-data-mask app-data)))
    (declare (type fixnum mask))
    (not (zerop (logand (the fixnum (ash 1 level)) mask)))))

(defmacro log-debug (&rest args &environment env)
  "Logs the message with the logger.

Logger is found by examinining the first argument of the macro.

If first argument is a string then logger name is automatically determined
in a implementation dependend manner. On implementations that support it
the logger name would be package.block-name. On implementations where
it's not possible to determine the block name that is currently being compiled
the logger name would be just package
"
  (log-with-level env +log-level-debug+ args))


(defmacro deflog-macros (&rest levels)
  (let (list)
    (dolist (level levels)
      (let ((log-macro-name (intern (format nil "LOG-~a" level)))
            (level-name (intern (format nil "+LOG-LEVEL-~a+" level))))
        (push `(defmacro ,log-macro-name (&rest args &environment env)
                 "Logs the message with the logger.

Synopsis:

 (defun foo ()
   ;; DEBUG - cl-user.foo: entered
   (log-debug \"entered\")

   ;; DEBUG - cl-user.bar: message
   (log-debug :bar \"message\")

   ;; INFO - full.logger.name: message
   (log-info :/full.logger.name \"message\")

   ;; DEBUG - cl-user.foo.test: entered
   ;; DEBUG - cl-user.foo.test: arg1=1 arg2=2
   (flet ((test (arg1 arg2)
            (log-debug \"enter\")
            (log-debug arg1 arg2)))
     (test 1 2)))

Logger category is found by examinining the first argument of the macro.

If first argument is a string then logger name is automatically determined
in a implementation dependend manner. On implementations that support it
the logger name would be package.block-name. On implementations where
it's not possible to determine the block name that is currently being compiled
the logger name would be just package.

"
                 (log-with-level env ,level-name args))
              list)))
    `(progn
       ,@(nreverse list))))

(deflog-macros debug fatal error warn info user1 user2 user3 user4 
               trace user5 user6 user7 user8 user9)

(defun log-with-level (env level args)
  "Returns a FORM that is used as an expansion of log-nnnnn macros"
  (declare (type fixnum level)
	   (type list args))
  (multiple-value-bind (logger-name args)
      (maybe-auto-logger-name args env)
    (let* ((logger (get-logger logger-name))
           (logger-symbol (gensym "logger")))
      (if args 
          `(let ((,logger-symbol ,logger))
             (when
                 (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                   (is-enabled-for ,logger-symbol ,level))
               (log-with-logger ,logger-symbol ,level
                                (lambda (stream)
                                  (format stream ,@args))))
             (values))
          ;; null args means its being used for checking if level is enabled
          ;; in the (when (log-debug) ... complicated debug ... ) way
          `(let ((,logger-symbol ,logger))
             (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
               (is-enabled-for ,logger-symbol ,level)))))))

(defun maybe-auto-logger-name (args env)
  "Return logger name and rest of the arguments based on arguments for (log-XXX) funcitons "
  (declare (type list args))
  (let ((arg (first args)))
    (cond
      ;; no parameters mean automatically determine logger name and
      ;; return true if logging is enabled for (when (log-debug) ....)
      ((null args)
       (values (get-package-and-block-name env) nil))
      ;; (log-debug "~s" foo) automatically determine logger name from
      ;; context
      ((stringp arg)
       (values (get-package-and-block-name env) args))
      ;; (log-debug ::logger whatever) or (log-debug :sublogger whatever)
      ;; take the logger name from the keyword (:: means from root)
      ((keywordp arg)
       (values (logger-name-from-symbol arg env)
               (rest args)))
      (t
       (error "(maybe-auto-logger-name): Unable to figure out type of log statement")))))


(defmacro log-sexp (&rest args) 
  (let ((format 
         (with-output-to-string (*standard-output*)  
           (let ((first t))
             (dolist (arg args)
               (unless first
                 (write-string " "))
               (setf first nil)
               (format t "~s=~~s" arg))))))
    `(log-debug ,format ,@args)))

(defun substr (seq from &optional (to (length seq)))
  (declare (fixnum from to)
           (vector seq))
           (make-array (- to from) :element-type (array-element-type seq)
                       :displaced-to seq :displaced-index-offset from))
         
(defun logger-name-from-symbol (symbol env)
  "Return a logger name from a symbol."
  (declare (type keyword symbol) (ignore env))
  (let ((name (symbol-name symbol)))
    (cond ((char= (char name 0) #\/)
           (format nil "~(~a~)" (substr name 1)))
          (t
           (format nil "~(~a.~a~)" (shortest-package-name *package*) name)))))
	  
(defun log-with-logger (logger level log-func)
  "Submit message to logger appenders, and its parent logger"
  (labels ((log-to-logger-appenders (logger orig-logger level log-func)
	     (let* ((app-data (current-appdata logger))
		    (appenders
		     (logger-app-data-appenders app-data)))
	       (dolist (appender appenders)
		 (appender-do-append appender level (logger-name orig-logger)
				     log-func)))
	     (let ((parent (logger-parent logger)))
	       (when  parent
		 (log-to-logger-appenders parent orig-logger level log-func)))
	     (values)))
    (log-to-logger-appenders logger logger level log-func)
    (values)))


(defun set-log-level (logger level &optional (adjust-p t))
  "Set the log level of a logger. Returns T if level was set or NIL if
level was already set to stpecified value."
  (declare (type logger logger))
  (let* ((level (make-log-level level))
         (app-data (current-appdata logger))
         (old-level (logger-app-data-level app-data))
         (new-level (when (/= level +log-level-unset+) level)))
    (declare (type fixnum level)
             (type logger-app-data app-data)
             (type (or null fixnum) old-level new-level))
    (unless (eql old-level new-level)
      (setf (logger-app-data-level app-data) new-level)
      (when adjust-p
        (adjust-logger logger))
      t)))

(defun add-appender (logger appender)
  (declare (type logger logger) (type appender appender))
  (let* ((app-data (current-appdata logger)))
    (unless (member appender (logger-app-data-appenders app-data))
      (push appender (logger-app-data-appenders app-data))
      (adjust-logger logger)))
  (values))

(defun add-app (app-name)
  (declare (type string app-name))
  (let ((app-num (gethash app-name *apps*)))
    (unless app-num
      (setf app-num *num-apps*)
      (setf (gethash app-name *apps*) app-num)
      (incf *num-apps*)
      (adjust-all-loggers-appdata))
    app-num))


(defun adjust-all-loggers-appdata ()
  (values))

(defun create-root-logger ()
  (let ((root (create-logger :name "")))
    (adjust-logger root)
    root))

(defvar *root-logger*
  (create-root-logger)
  "The root logger")

(defun clear-all-loggers ()
  "Delete all loggers"
  (setq *root-logger* (create-root-logger))
  (values))

(defmacro make-logger (&optional arg &environment env)
  (cond
    ((null arg)
     (get-logger (maybe-auto-logger-name (list "") env)))
    ((keywordp arg)
     (get-logger (maybe-auto-logger-name (list arg) env)))
    (t (get-logger arg))))


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
               (when (scan regexp (logger-name logger))
                 (push logger loggers))
               (map-logger-children #'doit logger)))
      (doit *root-logger*))
    (nreverse loggers)))
