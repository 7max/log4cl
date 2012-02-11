(in-package :log4cl)

(defgeneric log-level-from-object (obj package)
  (:documentation "Should return numeric log level from the user
representation, can be specialized per-package to have custom log
level names. Default implementation converts object to string and
parses \"fatal\" \"debug\" and so on. Called by MAKE-LOG-LEVEL
function"))

(defgeneric naming-option (package option)
  (:documentation "Return the automatic logger naming option
for the specified package. Valid options are:

  :CATEGORY-SEPARATOR - String that separates category names, default
  method returns \":\"

  :CATEGORY-CASE - Determining how logger naming converts symbols to
  in the category name.

  Valid values are: 
    - NIL        :  as printed by princ (ie affected by active read-table case)
    - :UPCASE    :  convert to upper case
    - :DOWNCASE  :  convert to lower case
    - :INVERT    :  invert, as inverted readtables do

Note that pattern layout offers similar facility that changes how
logger category is printed on the output side."))


(defgeneric package-log-category (package categories explicit-p)
  (:documentation
   "Allows packages to optionally massage logger names in their
namespace. CATEGORIES will be a list of of category names from parent
to child, and EXPLICIT-P will be non-NIL if that list was specified as
an explicit list constant in a logging macro.

Default method will prefix the passed category list with the current
package shortest nickname. 

Example:

    ;; Some package wishes to always wrap their logger names in weird prefix and suffix
    (DEFMETHOD package-log-category ((PKG (EQL *PACKAGE*)) CATEGORIES EXPLICIT-P)
      (IF EXPLICIT-P CATEGORIES
        (APPEND '(FOO BAR) CATEGORIES '(BAZ))))

Will result in the macro (MAKE-LOGGER :NAME) returning logger named
FOO:BAR:NAME:BAZ"))

(defgeneric resolve-logger-form (package env args)
  (:documentation "Is called by all logging macros to figure out the
logger to log into. PACKAGE and ENV are the current value of *PACKAGE*
and the macro environment of the logging macro, and ARGS
are its arguments.

Returns two values, first being either a logger, or a form that when
evaluated will return a logger, and second value being list of
arguments to be passed to the format statement that will log the
message.

When second value returned is NIL, then logging macro will not log any
message but will rather expand into a non-nil value if logger is"))

(defgeneric resolve-default-logger-form (package env args)
  (:documentation "Is called by RESOLVE-LOGGER-FORM when logging macro
arguments do not specify the logger to log into. See
RESOLVE-LOGGER-FORM for return values"))

(defmethod log-level-from-object (arg package)
  "Converts human readable log level description in ARG into numeric log level.

Supported values for ARG are:

- Symbol or string which name matches log level, e.g: :debug, :info,
  DEBUG, USER1, :err \"off\"

- 1-character long symbol or string, used as a shortcut. All standard
  levels can be uniquely identified by their first
  character: (o)ff (f)atal (e)rror (w)arn (i)nfo (d)ebug (t)race (u)nset,

- 1 character digit 1 through 9 identifying user1 through user9 levels." 
  (declare (ignore package))
  (cond ((symbolp arg)
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

#-sbcl
(defmethod resolve-default-logger-form (package env args)
  "Returns the logger named after the package by default"
  (declare (ignore env))
  (values (get-logger (package-log-category package nil nil))
          args))

(defmethod package-log-category (package categories explicit-p)
  "Find the PACKAGES shortest name or nickname, and prefix category
list with it and prefix CATEGORIES list with it"
  (if explicit-p categories
      (append (split-into-categories (shortest-package-name package)
                                     package)
              categories)))

(defun shortest-package-name (package)
  "Return the shortest name or nickname of the package"
  (let ((name (package-name package)))
    (dolist (nickname (package-nicknames package))
      (when (< (length nickname) (length name))
        (setq name nickname)))
    name))

#+sbcl 
(defun include-block-debug-name? (debug-name)
  "Figures out if we should include the debug-name into the stack of
nested blocks..  Should return the symbol to use.

For now SBCL seems to use:

  SYMBOL => normal defun block
  (LABELS SYMBOL) => inside of labels function
  (FLET SYMBOL)   => inside of flet function
  (LAMBDA (arglist) => inside of anonymous lambda
  (SB-PCL::FAST-METHOD SYMBOL ...) for defmethod
  (SB-PCL::VARARGS-ENTRY (SB-PCL::FAST-METHOD SYMBOL )) for defmethod with &rest parametwer
  (SB-C::HAIRY-ARG-PROCESSOR SYMBOL) => for functions with complex lambda lists

In all of the above cases except LAMBDA we simply return SYMBOL, for
LAMBDA we return the word LAMBDA and NIL for anything else.

Example: As a result of this default logger name for SBCL for the
following form:

   (defmethod foo ()
     (labels ((bar ()
                (funcall (lambda ()
                           (flet ((baz ()
                                    (log-info \"test\")))
                             (baz))))))
       (bar)))

will be: package.foo.bar.baz

"
  (if (symbolp debug-name)
      (when (and (not (member debug-name '(sb-c::.anonymous. 
                                           sb-thread::with-mutex-thunk)))
                 (not (scan "(?i)^cleanup-fun-" (symbol-name debug-name))))
        debug-name)
      (case (first debug-name)
        (labels (include-block-debug-name? (second debug-name)))
        (flet (include-block-debug-name? (second debug-name)))
        ;; (lambda 'lambda)
        (SB-PCL::FAST-METHOD (rest debug-name))
        (SB-C::HAIRY-ARG-PROCESSOR (include-block-debug-name? (second debug-name)))
        (SB-C::VARARGS-ENTRY (include-block-debug-name? (second debug-name))))))

#+sbcl
(defun sbcl-get-block-name  (env)
  "Return a list naming SBCL lexical environment. For example when
compiling local function FOO inside a global function FOOBAR, will
return \(FOOBAR FOO\)"
  (let* ((names
           (loop
             as lambda = (sb-c::lexenv-lambda env)
             then (sb-c::lambda-parent lambda)
             while lambda
             as debug-name = (include-block-debug-name? (sb-c::leaf-debug-name lambda))
             if debug-name collect debug-name)))
    (nreverse names)))

(defun join-categories (separator list)
  "Return a string with each element of LIST printed separated by
SEPARATOR"
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (s) 
      (princ (pop list) s)
      (dolist (elem list)
        (princ separator s)
        (princ elem s)))))

#+sbcl
(defmethod resolve-default-logger-form (package env args)
  "Returns the logger named after the current lexical environment"
  (values
   (get-logger
    (package-log-category package (sbcl-get-block-name env) nil))
   args))

(defmethod naming-option (package option)
  "Return default values for naming options which are:
    :CATEGORY-SEPARATOR \":\""
  (declare (ignore package))
  (case option
    (:category-separator ":")))

(defmethod resolve-logger-form (package env args)
  "- When first element of args is NIL or a constant string, calls
 RESOLVE-DEFAULT-LOGGER-FORM that will try to obtain logger name from
 the environment

- When first argument is a :KEYWORD, returns logger named <KEYWORD>

- When first argument is a quoted symbol, returns logger named
  <current-package>.<symbol>

- Otherwise returns the form `(GET-LOGGER ,(FIRST ARGS) ,@(REST ARGS))'"
  (cond
    ((or (null args)
         (stringp (first args)))
     (resolve-default-logger-form package env args))
    ((keywordp (first args))
     (values (get-logger
              (package-log-category package
               (split-into-categories (symbol-name (first args)) package)
               nil))
             (rest args)))
    ((constantp (first args))
     (let ((value (eval (first args))))
       (cond ((symbolp value)
              (get-logger (package-log-category package
                           (split-into-categories (symbol-name value) package)
                           nil)))
             ((listp value)
              (get-logger (package-log-category package value t)))
             (t (values (first args) (rest args))))))
    (t
     (values (first args) (rest args)))))

(defun write-string-modify-case (string stream
                                 &optional
                                 case
                                 (start 0)
                                 (end (length string)))
  "Helper function that writes STRING to STREAM, optionally doing case
conversion."
  (declare (type simple-string string)
           (type stream stream)
           (type fixnum start end))
  (cond ((eq case :upcase)
         (loop for i fixnum from start below end
               do (write-char (char-upcase (schar string i))
                              stream)))
        ((eq case :downcase)
         (loop for i fixnum from start below end
               do (write-char (char-downcase (schar string i))
                              stream)))
        ((eq case :invert)
         (let ((all-up t)
               (all-down t))
           (loop for i fixnum from start below end
                 as c = (schar string i)
                 if (upper-case-p c)
                 do (setq all-down nil)
                 if (lower-case-p c)
                 do (setq all-up nil))
           (cond
             (all-down (write-string-modify-case
                        string stream :upcase start end))
             (all-up (write-string-modify-case
                      string stream :downcase start end))
             (t (write-string string stream :start start
                                            :end end)))))
        (t (write-string string stream :start start
                                       :end end)))
  (values))

