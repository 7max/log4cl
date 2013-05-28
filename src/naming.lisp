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

(in-package #:log4cl)

(defparameter +expr-format-simple+ "~W: ~W ~:_")
(defparameter +expr-format-fancy+ "~:_~<~(~W~): ~2I~_~W~:> ")

(defclass naming-configuration ()
  ((category-separator :initform "." :accessor %category-separator)
   (category-case :initform nil :accessor %category-case)
   (expr-print-format :initform +expr-format-simple+ :accessor %expr-print-format)
   (use-shortest-nickname :initform  nil :initarg :use-shortest-nickname :accessor %use-shortest-nickname)
   (expr-log-level :initform +log-level-debug+ :accessor %expr-log-level)
   (old-logging-macros :initform nil :accessor %old-logging-macros))
  (:documentation "Contains configuration that affects expansion of logger macros."))

(defvar *naming-configuration* nil
  "Naming configuration currently in effect")

;; shortcuts
(defun category-separator (&optional (nc *naming-configuration*))
  (%category-separator nc))
(defun category-case (&optional (nc *naming-configuration*))
  (%category-case nc))
(defun expr-print-format (&optional (nc *naming-configuration*))
  (%expr-print-format nc))
(defun use-shortest-nickname (&optional (nc *naming-configuration*))
  (%use-shortest-nickname nc))
(defun old-logging-macros (&optional (nc *naming-configuration*))
  (%old-logging-macros nc))
(defun expr-log-level (&optional (nc *naming-configuration*))
  (%expr-log-level nc))

(defparameter *default-naming-configuration* (make-instance 'naming-configuration :use-shortest-nickname t)
  "Default naming configuration")

(defparameter *dotted-naming-configuration*
  (make-instance 'naming-configuration :use-shortest-nickname nil)
  "Naming configuration for dotted packages")

(defvar *naming-configurations* (make-hash-table :test #'equal))

(defun dotted-package-p (package)
  (not (null (position #\. (package-name package)))))

(defun find-or-create-naming-configuration (p &optional createp)
  (declare (type package p))
  (let ((nc (gethash (package-name p) *naming-configurations*)))
    (or nc (and createp (setf (gethash (package-name p) *naming-configurations*)
                              (make-instance 'naming-configuration
                               :use-shortest-nickname (not (dotted-package-p p))))))))

(defun ensure-naming-configuration (package) 
  (or (find-or-create-naming-configuration package)
      (if (dotted-package-p package)
          *dotted-naming-configuration*
          *default-naming-configuration*)))

(defmacro with-package-naming-configuration ((package) &body body)
  `(let ((*naming-configuration* (ensure-naming-configuration ,package)))
     ,@body))

(defgeneric log-level-from-object (obj package)
  (:documentation "Should return numeric log level from the user
representation, can be specialized per-package to have custom log
level names. Default implementation converts object to string and
parses \"fatal\" \"debug\" and so on. Called by MAKE-LOG-LEVEL
function"))

(defgeneric naming-option (package option)
  (:documentation "DEPRECIATED. Use PACKAGE-OPTIONS macro instead "))


(defgeneric package-wrapper (package categories explicit-p)
  (:documentation
   "DEPRECIATED. 

   Allows packages to optionally massage logger names in their
namespace. CATEGORIES will be a list of category names from parent
to child, and EXPLICIT-P will be non-NIL if that list was specified as
an explicit list constant in a logging macro.

Should return (values NEW-CATEGORIES [CAT-LIST-INDEXES])

Where NEW-CATEGORIES should be a new list of categories to use instead
of CATEGORIES.

CAT-LIST-INDEXES should be a list of three numbers (FILE-IDX
PACKAGE-START-IDX PACKAGE-END-IDX) which have the following meaning:

  * FILE-IDX    -- index of the category representing file name (zero based)
  * PACKAGE-IDX -- index of the first and last (exclusive) category representing
  the package.

Based on the above indexes, the pattern layout %g (package) and
%F (file name) and %G (everything else) will be able to return correct
values, on matter where in the package or filename are located in the
category hierarchy.

Default method will first find PACKAGE shortest nickname, then split
it according to category-separator naming option, then return the
values like so:

 (,@<split package> ,*LOGGER-TRUENAME* ,@CATEGORIES)

"))

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
message but will rather expand into a non-NIL value if logging is
enabled on that logger."))

(defgeneric resolve-default-logger-form (package env args)
  (:documentation "Is called by RESOLVE-LOGGER-FORM when logging macro
arguments do not specify the logger to log into. See
RESOLVE-LOGGER-FORM for return values"))

(defgeneric enclosing-scope-block-name (package env)
  (:documentation "Is called by RESOLVE-DEFAULT-LOGGER-FORM to try to
determine the enclosing lexical scope name. For example if logging
macro is being expanded while compiling local function BAR inside of a
definition of function FOO, the implementation of this method should
strive to return '(FOO BAR) if possible.

For CLOS method it is recommended that return value be a generic
function name, followed by optional qualifier, and then followed by
any non-T specializers, with EQL specializers flattened to their
values, for example for the :AROUND method FOO with lambda list
of ((OBJ1 BAR) (OPTION (EQL :BAZ)) OBJ3) should strive to return
'(FOO AROUND BAR BAZ) "))

#-(or sbcl ccl)
(defmethod enclosing-scope-block-name (package env)
  "Default method that always return NIL"
  (declare (ignore package env)))

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
                          (log4cl-error "~s matches more then one log level" arg)
                          (setf match level)))))
           (or match 
               (log4cl-error "~s does not match any log levels" arg))))
        ((and (numberp arg)
              (>= arg 0)
              (<= arg +log-level-unset+))
         arg)
        (t (log4cl-error "~s does not match any log levels" arg))))

(defun instantiate-logger (package categories explicitp createp)
  (let* ((cat-separator (naming-option package :category-separator))
         (cat-case (naming-option package :category-case)))
    (multiple-value-bind (wrapped-categories file pkg-start pkg-end)
        (package-wrapper package categories explicitp)
      (if (and (null categories)
               (not explicitp)
               (pathnamep file)
               (zerop pkg-start)
               (eql pkg-end (length wrapped-categories)))
          ;; when creating a logger for the package, when we have no block name
          ;; but are still instantiated from a file, return the source-file-logger
          (%get-logger (append wrapped-categories (list (namestring file))) cat-separator cat-case nil createp file
                       pkg-start pkg-end t)
          (%get-logger wrapped-categories cat-separator cat-case nil createp file
                       pkg-start pkg-end)))))

(defmethod resolve-default-logger-form (package env args)
  "Returns the logger named after the enclosing lexical environment"
  (values (instantiate-logger
           package
           (enclosing-scope-block-name package env)
           nil t)
          args))

(defun make-package-categories (package)
  "Return package categories split as per package configuration"
  (split-into-categories (if (naming-option package :use-shortest-nickname)
                             (shortest-package-name package)
                             (package-name package))
                         package))

(defmethod package-wrapper (package categories explicit-p)
  "Find the PACKAGES shortest name or nickname, and prefix CATEGORIES
list with it"
  (if explicit-p categories
      (let* ((package-categories (make-package-categories package))
             (file (or *logger-truename*
                       *compile-file-truename*
                       *load-truename*))
             (package-idx-start (when package-categories 0))
             (package-idx-end (when package-categories (length package-categories))))
        (values (append package-categories categories)
         file package-idx-start package-idx-end))))

(defun shortest-package-name (package)
  "Return the shortest name or nickname of the package"
  (let ((name (package-name package)))
    (dolist (nickname (package-nicknames package))
      (when (< (length nickname) (length name))
        (setq name nickname)))
    name))

(defun join-categories (separator list)
  "Return a string with each element of LIST printed separated by
SEPARATOR"
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (s) 
      (loop (if list
                (progn (princ (pop list) s)
                       (when list
                         (princ separator s)))
                (return))))))

(defmethod naming-option (package option)
  "Return default values for naming options which are:
    :CATEGORY-SEPARATOR \":\""
  (flet ((doit () 
           (ecase option
             (:category-separator (%category-separator *naming-configuration*))
             (:category-case (%category-case *naming-configuration*))
             (:expr-print-format (%expr-print-format *naming-configuration*))
             (:use-shortest-nickname (%use-shortest-nickname *naming-configuration*))
             (:expr-log-level (%expr-log-level *naming-configuration*))
             (:old-logging-macros
              (%old-logging-macros *naming-configuration*)))))
    (if *naming-configuration* (doit)
        (with-package-naming-configuration (package) (doit)))))


(defmethod resolve-logger-form (package env args)
  "- When first element of args is NIL or a constant string, calls
 RESOLVE-DEFAULT-LOGGER-FORM that will try to obtain logger name from
 the environment

- When first argument is a :KEYWORD, returns logger named <KEYWORD>

- When first argument is a quoted symbol, returns logger named
  <current-package>.<symbol>

- Otherwise returns the form `(GET-LOGGER ,(FIRST ARGS) ,@(REST ARGS))'"
  ;;
  ;; zzz, new way
  ;;
  
  (assert *naming-configuration*)
  ;; stupid hack, but I don't want to change signature of the generic
  (let ((from-log-expr-p
          (when (eq (first args) 'from-log-expr)
            (pop args)
            t))
        (from-make-logger-p
          (when (eq (first args) 'from-make-logger)
            (pop args)
            t)))
    (if (or (null (%old-logging-macros *naming-configuration*))
            from-log-expr-p
            from-make-logger-p)
        ;; new way
        (let (logger-expr format-expr) 
          (unless (or from-log-expr-p from-make-logger-p) 
            (loop
              (cond
                ((and (not logger-expr) (eq (first args) :logger))
                 (setq logger-expr (check-arg :logger (second args))))
                ((and (not format-expr) (eq (first args) :format-control))
                 (setq format-expr (check-arg :format-control (second args))))
                (t (return)))
              (setq args (cddr args))))
          (when (and (null logger-expr)
                     (not (null args)))
            (cond
              ((keywordp (first args))
               (setq logger-expr
                     (instantiate-logger package
                                         (split-into-categories
                                          (symbol-name (first args)) package)
                                         nil t)
                     args (rest args)))
              ((constantp (first args))
               (let ((value (eval (first args))))
                 (cond ((symbolp value)
                        (setq logger-expr
                              (instantiate-logger
                               package
                               (split-into-categories (symbol-name value) package)
                               nil t)
                              args (rest args)))
                       ((listp value)
                        (setq logger-expr (instantiate-logger package value t t)
                              args  (rest args))))))))
          (when (and (null logger-expr)
                     from-make-logger-p
                     (not (null args)))
            (assert (null (rest args)))
            (setq logger-expr (first args)))
          (or logger-expr (setq logger-expr
                                (instantiate-logger
                                 package
                                 (enclosing-scope-block-name package env)
                                 nil t)))
          (assert logger-expr)
          (cond ((not (null format-expr))
                 (values logger-expr `(,format-expr ,@args)))
                ((null args) logger-expr)
                ((and (not from-log-expr-p)
                      (stringp (first args))
                      (position #\~ (first args)))
                 (values logger-expr args))
                (t (when (and (cddr args)
                              (typep (first args) '(or symbol list))
                              (stringp (second args))
                              (position #\~ (second args)))
                     (log4cl-style-warning "Tilde in second argument, missing :LOGGER?"))
                   (values logger-expr (make-log-expr-format args))))) 
        ;; old way
        (cond
          ((or (null args)
               (stringp (first args)))
           (resolve-default-logger-form package env args))
          ((keywordp (first args))
           (values (instantiate-logger package
                                       (split-into-categories
                                        (symbol-name (first args)) package)
                                       nil t)
                   (rest args)))
          ((constantp (first args))
           (let ((value (eval (first args))))
             (cond ((symbolp value)
                    (values (instantiate-logger
                             package
                             (split-into-categories (symbol-name value) package)
                             nil t)
                            (rest args)))
                   ((listp value)
                    (values (instantiate-logger package value t t)
                            (rest args)))
                   (t (values (first args) (rest args))))))
          (t
           (values (first args) (rest args)))))))

(defun make-log-expr-format (args)
  "Implement the parsing for (log:expr) arguments. Should return the
list of arguments to FORMAT starting with control string"
  (assert *naming-configuration*)
  (let* ((expr-format (%expr-print-format *naming-configuration*))
         fmt-string fmt-args arg)
    (setq
     fmt-string
     (with-output-to-string (*standard-output*)
       (loop
         (if (null args) (return)
             (setq arg (first args)
                   args (rest args)))
         (cond ((stringp arg)
                ;; trim spaces
                (let* ((n1 (position #\Space arg :test-not #'char=))
                       (n2 (position #\Space arg :test-not #'char=
                                                 :from-end t))) 
                  (when n1 (setq arg (substr arg n1 (1+ n2)))) 
                  (cond ((position #\~ arg)
                         (loop for c :of-type character across arg
                               if (char= c #\~)
                               do (write-string "~~")
                               else do (write-char c)))
                        (t (write-string arg)))
                  ;; Don't add extra space after literal if current or
                  ;; next arg is all whitespace literal
                  (unless (or (not n1)    ; current arg is whitespace
                              (null args) ; last arg
                              ;; next arg is whitespace or empty string
                              (and (stringp (first args))
                                   (null (position #\Space (first args)
                                                   :test-not #'char=)))) 
                    (princ #\Space))))
               ((search "~:>" expr-format)
                ;; if expression format is pretty-print logical block
                ;; put arguments into a list
                (princ expr-format)
                (push `(list (quote ,arg) ,arg) fmt-args))
               (t (princ expr-format)
                  (push `(quote ,arg) fmt-args)
                  (push arg fmt-args))))))
    (cons fmt-string (nreverse fmt-args))))

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


(defun fix-method-spec-list (spec)
  "Flatten method specializer list, remove any T specializers, replace
all constant EQL specializers with their values, and eliminate
non-constant ones"
  (let (list)
    (labels ((traverse (spec)
               (cond
                 ((null spec))
                 ((and (consp spec)
                       (eq 'eql (first spec))
                       (endp (cddr spec)))
                  (when (constantp (second spec)) 
                    (let ((result (eval (second spec))))
                      (when (atom result)
                        (push result list)))))
                 ((consp spec)
                  (traverse (first spec))
                  (traverse (rest spec)))
                 ((eq spec T))
                 ((typep spec 'built-in-class)
                  (traverse (class-name spec)))
                 #+ccl ((typep spec 'ccl:eql-specializer)
                        (traverse (ccl:eql-specializer-object spec)))
                 ((typep spec 'class)
                  (traverse (class-name spec)))
                 ((atom spec)
                  (push spec list)))))
      (traverse spec)
      (nreverse list))))
