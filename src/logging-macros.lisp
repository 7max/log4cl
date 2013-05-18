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

(defmacro with-log-indent ((&optional (indent '(1+ *log-indent*)))
                           &body body)
  "Executes forms in BODY with *LOG-INDENT* set to INDENT"
  `(let ((*log-indent* ,indent))
     ,@body))

(defmacro deflog-macros (levels)
  (let (list)
    (dolist (level levels)
      (let ((log-macro-name (intern (format nil "~a-~a"
                                            (string '#:log)
                                            (string level))))
            (level-name (intern (format nil "+~a-~a+"
                                        (string '#:log-level)
                                        (string level)))))
        (push `(defmacro ,log-macro-name (&rest args &environment env)
                 "
Submit log message to the logging system. Whenever
the message is actually displayed or not depends on logging system
configuration at run-time.

The ARGS are parsed as follows:

1. Determine a logger object

If first argument is a constant list, constant symbol or a keyword, it
is made into a logger object as described in the MAKE-LOGGER macro
documentation

If first argument is a constant string, logger name is auto-determined
from context as described in the MAKE-LOGGER macro documentation, and
system proceeds to step 2.

Otherwise any non-NIL first argument is assumed to be a form, that
when evaluated will return a logger object.

2. If there are remaining arguments, they are used as format control
string and format arguments, to be passed into the FORMAT function to
produce the log message, when one is produced.

If there were no other arguments, then this macro expands into a form,
that will return T or NIL depending if logging with specified log
level will actually produce any log messages. Note that having log
level enabled does not necessary mean logging with log level is
enabled, it also takes into account whenever log message will reach
any appenders.
"
                 (expand-log-with-level env ,level-name args))
              list)))
    `(progn
       ,@(nreverse list))))

(deflog-macros #.+log-level-macro-symbols+)

(defvar +make-logger-symbols+ '(make-logger))

(defmacro log-sexp-with-level (level &rest sexps &environment env) 
  "Expands into LOG-<LEVEL> log statement that will print each element
of SEXPS in the form of ELEMENT=VALUE where ELEMENT will be the
literal argument without evaluating it, and VALUE will be the result
of evaluation. Constant string elements will be output directly.

A pretty printer (pprint-newline :fill) format will be used as a
separator between expressions, so that long expressions start
on a new line, if *PRINT-PRETTY* is non-NIL

Example:

     (let ((a 1) (b '(two three))
           (c (loop for i from 1 to 15 collect i)))
       (log:sexp \"values are\" a b c))

will produce log message:

    [debug] - values are A=1 B=(TWO THREE)
              C=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)

Separator between expression and value, which defaults to equal sign,
and a suffix after each value, which defaults to \" ~:_\" (a space
followed by conditional newline) can be customized per package via
NAMING-OPTION generic function
"
  (declare (ignore env))
  (with-package-naming-configuration (*package*) 
    `(,level from-log-expr ,@sexps)))

(defmacro deflog-sexp-macros (levels)
  (let (list)
    (dolist (level levels)
      (let ((log-sexp-macro-name (intern (format nil "~a-~a"
                                                 (string '#:log-sexp)
                                                 (string level))))
            (log-macro-name (intern (format nil "~a-~a"
                                            (string '#:log)
                                            (string level)))))
        (push `(defmacro ,log-sexp-macro-name (&rest args)
                 "Expands into the log statement that will print each
element of ARGS in the form of ELEMENT=VALUE where ELEMENT will be the
literal argument without evaluating it, and VALUE will be the result
of evaluation. For constant string elements, it is output literally
without printing its value.

Example:

     (let ((a 1) (b '(two three)))  
       (log-sexp \"values are\" a b))

will produce log message:

    [debug] - values are A=1 B=(TWO THREE)

       "
                 `(log-sexp-with-level ,',log-macro-name ,@args))
              list)))
    `(progn ,@(reverse list))))

(deflog-sexp-macros #.+log-level-macro-symbols+)

(defmacro log-sexp (&rest args)
  (with-package-naming-configuration (*package*) 
    (let* ((level (naming-option *package* :expr-log-level))
           (log-sexp-macro-name (intern (format nil "~a-~a"
                                                (string '#:log-sexp)
                                                (string (aref +log-level-to-keyword+ level)))
                                        :log4cl-impl)))
      `(,log-sexp-macro-name ,@args))))

(setf (documentation 'log-sexp 'function) (documentation 'log-sexp-debug 'function))

(defmacro with-log-hierarchy ((hierarchy) &body body)
  "Binds the *CURRENT-HIERARCHY* to the specified hierarchy for the
dynamic scope of BODY. HIERARCHY can be hierarchy index or name"
  `(let ((*hierarchy* (hierarchy-index ,hierarchy)))
     ,@body))

(defmacro with-package-log-hierarchy (&body body)
  "Binds the *CURRENT-HIERARCHY* to the unique hierarchy for the current
package for the dynamic scope of BODY."
  `(with-log-hierarchy (*package*) ,@body))

(defmacro in-log-hierarchy (&optional hierarchy)
  "Sets the *CURRENT-HIERARCHY* to specified hierarchy, or the default
  one when NIL"
  `(setq *hierarchy* (hierarchy-index (or ,hierarchy :default))))

(defmacro in-package-log-hierarchy ()
  "Sets the *CURRENT-HIERARCHY* to specified hierarchy, or the default
  one when NIL"
  `(in-log-hierarchy *package*))

(defmacro make-logger (&optional (arg nil arg-p) &environment env)
  (with-package-naming-configuration (*package*) 
    (resolve-logger-form *package* env (if arg-p `(from-make-logger ,arg)))))

(defmacro with-ndc ((&optional (ndc nil ndcp)) &body body)
  "Execute forms in BODY with *NDC-CONTEXT* set to CONTEXT. The
context is printed by the %x pattern layout format"
  (if ndcp 
      `(let ((*ndc-context* ,ndc))
         ,@body)
      `(progv '(*ndc-context*)
           ()
         ,@body)))

