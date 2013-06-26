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


(defclass pattern-layout (layout)
  ((%pattern :initarg :conversion-pattern :accessor conversion-pattern)
   (%formatter))
  (:documentation
   "Pattern layout uses a configurable conversion pattern to format log
messages. For example, the following patterns produce these log
messages:

\"%-5p [%c] - %m%n\" produces the message

 INFO [CATEGORY.SUBCATEGORY.NAME] - message text

and \"%-5p [%c{2}{:invert}{--}] - %m%n\" produces the message:

 INFO [subcategory--name] - message text

Syntax of conversion pattern is: %[<FLAGS>]<PATTERN-CHAR>[{<EXTRA-ARG>}...]

FLAGS are consists of:
  [:][;<PREFIX>;][;<SUFFIX>;][-]<MIN>.<MAX>

If a string resulting from the pattern expansion is longer then MAX
its truncated by omitting characters from the start. Then if its
shorter then MIN its padded with spaces to the right or left (when
preceded by a minus)

If PREFIX and/or SUFFIX is specified, then string is wrapped with them
and all padding for justification, is done before the prefix and after
the suffix. The length of prefix and suffix are not included into
MIN/MAX or/padding calculations.

Example: %-7;<;;>;p with p expanding to INFO, will produce the string
\" <INFO>\"

If : flag is specified, and string is empty, then no output is made, including
not outputting any prefix or suffix.

Performance considerations: All formatting is done without consing,
except for the case of %m (user message) format, when it has MIN or
MAX field width flags. That is because user message is passed around
as a lambda that writes to the stream, and in order for its length to
be known, (with-output-to-string) is used.

Following pattern characters are recognized:

--------------------------------------------------------------------
   %p Log level string, for example DEBUG, INFO, TRACE. 

   %P Log level string in lower case

   %c Full category name of the logger for example CL-USER:FOO:BAR.

   There can be up to three extra flags in curly braces. Extra flag
   can be a set of empty curly braces, in this case the effect is same
   as default value.

   First extra flag is PRECISION. When a single integer its a number
   of categories to display, parent categories will be omitted if
   logger is more then N levels deep.

   For example if logger category name is CL-USER:ONE:TWO:THREE then
   conversion pattern %c{2} will produce TWO:THREE

   When PRECISION is specified as {<FROM>,<COUNT>}, then it means
   categories starting at FROM (zero-based) and output <COUNT>
   categories. Negative or zero means until end of the categories.

   Example: Category printed for a logger named CL-USER:ONE:TWO:THREE

   Precision | Result
   ----------|----------------
   {1}       | THREE
   {2}       | TWO:THREE
   {0,1}     | CL-USER
   {5,1}     | 
   {1,1}     | ONE
   {1,2}     | ONE:TWO
   {1,0}     | ONE:TWO:THREE
   {1,100}   | ONE:TWO:THREE

   Second extra argument is SEPARATOR and will be a separator used
   between category names. If not present then the loggers native
   value will be used, which can be overwritten per package by
   NAMING-OPTION method

   Third extra argument if present, can be one of :UPCASE, :DOWNCASE or
   :INVERT and will result in printing of the category name in the
   corresponding manner, similar to using non-standard readtable-case

   For example when logger category is CL-USER.FOO.BAR outputting it
   with conversion pattern of %c{}{--}{:invert} will result print it
   as cl-user--foo--bar
--------------------------------------------------------------------
   %g - like to %c, but only portion of the categories that represent
   the package name
   
   %C - like to %c, but only portion of the categories that are not
   the package name.

   Example: assuming category separator setup for the package was a
   dot, and a (log:logger :one.two.three) was instantiated in package
   cl.dotted.package:

     %g                    => CL.DOTTED.PACKAGE
     %g{}{--}{:downcase}   => cl--dotted--package
     %C                    => ONE.TWO.THREE
     (%C{}{ }{:downcase})  => (one two three)

   %F namestring of a file where logger was instantiated, same as
      returned by LOGGER-FILE-NAMESTRING
--------------------------------------------------------------------
   %d The date/time of the log message in UTC, extra argument
      can be a date pattern. Default date pattern is
      %d{%Y-%m-%d %H:%M:%S}

      To facilitate testing there can be optional second extra
      argument, which could be decimal value that will be used as
      universal time instead calling (GET-UNIVERSAL-TIME)

      Lisp does not have portable facility to get week and month
      names, so date format is printed by PATTERN-LAYOUT-FORMAT-DATE
      generic function, default method of which uses hard-coded
      English week/month/weekday names.

      Valid date format values are same as C strftime function, with
      GNU extensions.

        %A -- Full weekday name
        %A -- Abbreviated weekday name
        %B -- Full month name
        %b -- Abbreviated month name
        %c -- Standard date and time string
        %d -- Day of month as a decimal(01-31)
        %H -- Hour(00-23)
        %I -- Hour(01-12)
        %m -- Month as decimal(01-12)
        %M -- Minute as decimal(00-59)
        %p -- AM or PM
        %P -- am or pm
        %S -- Second as decimal(00-59)
        %y -- Year in decimal without century(0-99)
        %Y -- Year including century as decimal
        %z -- Time zone offset from UTC in -hhmm or +hhmm format
        %% -- The percent sign
--------------------------------------------------------------------
   %D date-time in local time, extra arguments can contain a strftime pattern

   %h hostname of the system (implementation dependent, obtained once
   when pattern is parsed, and cached

   %t Current thread name

   %x Value of *ndc-context* variable from (with-ndc (context)) macro

   %i Process id of the lisp process, implementation dependent.

   %I Two spaces repeated *log-indent* times. Different padding string
   can be specified in an extra argument.

   %n Mandatory newline, issues (TERPRI) on the stream. %:n issues (TERPRI) if
   *PRINT-PRETTY* is NIL or (PPRINT-NEWLINE :MANDATORY) otherwise

   %& Optional newline, issues FRESH-LINE on the stream

   %m Actual user log message.

--------------------------------------------------------------------

   PRETTY-PRINTER CONTROL

   %< and %> the formatting inside is wrapped into
   PPRINT-LOGICAL-BLOCK. Whenever pretty printing is actually used
   depends on runtime value of *PRINT-PRETTY* at call site

   The opening pattern can have extra arguments, with following
   meaning:

     %<{pretty}[{<num>}]  - bind *PRINT-PRETTY* to T at runtime,if followed
                            by a number, set *print-right-margin* to it
     %<{nopretty}         - bind *PRINT-PRETTY* to NIL at runtime
     %<{package}          - bind *PACKAGE* to :KEYWORD package
     %<{nopackage}        - bind *PACKAGE* to original package

   Both pretty and package can be used together like this %<{pretty}{package} ... %>

   %_ conditional newline, issues (PPRINT-NEWLINE :linear)
   %:_ conditional newline, issues (PPRINT-NEWLINE :fill)

   %[<n>]N does (PPRINT-INDENT :block n). 
   %:[<n>]N does (PPRINT-INDENT :current n).

   %<n1>.<n2>N is similar to above but the second argument of
   PPRINT-INDENT is calculated as (+ n1 (* n2 *LOG-INDENT*)).

   For example with %5.2N the wrapped lines will be indented
   default %I amount, plus extra 5 spaces.
"))

(defmethod property-alist ((instance pattern-layout))
  (append (call-next-method)
          '((:conversion-pattern %pattern string))))

(defvar *formatters* (make-hash-table))

(defmacro define-pattern-formatter ((char &optional stopchar) &body body)
  "Define a pattern formatter function, with the signature of

\(LAMBDA (STREAM FMT-INFO LOGGER LOG-LEVEL LOG-FUNC)\)

Arguments:

STREAM    - stream to print stuff to
FMT-INFO  - instance of FORMAT-INFO
LOGGER    - the logger category that event came from
LOG-LEVEL - log level of the message
LOG-FUNC  - user log function that outputs actual log message

When STOPCHAR is specified, the pattern format will be parsed until
%<STOPCHAR> is encountered, and the formatting function that outputs
everything in between will be passed as the extra argument to the
formatter, making the signature

\(LAMBDA (STREAM FMT-INFO LOGGER LOG-LEVEL LOG-FUNC WRAPPED-FORMATTER)\)

The WRAPPED-FORMATTER will be a function with the same signature as regular
non-wrapped formatter.

This second form allows writing formatters that establish the dynamic
context for the pattern inside, for example %< %> formatter that wraps
everything inside into PPRINT-LOGICAL-BLOCK is implemented this way.
"
  (if (null stopchar)
      `(setf (gethash ,char *formatters*)
             (lambda (stream fmt-info logger log-level log-func)
               ,@body))
      `(setf (gethash ,char *formatters*)
             (list (lambda (stream fmt-info logger log-level log-func wrap)
                     ,@body)
                   ,stopchar))))

(defmethod shared-initialize :after ((layout pattern-layout) slots &key conversion-pattern)
  (declare (ignore slots))
  (compile-pattern layout conversion-pattern))

(defmethod (setf conversion-pattern) :after (pattern (layout pattern-layout))
  (compile-pattern layout pattern))

(defmethod layout-to-stream ((layout pattern-layout)
			     stream
                             logger
			     level
                             log-func)
  "Format the log statement with the pattern layout"
  (declare (type stream stream)
           (type fixnum level)
           (type function log-func))
  (with-slots (%formatter)
      layout
    (funcall %formatter stream logger level log-func))
  (values))


(defclass format-info ()
  ((conversion-char :initarg :conversion-char :type character
                    :reader format-conversion-char)
   (minlen :initform 0 :initarg :minlen :type fixnum :reader format-min-len)
   (maxlen :initform nil :initarg :maxlen :type (or null fixnum)
           :reader format-max-len)
   (right-justify :initform nil :initarg :right-justify :type boolean
                  :reader format-right-justify)
   (prefix :initform nil :initarg :prefix :reader format-prefix
           :type (or null simple-string))
   (suffix :initform nil :initarg :suffix :reader format-suffix
                  :type (or null simple-string))
   (colon-flag :initform nil :initarg :colon-flag :reader format-colon-flag
               :type boolean)
   (at-flag :initform nil :initarg :at-flag :reader format-at-flag
               :type boolean))
  (:documentation "Represents data for a single conversion pattern"))

(defgeneric parse-extra-args (fmt-info pattern-char pattern-string start)
  (:documentation "Should parse extra arguments after the end of the
conversion character in the PATTERN-STRING and return next parse
position. The START is the index of the first character in the
conversion pattern after the PATTERN-CHAR, ie in the string
%-5p{foobar} it will be at the opening curly brace.

Should return two values, new parse position and either FMT-INFO or
its subclass."))

(define-condition pattern-layout-error (log4cl-error parse-error)
  ())

(defun pattern-layout-error (message &rest args)
  (error 'pattern-layout-error :format-control message :format-arguments args))

(defun parse-extra-args-in-curly-braces (pattern start)
  "While next character at START is curly brace, collect anything
between it and a closing curly brace into a string.

For the set of empty curly braces, collects NIL.

Return a list, with first element being new parse position, and rest
being extra arguments collected

Example: For the string {one}{}{three} will return the list (14
\"one\" NIL \"three\")"
  (loop with brace-pos
        with closedp
        with arg
        while (and (< start (length pattern))
                   (char= (aref pattern start) #\{))
        do (setq
            brace-pos start
            closedp nil
            arg (with-output-to-string (s)
                  (incf start)
                  (loop with c
                        while (< start (length pattern))
                        do (setq c (char pattern start))
                        do (incf start)
                        do (cond ((char= c #\}) (setq closedp t) (return))
                                 ((char= c #\\)
                                  (when (< start (length pattern))
                                    (write-char (aref pattern start) s)
                                    (incf start)))
                                 (t (write-char c s))))
                  (unless closedp
                    (pattern-layout-error
                     "~@<Unmatched brace at position ~d of conversion pattern ~_~S~:>"
                     brace-pos pattern))))
        collect (when (plusp (length arg))
                  (coerce arg 'simple-string))
        into extra-args
        finally (return (cons start extra-args))))

(defun format-string (string stream info
                      &optional (start 0) (end (length string)))
  "Write STRING to the STREAM, respecting flags in info"
  (declare (type string string)
           (type stream stream)
           (type format-info info)
           (type fixnum start end))
  (let* ((min (format-min-len info))
         (len (- end start))
         (max (or (format-max-len info) len)))
    (declare (type fixnum min max len))
    (when (< max len)
      (incf start (- len max))
      (decf len (- len max)))
    (cond
      ((and (zerop len) (format-colon-flag info)))
      ((< len min) 
       (cond ((not (format-right-justify info))
              (when (format-prefix info)
                (write-string (format-prefix info) stream))
              (write-string string stream :start start :end end)
              (when (format-suffix info)
                (write-string (format-suffix info) stream))
              (loop repeat (- min len)
                    do (write-char #\Space stream)))
             (t (loop repeat (- min len)
                      do (write-char #\Space stream))
                (when (format-prefix info)
                  (write-string (format-prefix info) stream))
                (write-string string stream :start start :end end)
                (when (format-suffix info)
                  (write-string (format-suffix info) stream)))))
      (t
       (when (format-prefix info)
         (write-string (format-prefix info) stream))
       (when (plusp len) 
         (write-string string stream :start start :end end))
       (when (format-suffix info)
           (write-string (format-suffix info) stream)))))
  (values)) 

(define-pattern-formatter (#\p) 
  "Output the %p (log level) pattern"
  (declare (ignore logger log-func))
  (format-string (log-level-to-string log-level) stream fmt-info)
  (values))

(define-pattern-formatter (#\P) 
  "Output the %P (log level) pattern"
  (declare (ignore logger log-func))
  (format-string (log-level-to-lc-string log-level) stream fmt-info)
  (values))

(defmethod parse-extra-args (fmt-info character pattern start)
  "Default method does not parse any extra arguments and returns INFO
unchanged"
  (declare (ignore character pattern))
  (values start fmt-info))

(defclass pattern-category-format-info (format-info)
  ((precision :initarg :precision :reader format-precision)
   (start :initarg :start :reader format-start)
   (separator :initarg :separator :reader format-separator)
   (case :initarg :case :reader format-case))
  (:documentation "Extra formatting flags for %c (log category) pattern"))

(defun parse-category-precision (string)
  (if (or (null string) (zerop (length string))) 1000
      (multiple-value-bind (n pos)
          (parse-integer string :junk-allowed t)
        (cond ((= pos (length string))
               n)
              ((char= #\, (char string pos))
               (values (parse-integer string :start (1+ pos)) n))
              (t (error "Junk after the number at position ~d" pos))))))

(defun kw= (arg what)
  (when (stringp arg)
    (when (plusp (length arg))
      (when (char= #\: (char arg 0)) 
        (setq arg (substr arg 1))))
    (equalp arg what)))

(defun parse-category-extra-args (fmt-info char pattern start)
  (declare (ignore char))
  (destructuring-bind (next-pos &optional precision separator case)
      (parse-extra-args-in-curly-braces pattern start)
    (multiple-value-bind (precision start)
        (handler-case
            (parse-category-precision precision)
          (error (err)
            (pattern-layout-error
             "~@<Invalid %c precision ~_~s: ~_~a~:>" precision err)))
      (values next-pos
              (change-class
               fmt-info 'pattern-category-format-info
               :precision precision
               :start start
               :separator separator
               :case (cond
                       ((null case) nil)
                       ((kw= case "upcase") :upcase)
                       ((kw= case "downcase") :downcase)
                       ((kw= case "invert") :invert)
                       ((kw= case "preserve") nil)
                       (t (pattern-layout-error
                           "~@<Invalid 3rd extra argument ~s ~:_around ~
                                 position ~d in conversion pattern ~:_~s~:>"
                           case start pattern))))))))

(defmethod parse-extra-args (fmt-info (char (eql #\c))
                             pattern start)
  (parse-category-extra-args fmt-info char pattern start))

(defmethod parse-extra-args (fmt-info (char (eql #\C))
                             pattern start)
  (parse-category-extra-args fmt-info char pattern start))

(defmethod parse-extra-args (fmt-info (char (eql #\g))
                             pattern start)
  (parse-category-extra-args fmt-info char pattern start))

(defmethod parse-extra-args (fmt-info (char (eql #\G))
                             pattern start)
  (parse-category-extra-args fmt-info char pattern start))

(defclass pattern-date-format-info (format-info)
  ((date-format :initarg :date-format :reader format-date-format)
   (universal-time :initarg :universal-time :reader format-universal-time)
   (utc-p :initarg :utc-p :reader format-utc-p))
  (:documentation "Extra formatting flags for %d and %D patterns"))


(defparameter +default-local-date-format+ "%Y-%m-%d %H:%M:%S")

(defun parse-date-format-extra-args (fmt-info utc-p pattern start)
  (destructuring-bind (next-pos &optional date-format time)
      (parse-extra-args-in-curly-braces pattern start)
    (values next-pos
            (change-class fmt-info 'pattern-date-format-info
                          :date-format (or date-format +default-local-date-format+)
                          :utc-p utc-p
                          :universal-time
                          (when time
                            (handler-case (parse-integer time)
                              (error (e)
                                (pattern-layout-error
                                 "Invalid universal time in 2nd extra argument ~
                                  around position ~d of pattern ~s:  ~a"
                                 start pattern e))))))))

(defmethod parse-extra-args (fmt-info (char (eql #\d)) pattern start)
  (parse-date-format-extra-args fmt-info t pattern start))

(defmethod parse-extra-args (fmt-info (char (eql #\D)) pattern start)
  (parse-date-format-extra-args fmt-info nil pattern start))

(defun format-categories (stream fmt-info cats num-cats separator)
  (declare (type pattern-category-format-info fmt-info) 
           (type stream stream)
           (type simple-vector cats)
           (type simple-string separator)
           (type fixnum num-cats))
  (let* ((precision (format-precision fmt-info))
         (case (format-case fmt-info))
         (minlen (format-min-len fmt-info))
         (maxlen (format-max-len fmt-info))
         (right-justify (format-right-justify fmt-info))
         (fmt-start (format-start fmt-info))
         (start 0)
         (end num-cats))
    (declare (type fixnum start end))
    (if fmt-start
        (progn
          (setq start fmt-start)
          (if (and precision (plusp precision)) (setq end (min num-cats (+ start precision)))))
        (if precision
            (setq start (max 0 (- num-cats precision)))))
    ;; make end inclusive
    (decf end)
    ;; (format t "here1 fmt-start ~d precision ~d start ~d end ~d num-cats ~d cats ~s ~%"
    ;;         fmt-start precision start end num-cats cats)
    (let ((field-len 0)
          (padlen 0)
          (skip-at-start 0))
      (declare (type fixnum field-len skip-at-start padlen))
      ;; sum logger names length
      (loop for i fixnum from start to end
            as logger = (svref cats (- num-cats i 1))
            do (incf field-len (logger-name-length logger)))
      ;; add length of separators between loggers
      (if (< start end)
          (incf field-len (* (length separator) (- end start))))
      (setq skip-at-start
            (max 0 (- field-len
                      (or maxlen field-len))))
      (decf field-len skip-at-start)
      (setq padlen (- minlen field-len))
      (labels
          ((write-string-or-skip (string start end case)
             (let* ((len (- end start))
                    (skip (min len skip-at-start)))
               (decf skip-at-start skip)
               (when (< (incf start skip) end)
                 (write-string-modify-case
                  string stream case start end))))
           (doit ()
             (when (format-prefix fmt-info)
               (write-string (format-prefix fmt-info) stream))
             (loop for i fixnum from start to end
                   as logger = (svref cats (- num-cats i 1))
                   ;; do (format t "doing logger ~s ~s ~s ~%" logger (logger-name logger) (%logger-name-start-pos logger))
                   do (write-string-or-skip (%logger-category logger)
                                            (%logger-name-start-pos logger)
                                            (length (%logger-category logger))
                                            case)
                   if (/= i end) 
                   do (write-string-or-skip separator 0
                                            (length separator) nil))
             (when (format-suffix fmt-info)
               (write-string (format-suffix fmt-info) stream)))
           (pad ()
             (loop repeat padlen do (write-char #\Space stream))))
        (cond ((and (plusp padlen) right-justify)
               (pad) (doit))
              ((plusp padlen)
               (doit) (pad))
              (t (doit))))))
  (values))

(defun simple-format-catogories (stream fmt-info logger start-depth end-depth)
  (declare (type pattern-category-format-info fmt-info) 
           (type stream stream)
           (type logger logger)
           (type logger-cat-idx start-depth end-depth))
  (let* ((precision (format-precision fmt-info))
         (fmt-start (format-start fmt-info)))
    ;; (format t "here0 start-depth ~d end-depth ~d start ~d precision ~d
    ;; logger ~s logger-depth ~s~%"
    ;;         start-depth end-depth
    ;;         start precision
    ;;         logger (logger-depth logger))
    (if fmt-start
        (progn
          (setq start-depth (+ start-depth fmt-start))
          (if (plusp precision) (setq end-depth (min end-depth (+ start-depth precision))))
          ;; (format t "here1 start ~d end  ~d ~%" start-depth end-depth)
          )
        (if precision
            (setq start-depth (max start-depth (- end-depth precision))))) 
    (let ((start nil)
          (end nil)
          (cat (%logger-category logger)))
      (declare (type (or fixnum null) start end))
      (loop while (< end-depth (logger-depth logger))
            do (setq end (1- (%logger-name-start-pos logger))
                     logger (%logger-parent logger)))
      (loop while (< start-depth (logger-depth logger))
            do (setq start (%logger-name-start-pos logger)
                     logger (%logger-parent logger)))
      (if start (format-string cat stream fmt-info
                               start (or end (length cat)))
          (format-string "" stream fmt-info)))))

(defmacro with-small-dynamic-extent-vector ((name len len-expr &optional (limit 32))
                                            &body body)
  (let ((foo (gensym)))
    `(flet ((,foo (,name ,len) 
              (declare (type simple-vector ,name)
                       (type fixnum ,len))
              ,@body))
       (declare (dynamic-extent #',foo))
       (locally (declare (optimize (speed 3) (space 3)))
         (let ((,len ,len-expr))
           (declare (type fixnum ,len))
           (if (<= ,len ,limit)
               (let ((,name (make-array ,limit)))
                 (declare (dynamic-extent ,name))
                 (,foo ,name ,len))
               (let ((,name (make-array ,len)))
                 (declare (dynamic-extent ,name))
                 (,foo ,name ,len))))))))


(defun format-categories-range (stream fmt-info logger start-depth end-depth)
  (declare (type pattern-category-format-info fmt-info) 
           (type stream stream)
           (type logger logger)
           (type logger-cat-idx start-depth end-depth))
  (let ((ccase (slot-value fmt-info 'case))
        (sep (slot-value fmt-info 'separator)))
    (if (or sep ccase)
        (with-small-dynamic-extent-vector
            (cats num-cats (- end-depth start-depth))
          (declare (ignore num-cats))
          (let ((cnt 0))
            (declare (type fixnum cnt)) 
            (loop with lgr = logger
                  for i fixnum downfrom (1- (logger-depth logger))
                  to start-depth
                  do (progn
                       ;; (format t "here1 i = ~d logger = ~s file-idx = ~s ~%" i lgr file-idx)
                       (when (< i end-depth)
                         (setf (svref cats cnt) lgr)
                         (incf cnt))
                       (setf lgr (%logger-parent lgr)))) 
            (format-categories stream fmt-info cats cnt (or sep (%logger-category-separator logger)))))
        (simple-format-catogories stream fmt-info logger
                                  start-depth end-depth)))
  (values))

(defun adjusted-logger-depth (logger)
  (if (typep logger 'source-file-logger)
      (1- (logger-depth logger))
      (logger-depth logger)))

(declaim (inline adjusted-logger-depth))

(define-pattern-formatter (#\c)
  "Format the %c (full log category) pattern"
  (declare (ignore log-level log-func))
  (format-categories-range stream fmt-info logger 0
                           (adjusted-logger-depth logger))
  (values))

(define-pattern-formatter (#\C)
  "Format the %C (log category excluding package) pattern"
  (declare (ignore log-level log-func))
  (let* ((start-depth (logger-pkg-idx-start logger))
         (end-depth (logger-pkg-idx-end logger)))
    (cond ((zerop start-depth) 
           (if (typep logger 'source-file-logger) 
               (format-string "" stream fmt-info)
               ;; logger has no package info, format entire thing
               (format-categories-range stream fmt-info logger 0
                                        (logger-depth logger))))
          ((= 1 start-depth)
           (if (typep logger 'source-file-logger) 
               (format-string "" stream fmt-info) 
               ;; package is the prefix, so start from end of the package
               (format-categories-range stream fmt-info logger (1- end-depth)
                                        (logger-depth logger))))
          (t
           ;; weird case that is possible by achieve by overloading
           ;; PACKAGE-WRAPPER method, where categories representing
           ;; package name can be in the middle, in which case
           ;; we have to skip them
           (let ((sep (slot-value fmt-info 'separator)))
             (with-small-dynamic-extent-vector
                 (cats num-cats (- end-depth start-depth))
               (declare (ignore num-cats))
               (let ((cnt 0))
                 (declare (type fixnum cnt)) 
                 (loop with lgr = logger
                       for i fixnum downfrom (1- (logger-depth logger))
                       to 0
                       do (progn
                            ;; (format t "here1 i = ~d logger = ~s file-idx = ~s ~%" i lgr file-idx)
                            (unless (< (1- start-depth) i (1- end-depth))
                              (setf (svref cats cnt) lgr)
                              (incf cnt))
                            (setf lgr (%logger-parent lgr)))) 
                 (format-categories stream fmt-info cats cnt
                                    (or sep (%logger-category-separator logger)))))))))
  (values))

(define-pattern-formatter (#\g)
  "Format the %g (log category representing the package) pattern"
  (declare (ignore log-level log-func))
  (let* ((start-depth (logger-pkg-idx-start logger))
         (end-depth (logger-pkg-idx-end logger)))
    (if (zerop start-depth)
        (simple-format-catogories stream fmt-info logger 0 0)
        (format-categories-range stream fmt-info logger (1- start-depth) (1- end-depth))))
  (values))

(define-pattern-formatter (#\F)
  (declare (ignore log-func log-level))
  (let ((name (logger-file-namestring logger))) 
    (format-string (or name "") stream fmt-info)))

(define-pattern-formatter (#\m)
  "Output the %m pattern layout info"
  (declare (ignore logger log-level))
  (if (or (plusp (slot-value fmt-info 'minlen))
          (slot-value fmt-info 'maxlen))
      ;; need to know length of message produced by log-func
      (format-string (with-output-to-string (s)
                       (call-user-log-message log-func s))
                     stream fmt-info)
      (call-user-log-message log-func stream))
  (values))

(defun compile-pattern-format (pattern
                               &optional (idx 0)
                                         stopchar)
  "Par
ses the pattern format and returns a function with lambda-list
of (STREAM LOGGER LOG-LEVEL LOG-FUNC) that when called will output
the log message to the stream with the specified format."
  (let ((str (make-array 0 :element-type 'character
                           :adjustable t
                           :fill-pointer t))
        (c #\Space) (fm-list '()) (state :normal)
        (minlen 0) (maxlen nil) (right-justify nil)
        (start-idx idx)
        (colon-flag nil)
        (at-flag nil)
        (prefix nil)
        (suffix nil))
    (declare (type fixnum idx)
             (type character c))
    (labels ((next ()
               (when (< idx (length pattern))
                 (prog1 (setq c (char pattern idx))
                   (incf idx))))
             (add-char (c)
               (vector-push-extend c str))
             (signal-error (msg &optional (pos (1- idx)))
               (pattern-layout-error "~a at position ~d of pattern ~s"
                                     msg pos pattern))
             (next-or-error (msg)
               (or (next) (signal-error msg idx)))
             (add-formatter (fm &optional fm-info)
               (push (cons fm fm-info) fm-list))
             (add-literal ()
               (when (plusp (length str))
                 (let ((literal (coerce str 'simple-string)))
                   (add-formatter
                    (lambda (stream format logger level log-func)
                      (declare (ignore format logger level log-func))
                      (write-string literal stream))))
                 (setf (fill-pointer str) 0))))
      (loop
        (ecase state
          (:normal (or (next) (return))
           (cond ((char= c #\%)
                  (setq state :percent))
                 ((char= c #\\)
                  (setq state :backslash))
                 (t (add-char c))))
          (:backslash (next-or-error "Expecting character after backslash")
           (cond ((char= c #\t)
                  (add-char #\Tab))
                 ((char= c #\n)
                  (map nil #'add-char (format nil "~%")))
                 (t (add-char c)))
           (setq state :normal))
          (:percent (next-or-error "Expecting conversion char")
           (cond ((char= c #\%)
                  (add-char c)
                  (setq state :normal))
                 (t (setq state :flag))))
          (:flag 
           (cond ((char= c #\:)
                  (setq colon-flag t)
                  (next-or-error "Expecting minimum length"))
                 ((char= c #\@) 
                  (setq at-flag t) 
                  (next-or-error "Expecting minimum length"))
                 ((char= c #\;)
                  (cond ((null prefix)
                         (setq state :prefix
                               prefix (make-array 0 :element-type 'character
                                                    :adjustable t
                                                    :fill-pointer t))
                         (next-or-error "Expecting prefix"))
                        ((null suffix)
                         (setq state :suffix
                               suffix (make-array 0 :element-type 'character
                                                    :adjustable t
                                                    :fill-pointer t))
                         (next-or-error "Expecting suffix"))
                        (t (signal-error "Both prefix and suffix are already set"))))
                 (t (setq state :minus))))
          (:prefix 
           (cond ((char/= c #\;)
                  (vector-push-extend c prefix)
                  (next-or-error "Runaway prefix"))
                 (t (setq state :flag)
                    (next-or-error "Expecting minimum length"))))
          (:suffix 
           (cond ((char/= c #\;)
                  (vector-push-extend c suffix)
                  (next-or-error "Runaway suffix"))
                 (t (setq state :flag)
                    (next-or-error "Expecting minimum length"))))
          (:minus (when (char= c #\-)
                    (next-or-error "Expecting minimum field width")
                    (setq right-justify t)
                    (or (digit-char-p c)
                        (signal-error "Expecting minimum field width")))
           (setq state :minlen))
          (:minlen (cond ((digit-char-p c)
                          (setq minlen (+ (digit-char-p c)  (* 10 minlen)))
                          (next-or-error "Expecting conversion char"))
                         (t (setq state :dot))))
          (:dot (cond ((char= c #\.)
                       (next-or-error "Expecting maximum field width")
                       (setq state :maxlen maxlen 0)
                       (or (digit-char-p c)
                           (signal-error "Expecting maximum field width")))
                      (t (setq state :pattern))))
          (:maxlen (cond ((digit-char-p c)
                          (setq maxlen (+ (digit-char-p c) (* 10 maxlen)))
                          (next-or-error "Expecting conversion char"))
                         (t (setq state :pattern))))
          (:pattern
           (when (eql c stopchar)
             (setq stopchar nil)
             (return))
           (let ((formatter (gethash c *formatters*)))
             (or formatter
                 (signal-error (format nil "Unknown conversion char ~s" c)))
             (add-literal)
             (let ((fmt-info (make-instance 'format-info
                              :conversion-char c
                              :minlen minlen
                              :maxlen maxlen
                              :right-justify right-justify
                              :colon-flag colon-flag
                              :at-flag at-flag
                              :prefix (when (and prefix (plusp (length prefix)))
                                        (coerce prefix 'simple-string))
                              :suffix (when (and suffix (plusp (length suffix)))
                                        (coerce suffix 'simple-string)))))
               (multiple-value-setq (idx fmt-info)
                 (parse-extra-args fmt-info c pattern idx))
               (if (atom formatter) (add-formatter formatter fmt-info)
                   (destructuring-bind (formatter stopchar) formatter
                     (multiple-value-bind (wrap newidx) 
                         (compile-pattern-format pattern idx stopchar)
                       (setq idx newidx)
                       (add-formatter (lambda (stream fmt-info logger level log-func)
                                        (funcall formatter stream fmt-info logger level log-func wrap))
                                      fmt-info))))
               (setq state :normal
                     minlen 0
                     maxlen nil
                     right-justify nil
                     colon-flag nil
                     at-flag nil
                     prefix nil suffix nil))))))
      (when stopchar
        (signal-error (format nil "%~c formatter started at position ~d is missing closing %~c"
                              (char pattern (- start-idx 1))
                              (- start-idx 2) stopchar)))
      (add-literal)
      (setq fm-list (nreverse fm-list))
      (values 
       (lambda (stream logger level log-func)
         (loop for (fm . fm-info) in fm-list
               do (funcall fm stream fm-info logger level log-func)))
       idx))))

(defun compile-pattern (layout pattern)
  (when pattern
    (setf (slot-value layout '%formatter)
          (compile-pattern-format pattern))))

(defgeneric format-time (stream pattern universal-time utc-p)
  (:documentation "Prints UNIVERSAL-TIME to the STREAM according to
strftime like PATTERN."))

(defun format-log-date (stream fmt-info utc-p)
  "Helper function to print either %d or %D (date/time) pattern"
  (declare (type pattern-date-format-info fmt-info))
  (let* ((ut (or (format-universal-time fmt-info) (log-event-time))))
    (if (or (plusp (format-min-len fmt-info))
            (format-max-len fmt-info))
        ;; need to know length of message produced by log-func
        (format-string (with-output-to-string (s)
                         (format-time s (format-date-format fmt-info)
                                      ut utc-p))
                       stream fmt-info)
        (format-time stream (format-date-format fmt-info)
                     ut utc-p)))
  (values))

(define-pattern-formatter (#\d)
  "Output the %d (UTC date/time) pattern"
  (declare (ignore logger log-level log-func))
  (format-log-date stream fmt-info t)
  (values))

(define-pattern-formatter (#\D)
  "Output the %D (local date/time) pattern"
  (declare (ignore logger log-level log-func))
  (format-log-date stream fmt-info nil)
  (values))

(defmethod format-time (stream pattern ut utc-p)
  (declare (type stream stream)
           (type simple-string pattern)
           (type unsigned-byte ut)
           (type boolean utc-p))
  (multiple-value-bind (sec min hour day mon year wday dst-p tz)
      (if utc-p (decode-universal-time ut 0)
          (decode-universal-time ut))
    (let ((state :normal)
          (idx 0)
          (c #\Space)
          (len (length pattern))
          flag-pad-numeric-with-spaces
          flag-numeric-no-pad
          flag-uppercase
          (width 0))
      (declare (type unsigned-byte width)
               (type boolean flag-pad-numeric-with-spaces
                     flag-uppercase flag-numeric-no-pad)
               (type character c)
               (type unsigned-byte len))
      (labels
          (
           ;; get next character or exit entire thing
           (next ()
             (if (< idx len)
                 (prog1 (setq c (schar pattern idx))
                   (incf idx))
                 (return-from format-time (values))))
           ;; output WIDTH - LEN spaces if needed
           (pad (len)
             (declare (type unsigned-byte len))
             (when (> width len)
               (loop repeat (- width len)
                     do (write-char #\Space stream)))
             (values))
           ;; format an two digit decimal
           (output-numeric-2 (n)
             (declare (type (integer 0 99) n))
             (cond ((< n 10)
                    (pad (if flag-numeric-no-pad 1 2))
                    (unless flag-numeric-no-pad
                      (write-char (if flag-pad-numeric-with-spaces #\Space #\0)
                                  stream))
                    (write-char (digit-char n) stream))
                   (t (multiple-value-bind (n1 n2) (truncate n 10)
                        (pad 2)
                        (write-char (digit-char n1) stream)
                        (write-char (digit-char n2) stream))))
             (values))
           ;; output 4 digit numeric value, only used for 4-digit year
           (output-numeric-4 (n)
             (declare (type (integer 1000) n)) ;; no logging on a time machine
             (pad 4)
             (multiple-value-bind (n1 n2) (truncate n 1000)
               (multiple-value-bind (n2 n3) (truncate n2 100)
                 (multiple-value-bind (n3 n4) (truncate n3 10)
                   (write-char (digit-char n1) stream)
                   (write-char (digit-char n2) stream)
                   (write-char (digit-char n3) stream)
                   (write-char (digit-char n4) stream))))
             (values))
           (output-string (s)
             (declare (type simple-string s))
             (let ((len (length s)))
               (declare (type fixnum len))
               (pad len)
               (if flag-uppercase
                   (dotimes (i len)
                     (write-char (char-upcase (schar s i)) stream))
                   (write-string s stream)))
             (values)))
        (loop
          (ecase state
            (:normal (if (char= (next) #\%) (setq state :percent)
                         (write-char c stream)))
            (:percent (cond ((char= (next) #\%)
                             (write-char c stream)
                             (setq state :normal))
                            (t (setq state :flag))))
            (:flag (cond ((char= c #\_)
                          (setq flag-pad-numeric-with-spaces t)
                          (next))
                         ((char= c #\-)
                          (setq flag-numeric-no-pad t)
                          (next))
                         ((char= c #\^)
                          (setq flag-uppercase t)
                          (next))
                         (t (setq state :width))))
            (:width (cond ((digit-char-p c)
                           (setq width (+ (digit-char-p c)
                                          (* 10 width)))
                           (next))
                          (t (setq state :conversion))))
            (:conversion
             (cond
               ((char= c #\Y)
                (output-numeric-4 year))
               ((char= c #\y)
                (output-numeric-2 (mod year 100)))
               ((char= c #\m)
                (output-numeric-2 mon))
               ((char= c #\d)
                (output-numeric-2 day))
               ((char= c #\H)
                (output-numeric-2 hour))
               ((char= c #\M)
                (output-numeric-2 min))
               ((char= c #\S)
                (output-numeric-2 sec))
               ((char= c #\I)
                (output-numeric-2 (let ((hour (if (< hour 12) hour (- hour 12))))
                                    (if (zerop hour) 12 hour))))
               ((char= c #\A)
                (output-string (aref #("Monday" "Tuesday" "Wednesday"
                                       "Thursday" "Friday" "Saturday" "Sunday") wday)))
               ((char= c #\a)
                (output-string (aref #("Mon" "Tue" "Wed"
                                       "Thu" "Fri" "Sat" "Sun") wday)))
               ((char= c #\B)
                (output-string (aref #("January" "February" "March"
                                       "April" "May" "June"
                                       "July" "August" "September"
                                       "October" "November" "December") (1- mon))))
               ((char= c #\b)
                (output-string (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                                       "Aug" "Sep" "Oct" "Nov" "Dec") (1- mon))))
               ((char= c #\c)
                (pad (if (< day 10) 29 30))
                (format-time stream "%a %-d %b %Y %H:%M:%S %z" ut utc-p))
               ((char= c #\p) (output-string (if (< hour 12) "AM" "PM")))
               ((char= c #\P) (output-string (if (< hour 12) "am" "pm")))
               ((char= c #\z)
                (multiple-value-bind (tz-hours tz-mins)
                    (floor (if dst-p
                               (+ tz (/ (- ut (encode-universal-time sec min hour day mon year tz))
                                        (* 60 60)))
                               tz))
                  (pad 5)
                  (format stream "~:[+~;-~]~2,'0d~2,'0d"
                          (plusp tz-hours) (abs tz-hours) 
                          (round (* 60 (coerce tz-mins 'float)))))))
             (setq flag-uppercase nil
                   flag-numeric-no-pad nil
                   flag-pad-numeric-with-spaces nil
                   state :normal
                   width 0))))))))

(defclass pattern-hostname-format-info (format-info)
  ((hostname :initarg :hostname))
  (:documentation "Extra formatting flags for %h pattern"))

(defmethod parse-extra-args (fmt-info (char (eql #\h)) pattern start)
  (declare (ignore pattern))
  (values start (change-class fmt-info 'pattern-hostname-format-info
                              :hostname
                              #+ (and sbcl unix) (sb-unix::unix-gethostname)
                              #+ (and sbcl win32) (sb-win32::get-computer-name)
                              #-sbcl "unknown")))

(define-pattern-formatter (#\h)
  "Output the %h pattern"
  (declare (ignore logger log-level log-func))
  (format-string (or (slot-value fmt-info 'hostname) "") stream fmt-info)
  (values))

(define-pattern-formatter (#\n)
  "Output the %n (newline) pattern"
  (declare (ignore logger log-level log-func))
  (if (and *print-pretty* (format-colon-flag fmt-info))
      (pprint-newline :mandatory stream)
      (terpri stream))
  (values))

(define-pattern-formatter (#\&)
  "Output the optional new line"
  (declare (ignore fmt-info logger log-level log-func))
  (fresh-line stream)
  (values))

(define-pattern-formatter (#\t)
  "Output %t (thread name) pattern"
  (declare (ignore logger log-level log-func))
  (format-string (or (thread-name (current-thread)) "") stream fmt-info))

(define-pattern-formatter (#\x)
  (declare (ignore logger log-level log-func))
  (if (boundp '*ndc-context*) 
      (let ((ndc *ndc-context*))
        (cond 
          ((stringp ndc) (format-string ndc stream fmt-info))
          ;; Directly output he object without consing if
          ;; there is no padding or justification
          ((and (zerop (slot-value fmt-info 'minlen))
                (null (slot-value fmt-info 'maxlen)))
           (when (format-prefix fmt-info)
             (write-string (format-prefix fmt-info) stream))
           (prin1 ndc stream)
           (when (format-suffix fmt-info)
             (write-string (format-suffix fmt-info) stream)))
          ;; have to do indirect formatting, because we have padding or
          ;; justification, and object was not a string
          (t (format-string (with-output-to-string (s)
                              (prin1 ndc s))
                            stream fmt-info))))
      (format-string "" stream fmt-info))
  (values))


(defclass process-id-fmt-info (format-info)
  ((process-id :initarg :process-id))
  (:documentation "Caches process-id"))

(defmethod parse-extra-args (fmt-info (char (eql #\i)) pattern start)
  (declare (ignore pattern))
  (values start (change-class
                 fmt-info 'process-id-fmt-info
                 :process-id (or
                              #+sbcl (sb-posix:getpid)
                              #+clisp (system::process-id)
                              0))))

(define-pattern-formatter (#\i)
  "Output %i (process id) pattern"
  (declare (ignore logger log-level log-func))
  (if (and (zerop (slot-value fmt-info 'minlen))
           (null (slot-value fmt-info 'maxlen)))
      (princ (slot-value fmt-info 'process-id) stream)
      (format-string (with-output-to-string (s)
                       (princ (slot-value fmt-info 'process-id) s))
                     stream fmt-info))
  (values))

(defclass pattern-log-indent-fmt-info (format-info)
  ((indent-string :initarg :indent-string))
  (:documentation "Extra formatting flags for %I (log indent) pattern"))

(defmethod parse-extra-args (fmt-info (char (eql #\I))
                             pattern start)
  (destructuring-bind (next-pos &optional indent-string)
      (parse-extra-args-in-curly-braces pattern start)
    (values next-pos
            (change-class
             fmt-info 'pattern-log-indent-fmt-info
             :indent-string (or indent-string "  ")))))

(define-pattern-formatter (#\I)
  "Output %I (current WITH-LOG-INDENT) pattern"
  (declare (ignore logger log-level log-func))
  (let ((str (slot-value fmt-info 'indent-string)))
    (if (and (zerop (slot-value fmt-info 'minlen))
             (null (slot-value fmt-info 'maxlen)))
        (loop repeat *log-indent* do (write-string str stream))
        (format-string (with-output-to-string (s)
                         (loop repeat *log-indent* do (write-string str s)))
                       stream fmt-info)))
  (values))

(defun log-event-package (logger)
  "Try to find the package at log event site. Can return NIL if
package does not exist at runtime"

  (cond ((packagep *log-event-package-hint*)
         *log-event-package-hint*)
        (t 
         (or 
          (let* ((start-depth (logger-pkg-idx-start logger))
                 (end-depth (logger-pkg-idx-end logger)))
            (when (plusp start-depth)
              (let* ((category (%logger-category logger))
                     (seplen (length (%logger-category-separator logger)))
                     (end (length category))
                     (start (%logger-name-start-pos logger))
                     name)
                ;; a bit of contortions, in unlikely case parent has
                ;; different category separator
                (loop while (> (logger-depth logger) start-depth)
                      do (progn
                           (decf start seplen)
                           (when (>= (logger-depth logger) end-depth) 
                             (setq end start))
                           (setq logger (%logger-parent logger))
                           (decf start (- (length (%logger-category logger))
                                          (%logger-name-start-pos logger)))))
                (setq name 
                      (if (and (zerop start)
                               (= end (length category)))
                          category (substr category start end)))
                (find-package name))))
          *package*))))


(defclass pattern-pretty-fmt-info (format-info)
  ((pretty :initarg :pretty :type (or null (member :pretty :nopretty)) :reader format-pretty)
   (margin :initarg :margin :reader format-margin)
   (package :initarg :package :type (or null (member :package :nopackage)) :reader format-package))
  (:documentation "Extra formatting flags for %<...%> (pretty print) format"))

(defmethod parse-extra-args (fmt-info (char (eql #\<)) pattern start)
  (let ((pretty nil)
        (margin nil)
        (package nil)
        arg)
    (declare (type (or null unsigned-byte (eql :default)) margin))
    (destructuring-bind (next-pos &rest args)
        (parse-extra-args-in-curly-braces pattern start)
      (loop
        (if (null args) (return) (setq arg (pop args)))
        (cond
          ((kw= arg "pretty")
           (setq pretty :pretty)
           (unless (null args)
             (cond ((null (car args))
                    (setq margin :default
                          args (rest args)))
                   ((and (plusp (length (car args)))
                         (digit-char-p (char (car args) 0)))
                    (handler-case
                        (setq arg (pop args)
                              margin (parse-integer arg))
                      (error (err)
                        (pattern-layout-error
                         "~@<Invalid margin argument ~_~s: ~_~a~:>" arg err)))))))
          ((kw= arg "nopretty")
           (setq pretty :nopretty))
          ((kw= arg "package")
           (setq package :package))
          ((kw= arg "nopackage")
           (setq package :nopackage))
          (t (pattern-layout-error
              "~@<Invalid extra argument ~s ~:_around ~
                      position ~d in conversion pattern ~:_~s~:>"
              arg start pattern))))
      (values next-pos
              (change-class
               fmt-info 'pattern-pretty-fmt-info
               :package package :pretty pretty
               :margin margin)))))

(define-pattern-formatter (#\< #\>)
  "Wrap content inside into PPRINT-LOGICAL-BLOCK"
  (flet ((doit (stream)
           (case (format-package fmt-info) 
             (:nopackage (let ((*package* (log-event-package logger)))
                           (funcall wrap stream logger log-level log-func)))
             (:package (let ((*package* (symbol-package :keyword)))
                         (funcall wrap stream logger log-level log-func)))
             (t (funcall wrap stream logger log-level log-func))))) 
    (let ((pretty (format-pretty fmt-info))) 
      (if (not (eq pretty :nopretty))
          (progn 
            (pprint-logical-block (stream nil)
              (if (eq pretty :pretty) 
                  (let* ((*print-pretty* t)
                         (margin (format-margin fmt-info)))
                    (if margin (let ((*print-right-margin*
                                       (if (eq margin :default) nil margin)))
                                 (doit stream))
                        (doit stream)))
                  (doit stream)))) 
          (let ((*print-pretty* nil)) (doit stream))))))

(define-pattern-formatter (#\_)
  "Issue conditional newline"
  (declare (ignore logger log-level log-func))
  (pprint-newline (if (format-colon-flag fmt-info) :fill :linear) stream))

(define-pattern-formatter (#\N)
  "Issue PPRINT-INDENT"
  (declare (ignore logger log-level log-func))
  (let ((n (let ((n1 (format-min-len fmt-info))
                 (n2 (format-max-len fmt-info)))
             (if (null n2) (if (format-right-justify fmt-info) (- n1) n1)
                 (+ n1 (* *log-indent*
                          (if (format-right-justify fmt-info) (- n2) n2))))))) 
    (pprint-indent (if (format-colon-flag fmt-info) :current :block) n stream))) 



