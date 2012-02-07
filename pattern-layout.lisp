(in-package #:log4cl)


(defclass pattern-layout (layout)
  ((pattern :initarg :pattern :accessor pattern-layout-pattern)
   (formatter))
  (:documentation
   "Pattern layout uses a configurable conversion pattern to format log
messages. For example, the following patterns produce these log
messages:

\"%-5p [%c] - %m%n\" produces the message

 INFO [CATEGORY:SUBCATEGORY:NAME] - message text

and \"%-5p [%c{2}{:invert}{.}] - %m%n\" produces the message:

 INFO [subcategory.name] - message text


Syntax of conversion pattern is: %[-][<MIN>][.<MAX>]<PATTERN-CHAR>[{<EXTRA-ARG>}...]

If a string resulting from the pattern expansion is longer then MAX
its truncated by omitting characters from the start. Then if its
shorter then MIN its padded with spaces to the right or left (when
preceded by a minus)

Performance considerations: All formatting is done without consing,
except for the case of %m (user message) format, when it has MIN or
MAX field width flags. That is because user message is passed around
as a lambda that writes to the stream, and in order for its length to
be known, (with-output-to-string) is used.

Following pattern characters are recognized:

--------------------------------------------------------------------
   %p Log level string, for example DEBUG, INFO, TRACE

   %c Full category name of the logger for example CL-USER:FOO:BAR.

   There can be up to three extra flags in curly braces. Extra flag
   can be a set of empty curly braces, in this case the effect is same
   as default value.

   First extra flag is precision number of categories to display, parent
   categories will be omitted if logger is more then N levels deep.

   For example if logger category name is CL-USER:ONE:TWO:THREE then
   conversion pattern %c{2} will produce TWO:THREE

   Second extra if present will be a separator used between category
   names. If not present then the loggers native value will be used,
   which can be overwritten per package by NAMING-OPTION method

   Third extra argument if present, can be one of :UPCASE, :DOWNCASE or
   :INVERT and will result in printing of the category name in the
   corresponding manner, similar to using non-standard readtable-case

   For example when logger category is CL-USER.FOO.BAR outputting it
   with conversion pattern of %c{}{--}{:invert} will result print it
   as cl-user--foo--bar
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

   %x Value of *ndc-context* variable from (with-ndc-context (context)) macro

   %i Process id of the lisp process, implementation dependent.

   %I Two spaces repeated *log-indent* times. Different padding string
      can be specified in an extra argument.

   %n OS-dependent newline sequence.

   %m Actual user log message.

   "))


(defvar *formatters* (make-hash-table))

(defmacro define-pattern-formatter ((char) &body body)
  `(setf (gethash ,char *formatters*)
         (lambda (stream fmt-info logger log-level log-func)
           ,@body)))

(defun compile-pattern (layout pattern)
  (when pattern
    (setf (slot-value layout 'formatter)
          (compile-pattern-format layout pattern))))

(defmethod initialize-instance :after ((layout pattern-layout) &key pattern)
  (compile-pattern layout pattern))

(defmethod reinitialize-instance :after ((layout pattern-layout) &key pattern)
  (compile-pattern layout pattern))

(defmethod (setf pattern-layout-pattern) :after (pattern (layout pattern-layout))
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
  (with-slots (formatter)
      layout
    (funcall formatter stream logger level log-func))
  (values))


(defclass format-info ()
  ((layout :initarg :layout)
   (conversion-char :initarg :conversion-char :type character)
   (minlen :initform 0 :initarg :minlen :type fixnum)
   (maxlen :initform nil :initarg :maxlen :type (or null fixnum))
   (right-justify :initform nil :initarg :right-justify :type boolean))
  (:documentation "Represents data for a single conversion pattern"))

(defgeneric parse-extra-args (fmt-info pattern-char pattern-string start)
  (:documentation "Should parse extra arguments after the end of the
conversion character in the PATTERN-STRING and return next parse
position. The START is the index of the first character in the
conversion pattern after the PATTERN-CHAR, ie in the string
%-5p{foobar} it will be at the opening curly brace.

Should return two values, new parse position and either FMT-INFO or
its subclass."))

(define-condition pattern-layout-error (simple-error)
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
                     "Unmatched brace at position ~d of conversion pattern ~s"
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
  (let* ((min (slot-value info 'minlen))
         (len (- end start))
         (max (or (slot-value info 'maxlen) len)))
    (declare (type fixnum min max len))
    (when (< max len)
      (incf start (- len max))
      (decf len (- len max)))
    (if (< len min)
        (cond ((not (slot-value info 'right-justify))
               (write-string string stream :start start :end end)
               (loop repeat (- min len)
                     do (write-char #\Space stream)))
              (t (loop repeat (- min len)
                       do (write-char #\Space stream))
                 (write-string string stream :start start :end end)))
        (write-string string stream :start start :end end)))
  (values)) 

(define-pattern-formatter (#\p) 
  "Output the %p (log level) pattern"
  (declare (ignore logger log-func))
  (format-string (log-level-to-string log-level) stream fmt-info)
  (values))

(defmethod parse-extra-args (fmt-info character pattern start)
  "Default method does not parse any extra arguments and returns INFO
  unchanged"
  (declare (ignore character pattern))
  (values start fmt-info))

(defclass pattern-category-format-info (format-info)
  ((precision :initarg :precision)
   (separator :initarg :separator)
   (case :initarg :case))
  (:documentation "Extra formatting flags for %c (log category) pattern"))

(defmethod parse-extra-args (fmt-info (char (eql #\c))
                             pattern start)
  (destructuring-bind (next-pos
                       &optional precision
                                 separator
                                 case)
      (parse-extra-args-in-curly-braces pattern start)
    (values next-pos
            (change-class
             fmt-info 'pattern-category-format-info
             :precision (if (and precision (plusp (length precision)))
                            (handler-case (parse-integer precision)
                              (error (err)
                                (pattern-layout-error
                                 "Invalid precision ~s because of: ~a" precision err)))
                            1000)
             :separator separator
             :case (cond
                     ((null case))
                     ((equalp case ":upcase") :upcase)
                     ((equalp case ":downcase") :downcase)
                     ((equalp case ":invert") :invert)
                     ((equalp case ":preserve") :preserve)
                     (t (pattern-layout-error
                         "Invalid 3rd extra argument ~s around ~
                          position ~d in conversion pattern ~s"
                         case start pattern)))))))

(defclass pattern-date-format-info (format-info)
  ((date-format :initarg :date-format)
   (universal-time :initarg :universal-time)
   (utc-p :initarg :utc-p))
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
                              (error ()
                                (pattern-layout-error
                                 "Invalid universal time in 2nd extra argument ~
                                  around position ~d of pattern ~s"
                                 start pattern))))))))

(defmethod parse-extra-args (fmt-info (char (eql #\d)) pattern start)
  (parse-date-format-extra-args fmt-info t pattern start))

(defmethod parse-extra-args (fmt-info (char (eql #\D)) pattern start)
  (parse-date-format-extra-args fmt-info nil pattern start))

(defun write-string-with-case-conversion (string stream
                                          &optional
                                          case (start 0)
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
             (all-down (write-string-with-case-conversion
                        string stream :upcase start end))
             (all-up (write-string-with-case-conversion
                      string stream :downcase start end))
             (t (write-string string stream :start start
                                            :end end)))))
        (t (write-string string stream :start start
                                       :end end)))
  (values))

(define-pattern-formatter (#\c)
  "Format the %c (log category) pattern"
  (declare (ignore log-level log-func)
           (type pattern-category-format-info fmt-info)
           (type stream stream))
  (with-slots (precision separator case minlen maxlen right-justify)
      fmt-info
    (let ((precision (or precision 1000))
          (separator separator)
          (case case)
          (minlen minlen)
          (maxlen maxlen)
          (right-justify right-justify))
      (if (or case separator)
          ;; have to output category piece by piece because separator
          ;; can be arbitrary string, and there can be case conversion
          (let ((loggers '())
                (num-loggers 0)
                (field-len 0)
                (padlen 0)
                (skip-at-start 0))
            (declare (type list loggers)
                     (dynamic-extent loggers)
                     (type fixnum num-loggers field-len skip-at-start
                           padlen))
            (unless separator
              (setq separator (logger-category-separator logger)))
            ;; collect all the loggers starting from parent one
            ;; that we need to display. Since we starting from
            ;; child and then advancing to parent, the loggers
            ;; end up in the right order without using nreverse
            (loop
              repeat precision
              for lgr = logger then (logger-parent lgr)
              while (plusp (logger-depth lgr))
              do (progn (incf field-len (logger-name-length lgr))
                        (incf num-loggers)
                        (push lgr loggers)))
            ;; add length of separators between loggers
            (incf field-len (* (length separator) (1- num-loggers)))
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
                       (write-string-with-case-conversion
                        string stream case start end))))
                 (doit ()
                   (dotimes (cnt num-loggers)
                     (let ((logger (pop loggers)))
                       (write-string-or-skip (logger-category logger)
                                             (logger-name-start-pos logger)
                                             (length (logger-category logger))
                                             case)
                       (when (< cnt (1- num-loggers))
                         (write-string-or-skip separator 0
                                               (length separator) nil)))))
                 (pad ()
                   (loop repeat padlen do (write-char #\Space stream))))
              (cond ((and (plusp padlen) right-justify)
                     (pad) (doit))
                    ((plusp padlen)
                     (doit) (pad))
                    (t (doit)))))
          ;; can output entire category in one piece
          (let ((start 0))
            (declare (type fixnum start))
            (when (< precision (logger-depth logger))
              (loop for lgr = logger then (logger-parent lgr)
                    while lgr
                    do (setq start (logger-name-start-pos lgr)
                             precision (1- precision))
                    until (zerop precision)))
            (format-string (logger-category logger) stream fmt-info start)))))
  (values))

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


(defun compile-pattern-format (layout pattern)
  "Parses the pattern format and returns a function with lambda-list
of \(STREAM STREAM LOG-LEVEL LOG-FUNC\) that when called will output
the log message to the stream with the specified format."
  (let ((idx 0)
        (str (make-array 0 :element-type 'character
                           :adjustable t
                           :fill-pointer t))
        (c #\Space) (fm-list '()) (state :normal)
        (minlen 0) (maxlen nil) (right-justify nil))
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
                 (t (setq state :minus))))
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
          (:pattern (let ((formatter (gethash c *formatters*)))
                      (or formatter
                          (signal-error (format nil "Unknown conversion char ~s" c)))
                      (add-literal)
                      (let ((fmt-info (make-instance 'format-info
                                       :layout layout
                                       :conversion-char c
                                       :minlen minlen
                                       :maxlen maxlen
                                       :right-justify right-justify)))
                        (multiple-value-setq (idx fmt-info)
                          (parse-extra-args fmt-info c pattern idx))
                        (add-formatter formatter fmt-info)
                        (setq state :normal
                              minlen 0 maxlen nil right-justify nil))))))
      (add-literal)
      (setq fm-list (nreverse fm-list))
      (lambda (stream logger level log-func)
        (loop for (fm . fm-info) in fm-list
              do (funcall fm stream fm-info logger level log-func))))))

(defgeneric format-time (stream pattern universal-time utc-p)
  (:documentation "Prints UNIVERSAL-TIME to the STREAM according to
                                                             strftime like PATTERN."))

(defun format-log-date (stream fmt-info utc-p)
  "Output the %d or %D pattern"
  (declare (type pattern-date-format-info fmt-info))
  (with-slots (minlen maxlen date-format universal-time) fmt-info
    (let* ((ut (or universal-time (log-event-time))))
      (if (or (plusp minlen) maxlen)
          ;; need to know length of message produced by log-func
          (format-string (with-output-to-string (s)
                           (format-time s date-format ut utc-p))
                         stream fmt-info)
          (format-time stream date-format ut utc-p))))
  (values))

(define-pattern-formatter (#\d)
  "Output the %d pattern"
  (declare (ignore logger log-level log-func))
  (format-log-date stream fmt-info t)
  (values))

(define-pattern-formatter (#\D)
  "Output the %D pattern"
  (declare (ignore logger log-level log-func))
  (format-log-date stream fmt-info nil)
  (values))

(defmethod format-time (stream pattern ut utc-p)
  (declare (type stream stream)
           (type simple-string pattern)
           (type unsigned-byte ut)
           (type boolean utc-p))
  (multiple-value-bind (sec min hour day mon year wday dst-p tz)
      (decode-universal-time ut (if utc-p 0))
    (declare (ignore dst-p))
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
                    (floor tz)
                  (pad 5)
                  (format stream "~:[+~;-~]~2,'0d~2,'0d"
                          (minusp tz-hours) (abs tz-hours) 
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
  (format-string (slot-value fmt-info 'hostname) stream fmt-info)
  (values))

(defclass pattern-newline-format-info (format-info)
  ((width    :initarg :width)
   (continue :initarg :continue))
  (:documentation "Extra formatting flags for %n pattern"))

(defmethod parse-extra-args (fmt-info (char (eql #\n)) pattern start)
  (destructuring-bind (next-pos &optional width continue)
      (parse-extra-args-in-curly-braces pattern start)
    (values next-pos
            (change-class fmt-info 'pattern-newline-format-info
                          :width (when width
                                   (handler-case (parse-integer width)
                                     (error (err)
                                       (pattern-layout-error
                                        "Invalid newline width ~s: ~a" width err))))
                          :continue continue))))

(define-pattern-formatter (#\n)
  "Output the %n (newline) pattern"
  (declare (ignore fmt-info logger log-level log-func))
  (terpri stream)
  (values))

(define-pattern-formatter (#\t)
  "Output %t (thread name) pattern"
  (declare (ignore logger log-level log-func))
  (format-string (thread-name (current-thread)) stream fmt-info))

(define-pattern-formatter (#\x)
  (declare (ignore logger log-level log-func))
  (let ((ndc (if (boundp '*ndc-context*)
                 (or *ndc-context* "") "")))
    (cond 
      ((stringp ndc) (format-string ndc stream fmt-info))
      ((and (zerop (slot-value fmt-info 'minlen))
            (null (slot-value fmt-info 'maxlen)))
       (prin1 ndc stream))
      (t (format-string (with-output-to-string (s)
                          (prin1 ndc s))
                        stream fmt-info))))
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
  "Output %i (process id) pattern"
  (declare (ignore logger log-level log-func))
  (let ((str (slot-value fmt-info 'indent-string)))
    (if (and (zerop (slot-value fmt-info 'minlen))
             (null (slot-value fmt-info 'maxlen)))
        (loop repeat *log-indent* do (write-string str stream))
        (format-string (with-output-to-string (s)
                         (loop repeat *log-indent* do (write-string str s)))
                       stream fmt-info)))
  (values))

