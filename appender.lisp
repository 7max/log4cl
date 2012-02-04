(in-package #:log4cl)

(defun log-appender-error (appender condition)
  (log-error "Appender ~s disabled because of ~s" appender condition))

(defmethod handle-appender-error (appender condition)
  (log-appender-error appender condition))

(defclass counting-appender (appender)
  ((count :initform 0))
  (:documentation "Count the number of times APPENDER-DO-APPEND was called"))

(defmethod appender-do-append ((appender counting-appender) logger level log-func)
  (declare (ignore logger level log-func))
  (incf (slot-value appender 'count))
  (when (next-method-p)
    (call-next-method)))

(defclass serialized-appender (appender)
  ((lock :initform (make-lock)))
  (:documentation "Appender that serializes itself using a lock"))

(defclass stream-appender (serialized-appender)
  ((immediate-flush  :initform nil :initarg :immediate-flush)
   (flush-interval   :initform 1 :initarg :flush-interval)
   (last-flush-time  :initform 0))
  (:documentation "Appender that writes message to stream returned by
  APPENDER-STREAM generic function.

Properties:

  - IMMEDIATE-FLUSH When non-NIL will call FINISH-OUTPUT after every
    log message

  - FLUSH-INTERVAL When set, will only flush if previous flush was
    that many seconds ago. Will only be used if IMMEDIATE-FLUSH is NIL"))

(defgeneric appender-stream (appender) 
  (:documentation "Should return the stream to which appender will write log messages"))

(defclass fixed-stream-appender (stream-appender)
  ((stream :initarg :stream :accessor appender-stream))
  (:documentation "Appender that writes message to the stream specified in STREAM slot"))

(defclass console-appender (stream-appender) () 
  (:documentation "A stream appender that writes messages to
*debug-io* stream.  The *debug-io* is late-binding, that is its the
value of that variable in the thread and at the moment of log message
being written.  If instead you want an appender that would write log
messages to the *debug-io* stream active when appender was created,
use FIXED-STREAM-APPENDER class"))


(defun maybe-flush-appender-stream (appender stream)
  "Flush the APPENDER's stream if needed"
  (with-slots (immediate-flush flush-interval last-flush-time)
      appender
    (cond (immediate-flush
           (setf last-flush-time (log-event-time))
           (finish-output stream))
          (flush-interval
           (let ((time (log-event-time)))
             (when (>= (- time last-flush-time) flush-interval)
               (setf last-flush-time time)
               (finish-output stream)))))))

(defmethod appender-do-append :around
    ((this serialized-appender) logger level log-func)
  (declare (ignore logger level log-func))
  (with-lock-held ((slot-value this 'lock))
    (call-next-method)))

(defmethod appender-do-append ((this stream-appender) logger level log-func)
  (let ((stream (appender-stream this)))
    (layout-to-stream (slot-value this 'layout) stream
                      logger level log-func)
    (maybe-flush-appender-stream this stream))
  (values))

;; Save one generic function dispatch by accessing STREAM slot directly
(defmethod appender-do-append ((this fixed-stream-appender)
                               logger
			       level
                               log-func)
  (let ((stream (slot-value this 'stream)))
    (layout-to-stream (slot-value this 'layout)
                      (slot-value this 'stream)
                      logger level log-func)
    (maybe-flush-appender-stream this stream)))

(defmethod appender-stream ((this console-appender))
  "Returns *DEBUG-IO*"
  *debug-io*)

(defclass file-appender-base (fixed-stream-appender) () 
  (:documentation "Appender that writes to a file"))

(defclass file-appender (file-appender-base)
  ((filename :initarg :filename)) 
  (:documentation "Appender that writes to a file with a fixed file
  name"))

(defclass rolling-file-appender-base (file-appender-base)
  ((period :initform 1 :initarg :period)
   (next-roll-time :initform 0))
  (:documentation "File appender that periodically checks
if it needs to roll the log file.

Calls to MAYBE-ROLL-FILE will be made when log event univeral time
advances past the boundary that is evenly divisible by PERIOD.

PERIOD is specified in minutes"))

(defclass time-rolling-file-appender (rolling-file-appender-base)
  ((backup  :initform nil :initarg :backup)
   (filename :initarg :filename)
   (utc-p :initform nil :initarg :utc-p)
   ;; filename of the currently active log file
   (current-filename :initform nil :reader appender-filename)
   ;; the name that the currently active file will be renamed into
   (current-backup :initform nil))
  (:documentation "An appender that writes to the file named by
expanding FILENAME pattern.  The expansion is done by the same
converter as the %d conversion pattern of the PATTERN-LAYOUT, which is
a subset of patterns supported by POSIX strftime function. UTC-P slot
controls if date pattern expansion uses local or GMT time, default is
local.

Each time an event is logged, and current time is greater or equal to
PERIOD boundary, both FILENAME and BACKUP (if present) will be
expanded.

If either of them differs from their previous values, current log file
will be closed, and a new log file named after expanding FILENAME will
be open. The old log file will be renamed to the result of previous
BACKUP expansion that was remembered when it was open)

In below examples it is assumed that current log file was created an
2012-02-21, and the event being logged is the first one on the next
day.

  1) Example: FILENAME is \"test.log\" and backup is unset,
     will always log to the file named test.log

  2) Example: FILENAME is \"test.%Y%m%d.log\" and BACKUP is
     unset. Will log into the file test.20120221.log file, on the
     rollover it will be closed and test.20120222.file will be opened.

  3) Example: FILENAME is \"test.log\" and BACKUP is \"test.%Y%m%d.log\". Will
     log into the file test.log. On rollover test.log will be renamed to
     test.20120221.log, and new test.log will be created.

     Note that backup file name is the one that was expanded at the
     time when current log file was first created, not one based on
     the current time.

  4) Example: FILENAME is \"test.%Y%m%d\" and backup is
     \"test.log.bak\". Will log into the file test.20120210.log and
     when the day changes, will rename it to test.log.bak (erasing old
     one if it exists)"))

(defun next-time-boundary (time period)
  "Return next universal time evenly divisible by PERIOD minutes "
  (declare (type unsigned-byte time period))
  (setq period (* period 60))
  (* period (truncate (+ time period) period)))

(defmethod appender-do-append :before ((this rolling-file-appender-base)
                                       logger level log-func)
  (declare (ignore logger level log-func))
  (let ((et (log-event-time)))
    (with-slots (this next-roll-time period) this
      (when (>= et next-roll-time)
        (setf next-roll-time (next-time-boundary et period))
        (maybe-roll-file this))))
  (values))

(defgeneric maybe-roll-file (appender)
  (:documentation "Should roll APPENDERs file if needed"))


(defmethod slot-unbound (class (appender file-appender-base)
                         (slot-name (eql 'stream)))
  (declare (ignore class slot-name))
  (create-appender-file appender))

(defun maybe-close-file (appender)
  (when (and (slot-boundp appender 'stream))
    (close (slot-value appender 'stream))
    (slot-makunbound appender 'stream)))

(defun create-appender-file (appender)
  (let ((filename (appender-filename appender)))
    (maybe-close-file appender)
    (setf (slot-value appender 'stream)
          (open (ensure-directories-exist filename)
                :direction :output
                :if-exists :append
                :if-does-not-exist :create))))

(defun expand-filename (pattern time utc-p)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (format-time s pattern time utc-p))))

(defgeneric backup-log-file (appender log-filename backup-filename)
  (:documentation "Should move or rename LOG-FILENAME into the
  BACKUP-FILENAME. When this function is called, LOG-FILENAME is
  already closed.

  Implemented as generic function so its possible to write extensions
  that compress the backup log files automatically, or append
  to them. One possible extension could be having daily log file
  and a weekly backup, that is appended to each day")
  (:method (appender log-filename backup-filename)
    (declare (ignore appender))
    (rename-file log-filename backup-filename)))

(defmethod maybe-roll-file ((appender time-rolling-file-appender)) 
  "Expands FILENAME and BACKUP patterns, and if one of them changed,
switches to the new log file"
  (with-slots (filename backup current-filename current-backup utc-p)
      appender
    (let* ((time (log-event-time))
           (new-file (expand-filename filename time utc-p))
           (new-bak (expand-filename
                     (or backup filename) time utc-p)))
      (unless (and (equal new-file current-filename)
                   (equal new-bak current-backup))
        (when current-filename
          (maybe-close-file appender)
          (unless (equal current-filename current-backup)
            (backup-log-file appender current-filename current-backup)))
        (setq current-filename new-file
              current-backup new-bak)))))
