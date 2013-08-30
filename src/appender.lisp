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

#-sbcl (defvar *global-console* (make-synonym-stream '*debug-io*))
#+sbcl (sb-ext:defglobal *global-console* (make-synonym-stream '*debug-io*))

(defmethod close-appender (appender)
  (declare (ignore appender)))

(defmethod save-appender (appender)
  (declare (ignore appender)))

(defmethod property-alist ((instance appender))
  "Abstract appender has no properties"
  '())

(defun log-appender-disabled (appender condition)
  (ignore-errors
   (log-error :logger +self-meta-logger+ "~@<Caught ~S ~:_~A ~_~
                                             Appender ~S disabled~:>"
              (type-of condition) condition appender)))

(defmethod handle-appender-error (appender condition)
  (log-appender-disabled appender condition)
  :disable)

(defclass counting-appender (appender)
  ((count :initform 0))
  (:documentation "Appender that counts Count the number of times
APPENDER-DO-APPEND was called, and writes its output to null sink"))

(defmethod appender-do-append :before ((appender counting-appender) logger level log-func)
  (declare (ignore logger level log-func))
  (incf (slot-value appender 'count)))

(defmethod appender-do-append ((appender counting-appender) logger level log-func)
  ;; we need to actually format the log message, to invoke any side effects
  ;; that formatting it may produce, this is used in testing error handling
  (with-output-to-string (s)
    (layout-to-stream (slot-value appender 'layout) s logger level log-func))
  (when (next-method-p)
    (call-next-method)))

(defclass temp-appender ()
  ((error-type :initarg :error-type :initform 'error :accessor temp-appender-error-type))
  (:documentation "A mixing for STREAM-APPENDER that will remove the
appender if it encounters an error matching ERROR-TYPE"))

(defclass serialized-appender (appender)
  ((%lock :initform (make-lock)))
  (:documentation "Appender that serializes itself using a lock"))

(defclass stream-appender (serialized-appender)
  ((immediate-flush
    :initform #+bordeaux-threads nil
              #-bordeaux-threads t
    :initarg :immediate-flush)
   (flush-interval   :initform 1 :initarg :flush-interval)
   (%last-flush-time  :initform 0)
   (%output-since-flush :initform nil))
  (:documentation "Appender that writes message to stream. Stream is
obtained on each output by calling APPENDER-STREAM function.

Properties:

IMMEDIATE-FLUSH

: When non-NIL will call FINISH-OUTPUT after every log message

FLUSH-INTERVAL

: When set, will only flush if previous flush was earlier than
FLUSH-INTERVAL seconds ago. In addition a background thread will be
used to flush all appenders with FLUSH-INTERVAL set. See
ADD-WATCH-TOKEN"))

(defmethod property-alist ((instance stream-appender))
  '((:immediate-flush immediate-flush boolean)
    (:flush-interval flush-interval number)))

(defgeneric appender-stream (appender)
  (:documentation "Should return the stream to which appender will write log messages"))

(defclass fixed-stream-appender-base (stream-appender)
  ((stream :accessor appender-stream)
   (stream-owner :initarg :stream-owner :initform nil))
  (:documentation "Appender that writes message to the stream in STREAM slot"))

(defclass fixed-stream-appender (fixed-stream-appender-base)
  ((stream :initarg :stream :accessor appender-stream))
  (:documentation "Appender that writes message to the stream in
STREAM slot."))

(defmethod property-alist ((instance fixed-stream-appender-base))
  (append (call-next-method)
          '((:stream-owner stream-owner nil))))

(defmethod property-alist ((instance fixed-stream-appender))
  (append (call-next-method)
          '((:stream stream :symbol-value))))

(defclass console-appender (stream-appender) ()
  (:documentation "A stream appender that writes messages to
*TERMINAL-IO* stream, which must be a synonym stream"))

(defclass this-console-appender (fixed-stream-appender temp-appender)
  ((stream :initform *global-console*))
  (:documentation
   "An appender that captures the current value of *TERMINAL-IO*
stream, and continues logging to it, until it encounters a stream
error, at which point it will delete itself.

To capture the target output stream, any chain of SYNONYM-STREAM or
TWO-WAY-STREAM is followed recursively, until result is no longer
either synonym or two way stream"))

(defmethod shared-initialize :after ((instance this-console-appender)
				     (slot-names t)
				     &key &allow-other-keys)
  (with-slots (stream) instance
    (setf stream (resolve-stream stream))))

(defclass tricky-console-appender (this-console-appender) ()
  (:documentation
   "Captures the *TERMINAL-IO* stream just like the
THIS-CONSOLE-APPENDER does, but at runtime checks if current value of
*TERMINAL-IO* resolves to the same value and only writes the
message if its different.

When used together with CONSOLE-APPENDER, results that current REPL
thread logs to REPL, while other threads log both to their
*TERMINAL-IO* and REPL.

Auto-deletes itself when encounters stream error"))

(defmethod appender-do-append :around
    ((this tricky-console-appender) logger level log-func)
  (declare (ignore logger level log-func))
  (unless (eq (appender-stream this) (resolve-stream *global-console*))
    (call-next-method)))

#+bordeaux-threads
(defmethod appender-added :after (logger (appender stream-appender))
  "Add appender to the watch tokens in the current hierarchy,
unless IMMEDAITE-FLUSH property is set."
  (declare (ignore logger))
  (with-slots (immediate-flush)
      appender
    (when (not immediate-flush)
      (add-watch-token appender :test #'eq))))

#+bordeaux-threads
(defmethod appender-removed :after (logger (appender stream-appender))
  "When appender refcount is zero, remove it from watch tokens"
  (declare (ignore logger))
  (with-slots (logger-count immediate-flush)
      appender
    (when (zerop logger-count)
      (remove-watch-token appender :test #'eq))))

(defmethod watch-token-check ((appender stream-appender))
  (with-slots (immediate-flush flush-interval %last-flush-time %lock
               %output-since-flush)
      appender
    (let ((time *watcher-event-time*))
      (when (and (not immediate-flush)
                 flush-interval
                 %output-since-flush)
        (let ((since-last-flush (- time %last-flush-time)))
          ;; flush it now if by the next heartbeat we'll be late for
          ;; example if flush-interval is 2, and heartbeat 5, and its
          ;; been 1 second since last flush, then it will be (> (+ 1
          ;; 5) 2) which would flush right now, since next opportunity
          ;; to flush will be only 5 seconds later.
          ;;
          ;; Same calculation with flush-interval 2, heartbeat 0.5 and
          ;; 1 second since last flush will be (> (+ 1 0.5) 2) which
          ;; will be false, because we'll come back in 0.5 seconds and
          ;; flush then.
          (when (> (+ since-last-flush *hierarchy-watcher-heartbeat*)
                   flush-interval)
            (with-lock-held (%lock)
              (setf %last-flush-time    time
                    %output-since-flush nil)
              (finish-output (appender-stream appender)))))))))

(defun maybe-flush-appender-stream (appender stream)
  "Flush the APPENDER's stream if needed, assumes that output had been
just made to an appender. Should be called with the appender lock
held"
  (with-slots (immediate-flush flush-interval %last-flush-time
               %output-since-flush)
      appender
    (cond (immediate-flush
           (finish-output stream)
           (setf %output-since-flush nil
                 %last-flush-time    (log-event-time)))
          (flush-interval
           (let ((time (log-event-time)))
             (when (and (>= (- time %last-flush-time) flush-interval))
               (finish-output stream)
               (setf %output-since-flush nil
                     %last-flush-time    time)))))))

(defmethod appender-do-flush ((appender stream-appender) time)
  "Flush the non-immediate-flush appender unconditionally if there
been any output. TIME will be used to mark the time of the flush"
  (with-slots (immediate-flush  %last-flush-time %lock
               %output-since-flush)
      appender
    (when (and (not immediate-flush)
               %output-since-flush)
      (with-lock-held (%lock)
        (setf %last-flush-time    time
              %output-since-flush nil)
        (finish-output (appender-stream appender))))))

(defun flush-appender (appender &optional (time (get-universal-time)))
  "Immediately flush the appender output if necessary, marking the
time of the flush with TIME"
  (appender-do-flush appender time))

(defun flush-all-appenders (&optional (all-hierarchies t))
  "Flush any appenders that had output since being flushed"
  (let ((time (get-universal-time)))
    (map nil (lambda (x) (flush-appender x time))
         (all-appenders all-hierarchies))))

(defun save-all-appenders (&optional (all-hierarchies t))
  "Flush any appenders that had output since being flushed"
  (map nil (lambda (x) (save-appender x))
       (all-appenders all-hierarchies)))

(defmethod appender-do-append :around
    ((this serialized-appender) logger level log-func)
  (declare (ignore logger level log-func))
  (with-lock-held ((slot-value this '%lock))
    (call-next-method)))

(defmethod appender-do-append ((this stream-appender) logger level log-func)
  (let ((stream (appender-stream this)))
    (with-slots (layout %output-since-flush) this
      (layout-to-stream layout stream logger level log-func)
      (setf %output-since-flush t)
      (maybe-flush-appender-stream this stream)))
  (values))

;; Save one generic function dispatch by accessing STREAM slot directly
(defmethod appender-do-append ((this fixed-stream-appender-base)
                               logger
			       level
                               log-func)
  (with-slots (layout stream %output-since-flush) this
    (layout-to-stream layout stream logger level log-func)
    (setf %output-since-flush t)
    (maybe-flush-appender-stream this stream))
  (values))

(defmethod appender-stream ((this console-appender))
  "Returns current value of *GLOBAL-CONSOLE*, which is a synonym
stream for *TERMINAL-IO*"
  *global-console*)

(defgeneric appender-filename (appender)
  (:documentation "Returns the appenders file name"))

(defgeneric appender-next-backup-file (appender)
  (:documentation "Returns the appenders next backup file name"))

(defgeneric appender-last-backup-file (appender)
  (:documentation "Returns the appenders last backup file name"))

(defun maybe-close-stream (appender)
  (when (and (slot-boundp appender 'stream)
             (slot-value appender 'stream-owner))
    (close (slot-value appender 'stream))
    (slot-makunbound appender 'stream)))

(defclass file-appender-base (fixed-stream-appender-base)
  ((stream-owner :initform t))
  (:documentation "Appender that writes to a file and closes it when
its no longer attached to loggers"))

(defmethod close-appender ((appender fixed-stream-appender-base))
  (maybe-close-stream appender))

(defmethod save-appender ((appender fixed-stream-appender-base))
  (maybe-close-stream appender))

(defclass file-appender (file-appender-base)
  ((filename :initarg :file :reader appender-filename))
  (:documentation "Appender that writes to a file with a fixed file
name"))

(defmethod property-alist ((instance file-appender))
  (append (call-next-method)
          '((:file filename :string-skip-whitespace))))

(defclass rolling-file-appender-base (file-appender-base)
  ((%rollover-check-period :initform 60 :initarg :rollover-check-period)
   (%next-rollover-time :initform 0))
  (:documentation "File appender that periodically checks if it needs
to rollover the log file.

Properties:

ROLLOVER-CHECK-PERIOD

: An integer, when current time advances past the boundary evenly divisible by this
number a call to MAYBE-ROLL-FILE will be made to check if log file needs
to be rolled over"))

(defmethod property-alist ((instance rolling-file-appender-base))
  (append (call-next-method)
          '((:rollover-check-period %rollover-check-period number))))

(defclass daily-file-appender (rolling-file-appender-base)
  ((backup-name-format  :initform nil :initarg :backup-name-format)
   (name-format :initarg :name-format)
   (utc-p :initform nil :initarg :utc)
   ;; File name of the currently active log file
   (%current-file-name :initform nil :reader appender-filename)
   ;; The name that the currently active file will be renamed into
   (%next-backup-name :initform nil :reader appender-next-backup-file)
   (%last-backup-name :initform nil :reader appender-last-backup-file))
  (:documentation "An appender that writes to the file named by
expanding a pattern.  The expansion is done by the same
converter as the %d conversion pattern of the PATTERN-LAYOUT, which is
a subset of patterns supported by strftime POSIX function.

Properties:

NAME-FORMAT
   : Expanded with date formatter to get the name of the current log file

BACKUP-NAME-FORMAT
   : Expanded with date formatter to get the name of the backup log file

UTC-P
   : Should be non-NIL if name and backup patterns expand the UTC time
   instead of local. Defaults to NIL.

MAYBE-ROLL-FILE method works as follows. It expands both name and
backup format (if present).

If either of them differs from their previous values, current log file
will be closed, and a new current log file will be opened.

The old log file will be renamed to %NEXT-BACKUP-NAME, which is a
value of the backup format expansion remembered when original log file
was opened.  The new value of the backup format expansion is
remembered in the %NEXT-BACKUP-NAME slot.

In below examples it is assumed that current log file was created an
2012-02-21, and the event being logged is the first one on the next
day.

  1) Example: NAME-FORMAT is \"test.log\" and backup is unset, will
     always log to the file named test.log

  2) Example: NAME-FORMAT is \"test.%Y%m%d.log\" and
     BACKUP-NAME-FORMAT is unset. Will log into the file
     test.20120221.log file, on the rollover it will be closed and
     test.20120222.file will be opened.

  3) Example: NAME-FORMAT is \"test.log\" and BACKUP-NAME-FORMAT is
     \"test.%Y%m%d.log\". Will log into the file test.log. On rollover
     test.log will be renamed to test.20120221.log, and new test.log
     will be created.

  4) Example: NAME-FORMAT is \"test.%Y%m%d\" and BACKUP-NAME-FORMAT is
     \"test.log.bak\". Will log into the file test.20120210.log and
     when the day changes, will rename it to test.log.bak (erasing old
     one if it exists)"))

(defmethod property-alist ((instance daily-file-appender))
  (append (call-next-method)
          '((:name-format name-format :string-skip-whitespace)
            (:backup-name-format backup-name-format :string-skip-whitespace)
            (:utc utc-p boolean))))

(defun next-time-boundary (time check-period)
  "Given universal time TIME return next boundary evenly divisible by
CHECK-PERIOD seconds "
  (declare (type unsigned-byte time check-period))
  (* check-period (truncate (+ time check-period) check-period)))

(defmethod appender-do-append :before ((this rolling-file-appender-base)
                                       logger level log-func)
  (declare (ignore logger level log-func))
  (let ((time (log-event-time)))
    (with-slots (%next-rollover-time %rollover-check-period) this
      ;; (log-sexp time %next-rollover-time (>= time %next-rollover-time))
      (when (>= time %next-rollover-time)
        (setf %next-rollover-time (next-time-boundary time %rollover-check-period))
        (maybe-roll-file this))))
  (values))

(defgeneric maybe-roll-file (appender)
  (:documentation "Should rollover the log file file if needed"))

(defmethod slot-unbound (class (appender file-appender-base)
                         (slot-name (eql 'stream)))
  (declare (ignore class slot-name))
  (create-appender-file appender))

(defun create-appender-file (appender)
  (let ((filename (appender-filename appender)))
    (maybe-close-stream appender)
    (setf (slot-value appender 'stream)
          (open (ensure-directories-exist filename)
                #+ccl :sharing #+ccl :external
                :direction :output
                :if-exists :append
                :if-does-not-exist :create))))

(defun expand-name-format (pattern time utc-p)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (format-time
       s (coerce (if (pathnamep pattern) (format nil "~a" pattern)
                     pattern) 'simple-string)
       time utc-p))))

(defgeneric backup-log-file (appender log-filename backup-filename)
  (:documentation "Should move or rename LOG-FILENAME into the
BACKUP-FILENAME. When this function is called, LOG-FILENAME is already
closed.

Implemented as generic function so its possible to write extensions
that compress the backup log files automatically, or append to
them. One possible extension could be having daily log file and a
weekly backup, that is appended to each day")
  (:method (appender log-filename backup-filename)
    (declare (ignore appender))
    (rename-file log-filename backup-filename))
  (:method ((appender daily-file-appender) log-filename backup-filename)
    (declare (ignore log-filename))
    (with-slots (%last-backup-name) appender
      (setf %last-backup-name backup-filename)
      (call-next-method))))

(defmethod maybe-roll-file ((appender daily-file-appender))
  "Expands FILENAME and BACKUP patterns, and if one of them changed,
switches to the new log file"
  (with-slots (name-format backup-name-format
               %current-file-name %next-backup-name
               %last-backup-name utc-p) appender
    (let* ((time (log-event-time))
           (new-file (expand-name-format name-format time utc-p)))
      ;; Handle roll over of the log file that we never written to,
      ;; based on its modification time, only happens once at initial
      ;; log into the newly created appender
      (when (and (null %current-file-name)
                 (null %next-backup-name)
                 (probe-file new-file))
        (setq %current-file-name new-file
              %next-backup-name (expand-name-format
                                 (or backup-name-format name-format)
                                 (file-write-date new-file)
                                 utc-p)))
      ;; Normal code path
      (let ((new-bak (expand-name-format
                      (or backup-name-format name-format)
                      time utc-p)))
        (unless (and (equal new-file %current-file-name)
                     (equal new-bak %next-backup-name))
          (when %current-file-name
            (maybe-close-stream appender)
            (unless (equal %current-file-name %next-backup-name)
              (backup-log-file appender %current-file-name %next-backup-name)))
          (setq %current-file-name new-file
                %next-backup-name new-bak))))))


(defmethod handle-appender-error ((a temp-appender) c)
  (cond ((typep c (temp-appender-error-type a))
         (let ((loggers (appender-loggers a)))
           (ignore-errors
            (log-error :logger +self-meta-logger+ "~@<Caught ~S ~:_~A ~_~
                                                    Removing ~S ~_from ~
                                                   ~{~S~^, ~:_~}~:>"
                       (type-of c) c a loggers)))
         (dolist (l (appender-loggers a))
           (remove-appender l a))
         :ignore)
        (t (if (next-method-p) (call-next-method) :ignore))))

(defun resolve-stream (stream)
  "Dereference synonym streams"
  (typecase stream
    (synonym-stream (resolve-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream (resolve-stream (two-way-stream-output-stream stream)))
    (t stream)))
