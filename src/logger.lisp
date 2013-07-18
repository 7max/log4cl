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

;;; Most of the logger internals.
;;;
;;; Note this file can not use any of the logging functions, as
;;; logging-macros.lisp had not yet complied.
;;;
;;; So any code that uses self-logging to log4cl:self logger, needs to
;;; be in the files that are later in the system definition file
;;; 
(in-package #:log4cl)

;;
;; These needs to be in separate file from the their definitions. I'm
;; actually unable to see SBCL generating code any differently with
;; these present, maybe even with speed 3 safety 0
;; 
#+sbcl
(declaim (sb-ext:always-bound
          *log-indent*
          *log-event-time*
          *log-event-package-hint*
          *inside-user-log-function*))

;; Circularity, *root-logger* needs to be referenced by logger
;; creation function which we need to be defined so we can defvar the
;; root logger
(declaim (special *root-logger*)
         (type logger *root-logger*)
	 (type fixnum *log-indent*)
         (inline is-enabled-for current-state 
                 log-level-to-string
                 log-level-to-lc-string
                 log-event-time))

;; Per-hierarchy logger state
(defstruct logger-state
  ;; List of appenders attached to this logger
  (appenders nil :type list)
  ;; Explicit log level for this logger if its set, NIL otherwise
  (level nil :type (or null fixnum))
  ;; Does logger passes messages upward to its ancestor appenders
  (additivity t :type boolean)
  ;; Performance hack, for each log level we set a bit in mask, if a
  ;; log message with said level will reach any appenders either in
  ;; this logger or it's parents
  ;;
  ;; ADJUST-LOGGER recalculates this value
  (mask 0 :type fixnum))

;; Actual logger object
(defstruct (logger (:constructor create-logger)
                   (:conc-name %logger-)
                   (:print-function 
                    (lambda  (logger stream depth)
                      (declare (ignore depth))
                      (print-unreadable-object (logger stream :identity t)
                        (princ (string 'logger) stream)
                        (princ #\Space stream)
                        (princ (if (%logger-parent logger) (%logger-category logger)
                                   (string '+root+)) stream)))))
  ;; Full category name in a string format, using whatever separator
  ;; that was in effect when logger was first instantiated
  (category  nil :type simple-string)
  ;; Native category separator
  (category-separator nil :type simple-string)
  ;; Position of the 1st character of this logger name in the CATEGORY. Used
  ;; as space saving measure so we don't have to keep loggers own short name
  ;; separately
  (name-start-pos 0 :type fixnum)
  ;; Parent logger, only NIL for the root logger
  (parent    nil :type (or null logger))
  ;; How many parents up and including the root logger this logger
  ;; has. Root logger has depth of zero
  (depth     0   :type fixnum)
  ;; Child loggers. Only set when any child loggers are present
  (child-hash  nil :type (or null hash-table))
  ;; Per-hierarchy state array
  (state (map-into (make-array *hierarchy-max*) #'make-logger-state)
   :type (simple-array logger-state *)))

(defstruct (root-logger
            (:constructor %create-root-logger)
            (:include logger))
  (dummy nil))

(defun logger-category (logger) (%logger-category logger))
(defun logger-category-separator (logger) (%logger-category-separator logger))
(defun logger-name-start-pos (logger) (%logger-name-start-pos logger))
(defun logger-parent (logger) (%logger-parent logger))
(defun logger-child-hash (logger) (%logger-child-hash logger))
(defun logger-state (logger) (%logger-state logger))

(declaim (inline logger-category logger-category-separator
                 logger-name-start-pos logger-parent logger-child-hash
                 logger-state))

;; Special logger representing the source file
(defstruct (source-file-logger
            (:constructor create-source-file-logger)
            (:include logger)
            (:print-function 
             (lambda  (logger stream depth)
               (declare (ignore depth))
               (print-unreadable-object (logger stream :type t :identity t)
                 (write-string (%logger-category logger) stream)))))
  (file nil :type pathname)
  (namestring nil :type simple-string))

(defstruct (file-logger
            (:constructor create-file-logger)
            (:include logger)
            (:print-function 
                    (lambda  (logger stream depth)
                      (declare (ignore depth))
                      (print-unreadable-object (logger stream :type t :identity t)
                        (princ (if (%logger-parent logger) (%logger-category logger)
                                   (string '+root+)) stream)
                        (princ #\Space stream)
                        (princ (logger-file logger) stream)))))
  (file nil :type (or null source-file-logger)))

;; package-start/end, file, own depth  -> 3 * 6 = 18
(defmacro define-category-depth-accessor (name1 name2 num)
  (let ((shift (* -1 num +logger-category-depth-bits+))
        (mask (1- (expt 2 +logger-category-depth-bits+))))
    `(progn
       (declaim (inline ,name1 ,name2)
                (ftype (function (logger) logger-cat-idx) ,name1))
       (defun ,name1 (logger)
         (logand (ash (%logger-depth logger) ,shift) ,mask))
       (defun ,name2 (flags)
         (let ((tmp (logand (ash flags ,shift) ,mask)))
           (if (plusp tmp) (1- tmp)))))))


(define-category-depth-accessor logger-depth logger-flags-depth 0)
(define-category-depth-accessor logger-pkg-idx-start logger-flags-pkg-idx-start 2)
(define-category-depth-accessor logger-pkg-idx-end logger-flags-pkg-idx-end 3)

(declaim (inline logger-first-after-package-p))

(defun logger-first-after-package-p (logger)
  (plusp (logand (%logger-depth logger) +logger-after-package-flag+)))

(defun make-logger-flags (depth &optional pkg-idx-start pkg-idx-end)
  (declare (type logger-cat-idx depth)
           (type (or null logger-cat-idx) pkg-idx-start pkg-idx-end))
  (assert (or (and (null pkg-idx-start)
                   (null pkg-idx-end))
              (and (< pkg-idx-start pkg-idx-end)
                   (<= (1- pkg-idx-end) depth))))
  (logior depth
          (if (eql depth pkg-idx-end) +logger-after-package-flag+ 0)
          (ash (or pkg-idx-start 0) (* 2 +logger-category-depth-bits+))
          (ash (or pkg-idx-end 0) (* 3 +logger-category-depth-bits+))))

(defun log-level-to-string (level)
  "Return log-level string for the level"
  (aref +log-level-to-string+ level))

(defun log-level-to-lc-string (level)
  "Return lower case log-level string for the level"
  (aref +log-level-to-lc-string+ level))

(defun make-log-level (arg)
  "Translate a more human readable log level into one of the log
level constants, by calling LOG-LEVEL-FROM-OBJECT on ARG and current
value of *PACKAGE* "
  (log-level-from-object arg *package*))

(defun effective-log-level (logger)
  "Return logger's own log level (if set) or the one it had inherited
from parent"
  (declare (type logger logger))
  (let ((file-logger (when (typep logger 'file-logger)
                       (file-logger-file logger)))) 
    (labels ((%effective-log-level (lgr) 
             (if (null lgr) +log-level-off+
                 (let* ((state (current-state lgr))
                        (level (logger-state-level state)))
                   (or level
                       (when (and file-logger (logger-first-after-package-p lgr))
                         (effective-log-level file-logger))
                       (%effective-log-level (%logger-parent lgr)))))))
      (%effective-log-level logger))))

(defun inherited-log-level (logger)
  "Return logger's own log level (if set) or the one it had inherited
from parent"
  (declare (type logger logger))
  (let* ((file-logger (when (typep logger 'file-logger)
                        (file-logger-file logger))))
    (if (and file-logger (logger-first-after-package-p logger))
        (effective-log-level file-logger)
        (unless (eq logger *root-logger*) 
          (effective-log-level (%logger-parent logger))))))

(defun have-appenders-for-level (logger level)
  "Return non-NIL if logging with LEVEL will actually reach any
appenders"
  (declare (type logger logger) (type fixnum level))
  ;; Note: below code actually walk parent chain twice but since its
  ;; cached anyway, don't optimize unless becomes a problem
  (labels ((have-appenders (logger)
	     (when logger
               (let ((state (current-state logger)))
                 (or (logger-state-appenders state)
                     (and (logger-state-additivity state)
                          (have-appenders (%logger-parent logger))))))))
    (let* ((logger-level (effective-log-level logger)))
      (and (>= logger-level level)
	   (have-appenders logger)))))

(defun map-logger-children (function logger)
  "Apply the function to all of logger's children (but not their
descendants)"
  (let ((child-hash (%logger-child-hash logger)))
    (when child-hash
      (maphash (lambda (key logger)
                 (declare (ignore key))
                 (funcall function logger))
               child-hash))))

(defun map-logger-descendants (function logger)
  "Apply the function to all of logger's descendants"
  (let ((child-hash (%logger-child-hash logger)))
    (when child-hash
      (maphash (lambda (name logger)
                 (declare (ignore name))
                 (funcall function logger)
                 (unless (typep logger 'source-file-logger) 
                   (map-logger-descendants function logger)))
               child-hash))))

(defun adjust-logger (logger)
  "Recalculate LOGGER mask by finding out which log levels have
reachable appenders. "
  (labels ((doit (logger)
             (let ((state (current-state logger))
                   (mask 0))
               (declare (type fixnum mask))
               (loop
                  for level from +min-log-level+ upto +max-log-level+
                  if (have-appenders-for-level logger level)
                  do (setf mask (logior mask (ash 1 level))))
               (setf (logger-state-mask state) mask)
               (map-logger-children #'doit logger))))
    (doit logger))
  (values))

(defun strip-whitespace (string)
  "Strip tab and space characters from a string"
  (declare (type string string))
  (remove-if (lambda (c)
               (or (char= c #\Space)
                   (char= c #\Tab)))
             string))

(defun split-string (string separator &optional skip-whitespace-p)
  "Split the STRING into a list of strings. If SKIP-WHITESPACE-P is
non-NIL strip whitespace from the string first"
  (declare (type string string separator))
  (if (not skip-whitespace-p)
      (loop for start = 0 then (+ pos (length separator))
            as pos = (search separator string :start2 start)
            if (or (plusp start)
                   (< start (length string)))
            collect (substr string start pos)
            while pos)
      (split-string (strip-whitespace string) separator)))

(defun split-into-categories (category package)
  "Splits the category name into a list of categories from parent to
child. Uses NAMING-OPTION to determine category separator"
  (split-string category (naming-option package :category-separator)))

(defun %get-logger (categories cat-sep cat-case
                    &optional force-string-case
                              (createp t)
                              file
                              pkg-idx-start
                              pkg-idx-end
                              is-file-p)
  "Retrieve or create a logger.

CATEGORIES -- List of category names
SEPARATOR  -- Category separator. Will only be used if logger did not
              exist before.
CAT-CASE   -- How each category case is treated. See NAMING-OPTION
              generic function for description

FORCE-STRING-CASE -- Whenever elements of category which are strings,
should also undergo case conversion according to CAT-CASE

CREATEP    -- Create the logger if it does not exist
FILE       -- pathname, source file being compiled

PKG-IDX-START/PKG-IDX-END -- zero based indexes as to where
in CATEGORIES package name starts ends, meaning of indexes
is like SUBSEQ.. If package is unknown, both must be NIL

For historical reason the above indexes are incremented by one before
being stored in FLAGS slot, and functions LOGGER-PKG-IDX-START/END
return them 1 based, with 0 being used as a flag that there is no
package name in the category.

IS-FILE-P -- T when its an actual SOURCE-FILE-LOGGER being requested,
which is a special type of leaf logger representing the source file."

  (let ((source-file-logger nil)
        (names (make-array 0 :adjustable t :fill-pointer t)))
    (labels ((make-category ()
               (coerce (with-output-to-string (s)
                         (dotimes (i (length names))
                           (when (plusp i)
                             (write-string cat-sep s))
                           (write-string (aref names i) s)))
                       'simple-string))
             ;; this is really convoluted
             (ensure-source-logger (parent)
               (when (and file (>= (logger-depth parent) (or pkg-idx-end 0)))
                 (or source-file-logger
                     (setq source-file-logger 
                           (%get-logger
                            (append (subseq (coerce names 'list)
                                            (or pkg-idx-start 0)
                                            (or pkg-idx-end 0))
                                    (list (namestring file)))
                            cat-sep cat-case
                            nil t file pkg-idx-start pkg-idx-end t)))))
             (create-logger-from-parent (parent)
               (let* ((parent-depth (logger-depth parent))
                      (category (make-category))
                      (name-start-pos (if (= 1 (length names)) 0
                                          (- (length category)
                                             (length (aref names (1- (length names)))))))
                      (flags
                        (make-logger-flags
                         (1+ parent-depth)
                         (if (and pkg-idx-start (<= pkg-idx-end (1+ parent-depth))) 
                             (1+ pkg-idx-start))
                         (if (and pkg-idx-start  
                                  (<= pkg-idx-end (1+ parent-depth))) 
                             (1+ pkg-idx-end))))
                      (logger 
                        (if (and is-file-p (null categories)) 
                            (create-source-file-logger
                             :category category
                             :category-separator (coerce cat-sep 'simple-string)
                             :parent parent
                             :name-start-pos name-start-pos
                             :depth flags
                             :file file
                             :namestring (coerce (file-namestring file) 'simple-string))
                            (create-file-logger
                             :category category
                             :category-separator (coerce cat-sep 'simple-string)
                             :parent parent
                             :name-start-pos name-start-pos
                             :depth flags
                             :file (ensure-source-logger parent)))))
                 (when (and (not is-file-p)
                            source-file-logger)
                   (setf
                    (gethash logger 
                             (or (%logger-child-hash source-file-logger)
                                 (setf (%logger-child-hash source-file-logger)
                                       (make-hash-table :test #'equal))))
                    logger))
                 (dotimes (*hierarchy* *hierarchy-max*)
                   (adjust-logger logger))
                 logger))) 
      (do ((logger *root-logger*))
          ((null categories) logger)
        (let* ((cat (pop categories))
               (name (if (and (stringp cat)
                              (not force-string-case))
                         cat
                         (with-output-to-string (s)
                           (when (and (keywordp cat)
                                      (not (position #\: cat-sep)))
                             (write-char #\: s))
                           (write-string-modify-case
                            (coerce 
                             (typecase cat
                               (string cat)
                               (symbol (symbol-name cat))
                               (character (string cat))
                               (t (princ-to-string cat)))
                             'simple-string)
                            s cat-case))))
               (hash (%logger-child-hash logger)))
          (vector-push-extend name names)
          (setq logger
                (or
                 ;; special case of trying to get a source-file-logger
                 ;; by partial name, happens when Log4CL Slime
                 ;; integration calls us to get info on the file
                 ;; logger
                 (and hash is-file-p (not file) (not createp) (null categories)
                      (find-if (lambda (child)
                                 (zerop (or (mismatch name (%logger-category child)
                                                      :from-end t
                                                      :start2 (%logger-name-start-pos child))
                                            0)))
                               (logger-children logger)))
                 (and hash
                      (let ((cached (gethash name hash)))
                        ;; We only do below two checks for part of the
                        ;; category after the package
                        (when (and cached (typep cached 'file-logger)
                                   (not is-file-p)
                                   (or
                                    ;; leaf, but not part of the package name
                                    (and (null categories)
                                         (> (logger-depth cached) 
                                            (or pkg-idx-end 0))) 
                                    ;; parent is not part of package name 
                                    (> (logger-depth logger)
                                       (or pkg-idx-end 0))))
                          ;; Check if logger source file had changed
                          (let ((old (file-logger-file cached))
                                (new (ensure-source-logger cached))
                                (didit nil))
                            ;; if logger is in the same file as before, nothing needs to be done
                            (unless (eq old new)
                              ;; If was in the file, and now moved to different file
                              ;; remove it from old file
                              (when (and old new (%logger-child-hash old))
                                (remhash cached (%logger-child-hash old)))
                              ;; Is now in the new file, remember it there
                              (when new
                                (setf (gethash cached (or (%logger-child-hash new)
                                                          (setf (%logger-child-hash new)
                                                                (make-hash-table :test #'equal))))
                                      cached)
                                (setf (file-logger-file cached) new
                                      didit t))
                              ;; note that if logger is instantiated
                              ;; without a file, but was previously
                              ;; instantiated from a file, it keeps
                              ;; the old file, so that log statements
                              ;; issued from REPL do not reset
                              ;; logger's file
                              (when didit
                                (adjust-logger cached))))
                          ;; Check if the part of the logger that
                          ;; represents package had changed
                          (when (and pkg-idx-start (<= pkg-idx-end (logger-depth cached))
                                     (or (/= (1+ pkg-idx-start)
                                             (logger-pkg-idx-start cached))
                                         (/= (1+ pkg-idx-end)
                                             (logger-pkg-idx-end cached))))
                            (setf (%logger-depth cached) 
                                  (make-logger-flags (logger-depth cached)
                                                     (1+ pkg-idx-start) (1+ pkg-idx-end)))))
                        cached))
                 (unless createp (return-from %get-logger nil))
                 (setf (gethash name (or hash
                                         (setf (%logger-child-hash logger)
                                               (make-hash-table :test #'equal))))
                       (create-logger-from-parent logger)))))))))

(setf (fdefinition 'get-logger-internal) (fdefinition '%get-logger))

(defun current-state (logger)
  (svref (%logger-state logger) *hierarchy*))

(defun is-enabled-for (logger level)
  "Returns t if log level is enabled for the logger in the
context of the current application."
  (declare (type logger logger) (type fixnum level))
  (let* ((state (current-state logger))
	 (mask (logger-state-mask state)))
    (declare (type fixnum mask))
    (not (zerop (logand (the fixnum (ash 1 level)) mask)))))

(defconstant package-ref-sym (intern (format nil " ~A" (symbol-name 'log4cl-orig-package-ref))))

(defun expand-log-with-level (env level args)
  "Returns a FORM that is used as an expansion of log-nnnnn macros"
  (declare (type fixnum level)
	   (type list args))
  (with-package-naming-configuration (*package*) 
    (multiple-value-bind (logger-form args)
        (resolve-logger-form *package* env args)
      (let* ((logger-symbol (gensym "logger"))
             (log-stmt (gensym "log-stmt"))
             (stream (gensym "stream"))
             (const-logger (when (constantp logger-form)
                             (let ((logger (eval logger-form)))
                               (when (typep logger 'logger)
                                 logger))))
             (check-type (unless const-logger 
                           `(or (typep ,logger-symbol 'logger)
                                (error 'type-error :expected-type 'logger
                                                   :datum ,logger-symbol))))
             (pkg-hint
               (let ((sym (intern (symbol-name package-ref-sym) *package*))) 
                 (when sym `(symbol-package ',sym)))))
        (if args 
            `(let ((,logger-symbol ,logger-form))
               #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
               (when (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                       ,@(when check-type (list check-type))
                       (is-enabled-for ,logger-symbol ,level)) 
                 (flet ((,log-stmt (,stream)
                          (declare (type stream ,stream))
                          (format ,stream ,@args)))
                   (declare (dynamic-extent #',log-stmt))
                   (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                     (log-with-logger ,logger-symbol ,level #',log-stmt ,pkg-hint))))
               (values))
            ;; null args means its being used for checking if level is enabled
            ;; in the (when (log-debug) ... complicated debug ... ) way
            `(let ((,logger-symbol ,logger-form))
               (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                 ,@(when check-type (list check-type))
                 (is-enabled-for ,logger-symbol ,level))))))))

(defun substr (seq start &optional end)
  (declare (fixnum start)
           (type (or fixnum null) end)
           (vector seq))
  (unless end (setq end (length seq)))
  (make-array (- end start) :element-type (array-element-type seq)
                          :displaced-to seq :displaced-index-offset start))

(defun log-with-logger (logger level log-func package)
  "Submit message to logger appenders, and its parent logger"
  (let ((*log-event-time* nil)
        (*log-event-package-hint* package))
    (labels ((log-to-logger-appenders (logger orig-logger level log-func)
               (let* ((state (current-state logger))
                      (appenders
                        (logger-state-appenders state)))
                 (dolist (appender appenders)
                   (with-slots (last-error last-ignored-error
                                error-count ignored-error-count
                                message-count enabled) appender
                     (when enabled
                       (loop
                         for done = t
                         for error-cnt fixnum from 0
                         do (block nil
                              (handler-bind
                                  ((error 
                                     (lambda (e)
                                       ;; if error is inside the actual user log statement
                                       ;; fall through, to invoke the debugger or such.
                                       ;;
                                       ;; Idea is that we want (log:info "~s" (something-causing-error))
                                       ;; to pop the debugger rather then go through appender error logic.
                                       (unless *inside-user-log-function*
                                         (case (if (eq orig-logger +self-meta-logger+) :ignore
                                                   (handle-appender-error appender e))
                                           (:retry
                                            (incf error-count)
                                            (setf last-error e)
                                            (cond ((< error-cnt 2)
                                                   (setf done nil))
                                                  ;; manually disable if HANDLE-APPENDER-ERROR
                                                  ;; can't fix it 3 times in a row
                                                  (t (setf enabled nil)
                                                     (log-appender-disabled appender e))))
                                           (:ignore
                                            (incf ignored-error-count)
                                            (setf last-ignored-error e))
                                           ;; treat any other same as :disable
                                           (t
                                            (incf error-count)
                                            (setf last-error e enabled nil)))
                                         (return)))))
                                (progn (appender-do-append appender orig-logger level log-func)
                                       (incf message-count))))
                         until done))))
                 (let ((parent (%logger-parent logger)))
                   (when (and parent (logger-state-additivity state))
                     (log-to-logger-appenders parent orig-logger level log-func))))
               (values)))
      (log-to-logger-appenders logger logger level log-func)
      (values))))


(defun logger-children (logger)
  "Return a freshly consed list of LOGGER's direct children"
  (let ((hash (%logger-child-hash logger)))
    (when hash
      (loop for child being each hash-value in (%logger-child-hash logger)
            collect child))))

(defun logger-descendants (logger &optional noselfp)
  "Return a list of LOGGER's descendants.

NOSELFP if T filters out Log4CL self-logger from descendants"
  (labels ((%logger-descendants (logger) 
             (let ((children (logger-children logger)))
               (when (and noselfp (eq logger *root-logger*))
                 (setq children (delete +self-logger+ children)))
               (nconc children (mapcan #'%logger-descendants children)))))
    (delete-duplicates (%logger-descendants logger) :test #'eq)))

(defun logger-ancestors (logger)
  "Return a list of logger's ancestors starting from parent and ending wit root logger"
  (loop for lgr = (%logger-parent logger) then (%logger-parent lgr)
        while lgr
        collect lgr))

(defun logger-log-level (logger)
  "Return the logger own log level or NIL if unset. Please note that
by default loggers inherit their parent logger log level, see
EFFECTIVE-LOG-LEVEL"
  (declare (type logger logger))
  (logger-state-level
   (current-state logger)))

(defun logger-appenders (logger)
  "Return the list of logger's own appenders. Please note that by
default loggers inherit their parent logger appenders, see
EFFECTIVE-APPENDERS"
  (declare (type logger logger))
  (logger-state-appenders (current-state logger)))

(defun effective-appenders (logger)
  "Return the list of all appenders that logger output could possible go to,
including inherited one"
  (declare (type logger logger))
  (loop for tmp = logger then parent
        as state = (current-state tmp)
        as parent = (%logger-parent tmp)
        append (logger-state-appenders state)
        while (and parent (logger-state-additivity state))))

(defun (setf logger-log-level) (level logger)
  "Set logger log level. Returns logger own log level"
  (declare (type logger logger))
  ;; Note that set-log-level accepts various forms
  ;; of levels, so we return the numeric one from SETF
  (nth-value 1 (set-log-level logger level)))

(defun logger-additivity (logger)
  "Return logger additivity"
  (declare (type logger logger))
  (logger-state-additivity (current-state logger)))

(defun (setf logger-additivity) (additivity logger)
  "Set logger appender additivity. Returns new additivity"
  (declare (type logger logger))
  (set-additivity logger additivity))

(defun set-log-level (logger level &optional (adjust-p t))
  "Set the log level of a logger. Log level is passed to
MAKE-LOG-LEVEL to determine canonical log level. ADJUST-P controls if
logger effective log level needs to be recalculated, the caller should
NIL doing bulk operations that change the level of many loggers, as to
avoid overhead.

Returns if log level had changed as the 1st value and new level as the
second value."
  (declare (type logger logger))
  (let* ((level (make-log-level level))
         (state (current-state logger))
         (old-level (logger-state-level state))
         (new-level (if (/= level +log-level-unset+) level
                        (if (eq logger *root-logger*) +log-level-off+ nil))))
    (declare (type fixnum level)
             (type logger-state state)
             (type (or null fixnum) old-level new-level))
    (unless (eql old-level new-level)
      (setf (logger-state-level state) new-level)
      (when adjust-p
        (adjust-logger logger))
      (values t new-level))))

(defun set-additivity (logger additivity &optional (adjust-p t))
  "Set logger additivity."
  (declare (type logger logger))
  (let* ((state (current-state logger))
         (old-additivity (logger-state-additivity state)))
    (unless (eql old-additivity additivity)
      (setf (logger-state-additivity state) additivity)
      (when adjust-p (adjust-logger logger)))
    (values additivity)))

(defun add-appender-internal (logger appender &optional (adjust-p t))
  (declare (type logger logger) (type appender appender))
  (let* ((state (current-state logger)))
    (unless (member appender (logger-state-appenders state))
      (push appender (logger-state-appenders state))
      (when adjust-p
        (adjust-logger logger))
      (appender-added logger appender)))
  (values))

(defun add-appender (logger appender)
  "Adds appender to the logger"
  (add-appender-internal logger appender))

(defmethod appender-added (logger appender)
  (incf (appender-logger-count appender))
  (push logger (appender-loggers appender))
  (assert (eql (appender-logger-count appender)
               (length (appender-loggers appender)))))

(defmethod appender-removed (logger appender)
  "Decrement logger count and call CLOSE-APPENDER if it reaches zero"
  (setf (appender-loggers appender)
        (remove logger
                (appender-loggers appender)))
  (decf (appender-logger-count appender))
  (assert (eql (appender-logger-count appender)
               (length (appender-loggers appender))))
  (when (zerop (appender-logger-count appender))
    (close-appender appender)))

(defun remove-appender-internal (logger appender &optional (adjust-p t))
  (let* ((state (current-state logger))
         (list (logger-state-appenders state)))
    (when (member appender list)
      (setf (logger-state-appenders state) (remove appender list))
      (when adjust-p
        (adjust-logger logger))
      (appender-removed logger appender))))

(defun remove-all-appenders-internal (logger &optional (adjust-p t))
  (let* ((state (current-state logger))
         (list (logger-state-appenders state)))
    (setf (logger-state-appenders state) nil)
    (when adjust-p
      (adjust-logger logger))
    (mapc (lambda (a) (appender-removed logger a))
          list)))

(defun remove-appender (logger appender)
  "Removes APPENDER from the logger. APPENDER-REMOVED generic function
will be called if appender was removed"
  (remove-appender-internal logger appender))

(defun remove-all-appenders (logger)
  "Removes all appenders from the logger."
  (remove-all-appenders-internal logger))

(defun logger-name (logger)
  "Return the name of the logger category itself (without parent loggers)"
  (if (%logger-parent logger) 
      (substr (%logger-category logger)
              (%logger-name-start-pos logger))
      "ROOT"))

(defun logger-file-logger (logger)
  "Return source file path-name where logger was instantiated"
  (when (typep logger 'file-logger)
    (file-logger-file logger)))

(defun logger-file (logger)
  "Return source file path-name where logger was instantiated"
  (cond ((and (typep logger 'file-logger)
              (setq logger (file-logger-file logger)))
         (source-file-logger-file logger))
        ((typep logger 'source-file-logger)
         (source-file-logger-file logger))))

(defun logger-file-namestring (logger)
  "Return source file name string where logger was instantiated"
  (cond ((and (typep logger 'file-logger)
              (setq logger (file-logger-file logger)))
         (source-file-logger-namestring logger))
        ((typep logger 'source-file-logger)
         (source-file-logger-namestring logger))))

(defun logger-name-length (logger)
  "Return length of logger itself (without parent loggers)"
  (- (length (%logger-category logger))
     (%logger-name-start-pos logger)))

(defun logger-categories (logger)
  "Return LOGGER categories starting from parent logger as a newly
consed list of strings"
  (let ((categories '()))
    (loop
      for l = logger then parent
      as parent = (%logger-parent l)
      while parent
      do (push (coerce (logger-name l)
                       'simple-string)
               categories))
    categories))

(defun adjust-all-loggers-state (new-len)
  (labels ((doit (logger)
             (declare (type logger logger))
             (let ((tmp (adjust-array (%logger-state logger)
                                      new-len
                                      :element-type 'logger-state)))
               (setf (svref tmp (1- new-len)) (make-logger-state)
                     (%logger-state logger) tmp)
               (map-logger-children #'doit logger))))
    (doit *root-logger*))
  (values))

(defun %%create-root-logger ()
  (let ((root (%create-root-logger :category "" :category-separator "")))
    (adjust-logger root)
    root))

(eval-when (:compile-toplevel)
  (when (boundp '*root-logger*) 
    (unless (ignore-errors
             (typep *root-logger* 'root-logger))
      ;; loading over old version, re-create the root logger
      (makunbound '*root-logger*)
      (makunbound '+self-logger+) 
      (makunbound '+self-meta-logger+))))

(defvar *root-logger*
  (%%create-root-logger)
  "The one and only root logger")

(defmethod make-load-form ((logger logger) &optional env)
  "Creates the logger when a logger constant is being loaded from a
compiled file"
  (declare (ignore env))
  (let ((pkg-start (logger-pkg-idx-start logger))
        (pkg-end (logger-pkg-idx-end logger))) 
    `(%get-logger ',(logger-categories logger)
                  ,(%logger-category-separator logger)
                  nil nil t
                  ,(logger-file logger)
                  ,(when (plusp pkg-start) (1- pkg-start))
                  ,(when (plusp pkg-end) (1- pkg-end))
                  ,(typep logger 'source-file-logger))))

(defun log-event-time ()
  "Returns the universal time of the current log event"
  (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
    (or *log-event-time* (setq *log-event-time* (get-universal-time)))))

(declaim (inline call-user-log-message))

(defun call-user-log-message (log-func stream)
  "Calls the user log function, binding *INSIDE-USER-LOG-FUNCTION* for
the duration to indicate that any errors must be re-thrown, rather
then disable the appender"
  (let ((*inside-user-log-function* t))
    (funcall log-func stream)
    (values)))



