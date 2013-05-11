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

(defclass property-configurator (property-parser)
  ((loggers)
   (additivity)
   (appenders))
  (:documentation "Class that holds configurator state while parsing
  the properties file"))

(defmethod shared-initialize :after ((config property-configurator) slots &rest initargs &key)
  (declare (ignore slots initargs))
  (with-slots (loggers additivity appenders)
      config
    (setf loggers '() additivity '() appenders '())))

(defclass logger-record (property-location)
  ((logger :initarg :logger)
   (level :initarg :level)
   (appender-names :initform nil :initarg :appender-names)))

(defclass delayed-instance (property-location)
  ((class :initform nil)
   (properties :initform nil)
   (extra-initargs :initform nil)
   (instance :initform nil)))

(defclass delayed-layout (delayed-instance)
  ((name :initarg :name)))

(defclass delayed-appender (delayed-instance)
  ((layout :initform nil)
   (name :initarg :name)
   (used :initform nil)))

(defmethod parse-property-keyword ((parser property-configurator)
                                   keyword
                                   tokens
                                   value)
  "Ignores anything that does not start with LOG4CL prefix, otherwise
calls PARSE-PROPERTY-TOKENS again (which will convert 2nd level of the
token name into the keyword and call this function again"
  (log-sexp keyword)
  (if (eq keyword :log4cl)
      (parse-property-tokens parser tokens value)
      (call-next-method)))


(defun parse-logger-helper (parser keyword tokens value)
  "Common helper that handles both .rootLogger= and .logger.name=
  lines"
  (with-slots (name-token-separator name-token-read-case loggers)
      parser
    (let ((logger
            (cond ((eq keyword :rootlogger)
                   (or (null tokens)
                       (log4cl-error "Root logger cannot have any sub-properties"))
                   *root-logger*)
                  (t (or tokens (log4cl-error "Logger name missing"))
                     (%get-logger
                      tokens name-token-separator name-token-read-case))))
          (value-tokens (split-string value "," t)))
      (unless (plusp (length value-tokens))
        (log4cl-error "Expecting LEVEL, [<APPENDER> ...] as the value"))
      (setf loggers (delete logger loggers :key #'car))
      (push (cons logger
                  (make-instance 'logger-record
                   :logger logger
                   :level (log-level-from-object (first value-tokens) *package*)
                   :appender-names
                   (mapcar (lambda (name)
                             (log-sexp name
                                       (convert-read-case
                                        name name-token-read-case))
                             (convert-read-case
                              name name-token-read-case))
                           (rest value-tokens))))
            loggers))))

(defmethod parse-property-keyword ((parser property-configurator)
                                   (keyword (eql :rootlogger))
                                   tokens
                                   value)
  (parse-logger-helper parser keyword tokens value))


(defmethod parse-property-keyword ((parser property-configurator)
                                   (keyword (eql :logger))
                                   tokens
                                   value)
  (parse-logger-helper parser keyword tokens value))


(defun intern-boolean (value)
  "Parse boolean value"
  (setq value (strip-whitespace value))
  (cond ((zerop (length value))
         nil)
        ((char= (char value 0) #\#)
         nil)
        ((member value '("nil" "none" "false" "off") :test 'equalp)
         nil)
        ((member value '("t" "true" "yes" "on") :test 'equalp)
         t)
        (t (log4cl-error "Invalid boolean value ~s" value))))

(defmethod parse-property-keyword ((parser property-configurator)
                                   (keyword (eql :additivity))
                                   tokens
                                   value)
  (with-slots (name-token-separator name-token-read-case additivity)
      parser
    (or tokens
        (log4cl-error "Missing logger name"))
    (let* ((logger
             (if (equalp tokens '("rootlogger"))
                 *root-logger*
                 (%get-logger
                  tokens name-token-separator name-token-read-case))))
      (setf additivity (delete logger additivity :key #'car))
      (push (cons logger (intern-boolean value))
            additivity))))

(defun intern-class-name (string)
  (let ((pos (position #\: string))
        (*print-readably* nil))
    (if (null pos)
        (find-symbol string)
        (let ((pkg (find-package (substr string 0 pos)))
              (only-external-p t))
          (when pkg
            (incf pos)
            (when (and (< pos (length string))
                       (char= (char string pos) #\:))
              (incf pos)
              (setf only-external-p nil))
            (log-sexp pkg only-external-p (substr string pos))
            (multiple-value-bind (symbol visibility)
                (find-symbol (substr string pos) pkg)
              (and (or (not only-external-p)
                       (equal visibility :external))
                   symbol)))))))

(defun set-delayed-instance-class (instance value)
  (with-slots (class name) instance
    (let ((new-class (intern-class-name value)))
      (or (null class) (log4cl-error "~a class specified twice" name))
      (or new-class (log4cl-error "~a class ~s not found" name value))
      (setf class new-class))))

(defun set-delayed-instance-property (instance tokens value)
  (with-slots (name properties)
      instance
    (let ((prop (intern (pop tokens) :keyword)))
      (or (null tokens) (log4cl-error "~a expecting a single property" name))
      (or (null (assoc prop properties))
          (log4cl-error "~a property ~s specified twice" name prop))
      (push (list prop value (make-instance 'property-location)) properties))))

(defmethod parse-property-keyword ((parser property-configurator)
                                   (keyword (eql :appender))
                                   tokens
                                   value)
  (with-slots (name-token-separator name-token-read-case appenders)
      parser
    (when (null tokens)
      (log4cl-error "appender should be followed by appender name"))
    (let* ((name (strip-whitespace (pop tokens)))
           (appender (or (cdr (assoc name appenders :test 'equal))
                         (cdar (push (cons
                                      name
                                      (make-instance 'delayed-appender
                                       :name (format nil "appender ~A" name)))
                                     appenders)))))
      (with-slots (class layout initargs)
          appender
        (cond ((null tokens)
               (set-delayed-instance-class
                appender (convert-read-case
                          (strip-whitespace value) name-token-read-case)))
              ((equal (first tokens) (symbol-name :layout))
               (pop tokens)
               (or layout
                   (setf layout (make-instance 'delayed-layout
                                 :name
                                 (format nil "~A's appender layout"
                                         name))))
               (if (null tokens)
                   (set-delayed-instance-class
                    layout (convert-read-case
                            (strip-whitespace value) name-token-read-case))
                   (set-delayed-instance-property layout tokens value)))
              (t (set-delayed-instance-property appender tokens value)))))))


(defun create-delayed-instance (instance)
  "First filter all properties through through INSTANCE-PROPERTY-FROM-STRING,
and then create the instance"
  (with-property-location (instance)
    (with-slots (instance name class properties extra-initargs)
        instance
      (setf instance
            (make-instance (or class (log4cl-error "Class not specified for ~a" name))))
      ;; need to do it twice to apply properties, since property parsing
      ;; stuff is specialized on the instance class
      (setf instance (apply #'reinitialize-instance
                            instance
                            (append
                             (loop for (prop value location) in properties
                                   appending
                                      (with-property-location (location)
                                        (list prop (property-initarg-from-string
                                                    instance prop value))))
                             extra-initargs))))))

(defmethod parse-property-stream :after ((configurator property-configurator) stream)
  "Parse the stream and apply changes to logging configuration"
  (declare (ignore stream))
 (with-log-indent ()
    (with-slots (appenders loggers additivity)
        configurator
      ;; for each logger, see that logger's in appender list were defined
      (loop for (logger . rec) in loggers
            do (with-property-location (rec)
                 (log-sexp rec %parse-line %parse-line-num)
                 (dolist (name (slot-value rec 'appender-names))
                   (or (assoc name appenders :test 'equal)
                       (log4cl-error "Logger ~a refers to non-existing appender ~s"
                              logger name))
                   (setf (slot-value (cdr (assoc name appenders :test 'equal))
                                     'used) t))))
      ;; create the appenders, we do this before mucking with loggers,
      ;; in case creating an appender signals an error
      (loop for (nil . a) in appenders
            if (slot-value a 'used)
            do (with-slots (layout extra-initargs) a
                 (when layout
                   (setf extra-initargs
                         `(:layout ,(create-delayed-instance layout))))
                 (create-delayed-instance a)))
      (loop for (logger . rec) in loggers do
               (progn
                 (log-sexp "Doing " logger (slot-value rec 'level))
                 (when (assoc logger additivity)
                   (set-additivity logger (cdr (assoc logger additivity)) nil))
                 (remove-all-appenders-internal logger nil)
                 (set-log-level logger (slot-value rec 'level) nil)
                 (dolist (name (slot-value rec 'appender-names))
                   (add-appender-internal
                    logger (slot-value (cdr (assoc name appenders :test 'equal))
                                       'instance)
                    nil))
                 (adjust-logger logger))))))

(defmethod property-initarg-from-string (instance property value)
  "Generic implementation for numbers, boolean and string properties,
that calls PROPERTY-ALIST function to determine what kind of
property it is. Signals error if property is not in the list"
  (let* ((props-alist (property-alist instance))
         (type (third (assoc property props-alist))))
    (case type
      ((number :number) (parse-integer (strip-whitespace value)))
      ((boolean :boolean) (intern-boolean (strip-whitespace value)))
      ((string :string) value)
      (:string-skip-whitespace
       (loop for idx from 0 below (length value)
             for c = (char value idx)
             while (member c '(#\Space #\Tab) :test 'char=)
             finally (return (coerce (substr value idx) 'simple-string))))
      (t (log4cl-error "Unknown property ~s for class ~s" property instance)))))

(defgeneric configure (configurator source &key &allow-other-keys)
  (:documentation "Configure the logging system from specified source"))

(defmethod configure ((configurator property-configurator)
                      (s stream) &key)
  "Configures logging from the specified stream"
  (parse-property-stream configurator s))

(defclass property-configurator-file-watch ()
  ((filespec :initarg :filespec :accessor filespec-of)
   (time :initarg :time)
   (configurator :initarg :configurator)))

(defmethod print-object ((watch property-configurator-file-watch) stream)
  (print-unreadable-object (watch stream :type t)
    (prin1 (slot-value watch 'filespec) stream)))

(defmethod configure ((configurator property-configurator) filespec &key auto-reload)
  "Configures logging from the specified file. If AUTO-RELOAD is
non-NIL, then after initial configuration will watch the file for
modifications and re-configure when it changes. Note that auto-reload
will not be configured if initial configuration signaled a error"
  (let ((filespec (merge-pathnames filespec)))
    (with-open-file (s filespec)
      (configure configurator s))
    (when auto-reload
      (add-watch-token (make-instance 'property-configurator-file-watch
                        :filespec filespec
                        :time (file-write-date filespec)
                        :configurator configurator)
                       :test
                       (lambda (watch1 watch2)
                         (and (typep watch2 'property-configurator-file-watch)
                              (equal (slot-value watch1 'filespec)
                                     (slot-value watch2 'filespec))))))))

(defmethod watch-token-check ((token property-configurator-file-watch))
  "Checks properties file write time, and re-configure from it if it changed.
Catches and does not re-signal PROPERTY-PARSER-ERROR, so watching the
file continues if newly modified file had an error"
  (with-slots (filespec time configurator) token
    (let ((new-time (file-write-date filespec)))
      (when (/= new-time time)
        (setf time new-time)
        (handler-case
            (progn (configure configurator filespec)
                   (log-info '(log4cl) "Re-configured logging from ~A"
                             (enough-namestring filespec)))
          (property-parser-error (c)
            (log-error '(log4cl)
                       "Re-configuring from ~A failed:~%~A"
                       (enough-namestring filespec) c)))))))
