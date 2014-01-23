;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2013, 2014, Jan Moringen. All rights reserved.
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

(defclass syslog-appender (appender)
  ((name :initarg :name :type string)
   (include-pid? :initarg :include-pid? :type boolean))
  (:default-initargs
   :layout (make-instance 'pattern-layout :conversion-pattern "%m"))
  (:documentation
   "An appender that writes log messages to the syslog.

The identity of the syslog connection is controlled by the :name
initarg and defaults to

  (lisp-implementation-type)

The :include-pid? initarg controls whether log entries produced by the
syslog connection should include the process id (PID). The default is
true."))

(defmethod shared-initialize :after ((instance   syslog-appender)
                                     (slot-names t)
                                     &key
                                     (name         (lisp-implementation-type))
                                     (include-pid? t))
  (sb-posix:openlog name (logior sb-posix:log-user
                                 (if include-pid? sb-posix:log-pid 0))))

(defmethod reinitialize-instance :before ((instance syslog-appender) &key)
  (sb-posix:closelog))

(defmethod close-appender ((appender syslog-appender))
  (sb-posix:closelog))

(defmethod appender-do-append ((appender syslog-appender) logger level log-func)
  (let* ((syslog-level (%log4cl-level->syslog-level level))
         (layout       (appender-layout appender))
         (message
           (with-output-to-string (stream)
             (layout-to-stream layout stream logger level log-func))))
    (sb-posix:syslog syslog-level "~A" message)))

(defmethod property-alist ((instance syslog-appender))
  (append (call-next-method)
          '((:name name :string-skip-whitespace)
            (:include-pid? include-pid? boolean))))

;; Utility functions

(defun %log4cl-level->syslog-level (level)
  (let ((level/keyword (aref +log-level-to-keyword+ level)))
    (ecase level/keyword
      (:fatal sb-posix:log-crit)
      (:error sb-posix:log-err)
      (:warn  sb-posix:log-warning)
      (:info  sb-posix:log-info)
      ((:debu1 :debu2 :debu3 :debu4 :debu5 :debu6 :debu7 :debu8 :debu9)
       sb-posix:log-debug))))
