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

(defclass simple-layout (layout) ()
  (:documentation
   "Simple layout outputs log level and user message separated by
dash. For example: INFO - user log message"))

(declaim (inline write-log-level))

(defun write-log-level (level stream)
  "Print the log LEVEL's name to the STREAM"
  (write-string (log-level-to-string level) stream)
  (values))

(defmethod layout-to-stream ((layout simple-layout)
			     stream
                             logger
			     level
                             log-func)
  "Format the log message with the simple layout"
  (declare (ignore logger))
  (fresh-line stream)
  (write-log-level level stream)
  (write-string " - " stream)
  (call-user-log-message log-func stream)
  (terpri stream)
  (values))

