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

(defclass layout () ()
  (:documentation "Abstract layout class"))

(defgeneric layout-to-stream (layout stream logger level log-func)
  (:documentation
   "Prints the log message to the specified stream. log message can is
specified indirectly by LOG-FUNC argument, which is a callable object
that accepts a stream and writes log message to it"))

(defmethod property-alist ((instance layout))
  "Abstract layout has no properties"
  '())

