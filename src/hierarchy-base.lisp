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

(defvar *hierarchy-max* 1
  "Number of hierarchies registered with the log4cl library. Each
hierarchy has independent configuration with regards to log levels and
appenders")

(defvar *hierarchy* 0
  "Active hierarchy index. All logging functions use logger state
indexed by this variable. Can be assigned directly or ")

(defvar *hierarchy-lock*
  (make-recursive-lock "hierarchy-lock")
  "Global lock for changing logging configuration")

(defvar *hierarchy-watcher-heartbeat* 1
  "How long hierarchy watcher thread sleeps between calling
WATCHER-HOOK of each hierarchy")

(defvar *watcher-event-time* 0
  "Universal time of the current watcher heartbeat")

(defvar *watcher-thread* nil
  "The hierarchy watcher thread")

(defclass hierarchy ()
  ((name :initarg :name)
   ;; the index into logger-state array in each logger
   (index :initarg :index)
   ;; List of objects, for whom the watcher thread will call
   ;; WATCH-TOKEN-CHECK method every *HIERARCHY-WATCHER-HEARTBEAT*
   ;; seconds
   ;;
   ;; Used for auto-reloading the modified files in
   ;; PROPERTY-CONFIGURATOR but can be used for other stuff.
   (watch-tokens :initform nil :accessor watch-tokens)
   (%lock :initform (make-lock))))

(defvar *hierarchies*
  (make-array 1 :adjustable t :fill-pointer t
                :initial-contents
                `(,(make-instance 'hierarchy :name :default :index 0)))
  "Array of all hierarchies in the system")


(defvar *name-to-hierarchy* (let* ((table (make-hash-table))
                                   (h (aref *hierarchies* 0)))
                              (setf (gethash (slot-value h 'name) table) h)
                              table)
  "EQL hash table mapping hierarchy identifier to hierarchy index")


(defgeneric watch-token-check (token)
  (:documentation "Will be called on each member of WATCH-TOKENS list
when hierarchy watcher thread is started. If a unhandled condition is
signaled from this function the watcher thread will remove
corresponding token from the list"))



