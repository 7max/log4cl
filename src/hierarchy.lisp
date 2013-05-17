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

#+sbcl
(declaim (sb-ext:always-bound
          *hierarchy* 
          *hierarchy-lock*
          *name-to-hierarchy*
          *hierarchy-max*
          *watcher-event-time*))

(declaim (type fixnum  *hierarchy-max* *hierarchy*)
	 (type hash-table *name-to-hierarchy*)
         (inline hierarchy-index
                 current-hierarchy)
         (ftype (function (fixnum) t) adjust-all-loggers-state)
         (ftype (function (t) t) %hierarchy-index hierarchy-index))

(defun current-hierarchy ()
  "Return the currently active hierarchy"
  (aref *hierarchies* *hierarchy*))

(defmacro with-hierarchies-lock (&body body)
  `(with-recursive-lock-held (*hierarchy-lock*)
     ,@body))

(defmacro with-hierarchy-lock ((&optional (hierarchy (current-hierarchy)))
                               &body body)
  `(with-recursive-lock-held ((slot-value ,hierarchy '%lock))
     ,@body))


(defun %hierarchy-index (name)
  (when (stringp name)
    (setq name (intern name)))
  (let ((h (or (gethash name *name-to-hierarchy*)
               (with-hierarchies-lock
                   (let ((h (make-instance 'hierarchy
                             :index *hierarchy-max*
                             :name name)))
                     (adjust-all-loggers-state (1+ *hierarchy-max*))
                     (setf (gethash name *name-to-hierarchy*) h)
                     (vector-push-extend h *hierarchies*)
                     (incf *hierarchy-max*)
                     h)))))
    (slot-value h 'index)))

(defun hierarchy-index (hierarchy)
  "Return the hierarchy index for the specified hierarchy. Hierarchy
must be already a number or a unique identifier suitable for comparing
using EQL. If hierarchy is a string, it will be interned in the current
package"
  (typecase hierarchy
    (number hierarchy)
    (hierarchy (slot-value hierarchy 'index))
    (t (%hierarchy-index hierarchy))))

(defun add-watch-token (token &key
                              (test #'equal) key
                              (hierarchy (current-hierarchy)))
  "Add unique watch token to the HIERARCHY, uniqueness is determined
by TEST and KEY arguments which are passed to FIND and REMOVE. Any
matching token is already present, the old token is removed and new
one is inserted.

The per-hierarchy lock is held doing the operation.

Automatically starts hierarchy watcher thread, if it was not already started
"
  (with-slots (watch-tokens) hierarchy
    (with-hierarchy-lock (hierarchy)
      ;; remove first, in case caller got the test wrong initially,
      ;; and is now stuck with extra values
      (setf watch-tokens (remove token watch-tokens :test test :key key))
      (push token watch-tokens))
    (start-hierarchy-watcher-thread)))

(defun remove-watch-token (token &key
                                 (test #'equal) key
                                 (hierarchy (current-hierarchy)))
  "Removes the watch token from the hierarchy, that matches the
specified KEY and TEST arguments, which are passed to REMOVE
function. Holds per-hierarchy lock doing its operation"
  (with-slots (watch-tokens) hierarchy
    (with-hierarchy-lock (hierarchy)
      (setf watch-tokens (remove token watch-tokens :test test :key key)))))


