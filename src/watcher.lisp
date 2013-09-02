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

(defvar *watcher-thread-bindings* nil
  "Extra bindings for watcher thread")

;; Not sure if this stuff is needed, but since we exit hierarchy
;; watcher thread by asynchronous bt:destroy-thread, and SBCL provides
;; support for it, no penalty in being extra careful

(defmacro %without-interrupts (&body body)
  #+sb-thread `(sb-sys:without-interrupts ,@body)
  #-sb-thread`(progn ,@body))

(defmacro %allow-with-interrupts (&body body)
  #+sb-thread `(sb-sys:allow-with-interrupts ,@body)
  #-sb-thread`(progn ,@body))

(defmacro %with-local-interrupts (&body body)
  #+sb-thread `(sb-sys:with-local-interrupts ,@body)
  #-sb-thread`(progn ,@body))

(defun call-with-logged-problems (context thunk)
  (handler-case (funcall thunk)
    (error (condition)
      (log-error :logger +self-meta-logger+
                 "~@<Caught ~S during ~S: ~A; Continuing.~@:>"
                 (type-of condition) context condition))
    (warning (condition)
      (log-warn :logger +self-meta-logger+
                "~@<Caught ~S during ~S: ~A; Continuing.~@:>"
                (type-of condition) context condition))))

(defmacro with-logged-problems (context &body body)
  `(call-with-logged-problems ',context (lambda () ,@body)))

(defun start-hierarchy-watcher-thread ()
  (unless *watcher-thread*
    (let ((logger (make-logger '(log4cl))))
      (bordeaux-threads:make-thread
       (lambda ()
         ;; prevent two watcher threads from being started due to race
         (%without-interrupts
           (when (with-hierarchies-lock
                   (cond (*watcher-thread*
                          (%with-local-interrupts
                            (log-debug "Watcher thread already started")
                            nil))
                         (t (setq *watcher-thread* (bt:current-thread)))))
             (unwind-protect
                  (%with-local-interrupts
                    (handler-case
                        (progn
                          (log-info :logger logger "Hierarchy watcher started")
                          (loop
                            (let ((*watcher-event-time* (get-universal-time)))
                              (hierarchy-watcher-once))
                            (sleep *hierarchy-watcher-heartbeat*)))
                      (error (e)
                        (log-error :logger logger "Error in hierarchy watcher thread:~%~A" e))))
               (with-hierarchies-lock
                 (setf *watcher-thread* nil))
               (%with-local-interrupts (log-info :logger logger "Hierarchy watcher thread ended"))))))
       :name "Hierarchy Watcher"
       :initial-bindings
       `((*hierarchy* . 0)
         (*package* . (find-package :log4cl-impl))
         ,@*watcher-thread-bindings*)))))


(defun hierarchy-watcher-do-one-token (hier token)
  (with-slots (name) hier
    (with-log-hierarchy (hier)
      (handler-bind ((serious-condition
                       (lambda (c)
                         (remove-watch-token token :test #'eq)
                         (log-error
                          '(log4cl)
                          "WATCH-TOKEN-CHECK in ~S hierarchy signaled error for token ~S~%~A"
                          name token c)
                         (return-from hierarchy-watcher-do-one-token))))
        (watch-token-check token)))))

(defun hierarchy-watcher-once ()
  "Do one iteration of watcher loop."
  (map nil
       (lambda (hier)
         (dolist (token (slot-value hier 'watch-tokens))
           (hierarchy-watcher-do-one-token hier token)))
       *hierarchies*))

(defun stop-hierarchy-watcher-thread ()
  (let ((thread (with-hierarchies-lock *watcher-thread*)))
    (when thread
      (with-logged-problems '(stop-hierarchy-watcher-thread :destroy-thread)
        (bt::destroy-thread thread))
      (with-logged-problems '(stop-hierarchy-watcher-thread :join-thread)
        (bt:join-thread thread)))))

(defun maybe-start-watcher-thread ()
  (with-hierarchies-lock
    (let* ((tokens
             (loop for h :across *hierarchies* :append (watch-tokens h)))
           (have-appenders-p
             (some (lambda (x) (and (typep x 'stream-appender)
                                    (not (slot-value x 'immediate-flush))))
                   tokens)))
      (when have-appenders-p
        (start-hierarchy-watcher-thread)))))

(defun save-hook ()
  "Flushes all existing appenders, and stops watcher thread"
  (with-logged-problems (save-hook :flush-all-appenders)
    (flush-all-appenders))
  (with-logged-problems (save-hook :flush-all-appenders)
    (save-all-appenders))
  (with-logged-problems (save-hook :stop-hierarch-watcher-thread)
    (stop-hierarchy-watcher-thread)))

(defun exit-hook ()
  "Flushes all existing appenders"
  (with-logged-problems (exit-hook :flush-all-appenders)
    (flush-all-appenders)))

(defun init-hook ()
  "Starts watcher thread if any existing appenders don't
have :immediate-flush option"
  (with-logged-problems (init-hook :maybe-start-watch-thread)
    (maybe-start-watcher-thread))
  (with-logged-problems (init-hook :reinitialize-this-console-appender)
    (dolist (appender (all-appenders))
      (when (typep appender 'this-console-appender)
        (setf (slot-value appender 'stream) *global-console*)
        (reinitialize-instance appender)))))

(defun all-appenders (&optional (all-hierarchies t))
  "Return all existing appenders in all hierarchies"
  (let ((appenders '()))
    (labels ((collect-appenders (x)
               (dolist (a (logger-appenders x))
                 (push a appenders)))
             (collect-hier (x)
               (let ((*hierarchy* x))
                 (collect-appenders *root-logger*)
                 (map-logger-descendants #'collect-appenders *root-logger*))))
      (if all-hierarchies
          (with-hierarchies-lock (dotimes (i *hierarchy-max*) (collect-hier i)))
          (collect-hier *hierarchy*))
      appenders)))

(defun start/stop-watcher-hook (cmd &optional arg)
  (ecase cmd
    (:stop (let ((thread (with-hierarchies-lock *watcher-thread*)))
             (when thread
               (stop-hierarchy-watcher-thread)
               (funcall arg))))
    (:start (start-hierarchy-watcher-thread))))

#+sbcl (pushnew 'save-hook sb-ext:*save-hooks*)
#+sbcl (pushnew 'exit-hook sb-ext:*exit-hooks*)
#+sbcl (pushnew 'init-hook sb-ext:*init-hooks*)
