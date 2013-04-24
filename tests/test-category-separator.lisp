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

(log4cl-test:defsubsuite :log4cl-test.dots)
(in-package :log4cl-test.dots)
(log4cl-test:subsuite-start) 

;;
;; Test in a different package, where logger category separator is dot
;; instead of new line

(deftest make-logger-by-list-of-categories ()
  "Test MAKE-LOGGER macro with static list of categories"
  (with-package-log-hierarchy
    (let ((logger (make-logger '(two three four))))
      (log4cl-test::basics logger)
      (is (equal (logger-category logger)
                 (concatenate 'string
                              (symbol-name 'two) "."
                              (symbol-name 'three) "."
                              (symbol-name 'four))))
      (is (equal (logger-name logger) (symbol-name 'four)))
      (is (eql (logger-depth logger) 3)))))

(deftest logger-name-via-dotted-keyword ()
  "Test that specifying logger name by a keyword containing dots is
correctly parsed into multiple loggers"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (is (null (logger-appenders *root-logger*)))
    (is (null (logger-log-level *root-logger*)))
    (is (eql +log-level-off+ (effective-log-level *root-logger*)))
    (let* ((logger (make-logger :one.two.three))
           (logger-one (make-logger :one))
           (logger-two (make-logger :one.two)))
      (is (eq (logger-parent logger) logger-two))
      (is (eq (logger-parent logger-two) logger-one))
      (is (null (logger-log-level logger)))
      (is (eql +log-level-off+ (effective-log-level logger)))
      ;; Add appender to the parent
      (add-appender logger-one (make-instance 'console-appender))
      ;; see that it got inherited
      (is (null (logger-appenders logger)))
      (is (not (null (effective-appenders logger)))))))

(deftest appender-additivity-1 ()
  "Test appender additivity works"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((one (make-logger :one))
           (one-two (make-logger :one.two))
           (one-two-three (make-logger :one.two.three))
           (a1 (make-instance 'counting-appender))
           (a2 (make-instance 'counting-appender))
           (a3 (make-instance 'counting-appender)))
      (log-config :i)
      (add-appender one a1)
      (add-appender one-two a2)
      (add-appender one-two-three a3)
      (is (log-info :logger one-two-three))
      (setf (logger-additivity one-two) nil)
      (is (log-info :logger one-two-three))
      (log-info :logger one "hey")
      (log-info :logger one-two "hey")
      (is (equal 1 (slot-value a1 'count)))
      (is (equal 1 (slot-value a2 'count)))
      (is (equal 0 (slot-value a3 'count)))
      (log-info :logger one-two-three "hey")
      (is (equal 1 (slot-value a1 'count)))
      (is (equal 2 (slot-value a2 'count)))
      (is (equal 1 (slot-value a3 'count)))
      (setf (logger-additivity one-two) t)
      (log-info :logger one-two-three "hey")
      (is (equal 2 (slot-value a1 'count)))
      (is (equal 3 (slot-value a2 'count)))
      (is (equal 2 (slot-value a3 'count)))
      ;; damn, our (reset-logger) uses HAVE-APPENDERS-FOR-LEVEL
      ;; rather then (EFFECTIVE-APPENDERS), to avoid consing, and
      ;; I forgot to update it for additivity... Test that (log-whatever)
      ;; expressions take additivity into account as well
      (log-config one-two-three :d)
      (is (log-debug :logger one-two-three))
      (setf (logger-additivity one-two-three) nil)
      (is (log-debug :logger one-two-three))
      (remove-appender one-two-three a3)
      (is (not (log-debug :logger one-two-three)))
      (setf (logger-additivity one-two-three) t)
      (setf (logger-additivity one-two) nil)
      (is (log-debug :logger one-two-three))
      (remove-appender one-two a2)
      (is (not (log-debug :logger one-two-three))))))

(deftest appender-additivity-2 ()
  "Test appender additivity works"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let* ((one (make-logger :one))
           (one-two (make-logger :one.two))
           (one-two-three (make-logger :one.two.three)))
      ;; Add appender to the root, and one-two
      (add-appender *root-logger* (make-instance 'console-appender))
      (add-appender one (make-instance 'console-appender))
      ;; see that it got inherited
      (is (null (logger-appenders one-two-three)))
      (is (not (null (effective-appenders one-two-three))))
      (is (equal 2 (length (effective-appenders one-two-three))))
      (is (equal 2 (length (effective-appenders one-two))))
      (is (equal 2 (length (effective-appenders one))))
      ;; now make  :one.two non-additive
      (setf (logger-additivity one-two) nil)
      (is (equal 0 (length (effective-appenders one-two-three))))
      (is (equal 0 (length (effective-appenders one-two))))
      (is (equal 2 (length (effective-appenders one))))
      ;; add logger to one-two
      (add-appender one-two (make-instance 'console-appender))
      (is (equal 1 (length (effective-appenders one-two-three))))
      (is (equal 1 (length (effective-appenders one-two))))
      (add-appender one-two (make-instance 'console-appender))
      (is (equal 2 (length (effective-appenders one-two-three))))
      (setf (logger-additivity one-two) t)
      (is (equal 4 (length (effective-appenders one-two-three)))))))

(deftest inherit-log-levels ()
  "Test log level inheritance"
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (let ((logger (make-logger :one.two.three))
          (parent (make-logger :one)))
      ;; verify no logging
      (is (eql +log-level-off+ (effective-log-level logger)))
      (is (null (log-warn)))
      (is (null (log-warn :logger logger)))
      ;; now set root log level to info, and verify that
      ;; the levels are right
      (setf (logger-log-level parent) :info)
      (is (null (logger-log-level logger)))
      (is (eql +log-level-info+ (effective-log-level logger)))
      ;; debugging is still off because of no appenders
      (is (null (log-debug :logger logger)))
      (is (null (log-warn :logger logger)))
      ;; add appender, verify debugging is now on
      (add-appender parent (make-instance 'console-appender))
      (is (log-warn :logger logger))
      (is (null (log-debug :logger logger)))
      ;; turn debug on on :one.two.three logger
      (setf (logger-log-level logger) :debug)
      ;; verify the level
      (is (eql +log-level-debug+ (logger-log-level logger)))
      (is (eql +log-level-debug+ (effective-log-level logger)))
      ;; verify both debug and info are on for logger
      (is (log-debug :logger logger))
      (is (log-warn :logger logger))
      ;; verify only info is on for root logger
      (is (null (log-debug :one)))
      (is (log-warn :one))
      ;; and same for logger parent
      (is (null (log-debug :one.two)))
      (is (log-warn :one.two))
      ;; set root logger off, verify that explicit setting on logger
      ;; is still in effect
      (setf (logger-log-level parent) :off)
      (is (null (log-debug)))
      (is (null (log-warn)))
      (is (log-debug :logger logger))
      (is (log-warn :logger logger))
      (values logger parent))))

(deftest make-logger-with-dotted-symbol-name ()
  (with-package-log-hierarchy
    (let ((logger (make-logger :one.two.three)))
      (log4cl-test::basics logger)
      (is (equal (logger-category logger)
                 (concatenate 'string
                              (package-name #.*package*)
                              "."
                              (symbol-name :one.two.three)))))))


