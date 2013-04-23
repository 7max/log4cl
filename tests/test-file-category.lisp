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

(defpackage :log4cl-test.file-category
  (:use :cl :stefil :log4cl-impl :log4cl-test))

(in-package :log4cl-test.file-category)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (log4cl-impl:log-setup :category-separator ".")
  (in-suite log4cl-test:test)
  (defsuite* test-file-categories))

;; (setup-logging)

(deftest test-file-category-1 ()
  (with-package-log-hierarchy
    (clear-logging-configuration) 
    (let ((no-file-logger (make-logger '(one two three)))
          (has-file-logger (make-logger :foo.bar)))
      (is (null (logger-file no-file-logger)))
      (is (pathnamep (logger-file has-file-logger)))
      (is (equal "test-file-category.lisp" (logger-file-namestring has-file-logger)))
    
      ;; cross test that %c and %C ignore each other stuff
      (test-pattern-layout "%c" (make-expected (list (package-name *package*) :foo.bar) ".")
                           :logger has-file-logger)

      ;; just the package
      (test-pattern-layout "%g" (make-expected (list (package-name *package*)) ".")
                           :logger has-file-logger)

      ;; everything else but the package
      (test-pattern-layout "%C" (make-expected (list :foo.bar) ".")
                           :logger has-file-logger)

      ;; using : as separator because '(one two three) logger was
      ;; instantiated in earlier test with default package config
      (test-pattern-layout "%c" (make-expected '(one two three) ".")
                           :logger no-file-logger)
      (test-pattern-layout "%C" (make-expected '(one two three) ".")
                           :logger no-file-logger)
      (test-pattern-layout "%g" "" :logger no-file-logger))))


(deftest test-file-category-3 ()
  ;; Test %F (file name) pattern
  (with-package-log-hierarchy
    (clear-logging-configuration) 
    (let ((logger (make-logger :test-file-category-3)))
      (is (pathnamep (logger-file logger)))
      (is (equal "test-file-category.lisp" (logger-file-namestring logger)))
    
      (test-pattern-layout "%c" (make-expected (list (package-name *package*) :test-file-category-3) ".")
                           :logger logger)
      (test-pattern-layout "%F" "test-file-category.lisp" :logger logger))))

(deftest test-file-category-4 ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (is (not (log-debug)))
    (is (not (log-warn)))
    (let* ((logger-1 (make-logger :one.two.three))
           (logger-2 (make-logger :one)))
      (dolist (logger (list logger-1 logger-2)) 
        (is (logger-file-logger logger)) 
        (is (equal "test-file-category.lisp" (logger-file-namestring logger))))

      (is (eq logger-2 (logger-parent (logger-parent logger-1))))
      (is (eq (logger-file-logger logger-1)
              (logger-file-logger logger-2)))

      (log-config *root-logger* :warn :console)
      (is (log-warn :logger logger-1))
      (is (log-warn :logger logger-2))
      (is (not (log-info :logger logger-1)))
      (log-config logger-2 :info)
      (is (log-info :logger logger-1))
      (is (log-info :logger logger-2)))))


