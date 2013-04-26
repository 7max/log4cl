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



(log4cl-test:defsubsuite :log4cl-test.file-category)
(in-package :log4cl-test.file-category)
(log4cl-test:subsuite-start)

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


(deftest logger-package-override ()
  "If a logger is first retrieved via exploit category, ie
\(LOG:LOGGER '(P A)\) it won't have any package info, and %g (package
category) pattern format will print empty string for the logger.

Test that if later the same logger is referenced from the package P,
for example from (defun P:A () (LOG-DEBUG whatever)), that we store
the package info, even if logger already exist, and did not have one."

  (let* ((name-sym (gensym "SOME-PACKAGE.ONE"))
         (logger (eval `(make-logger '(,@(log4cl::split-string (symbol-name name-sym) ".")
                                       one two)))))
    (test-pattern-layout "%g" "" :logger logger)
    ;; now instantiate it from a package
    (let* ((*package* (find-package :keyword))
           (form-string (with-output-to-string (*standard-output*)
                          (write 
                           `(progn
                              (in-package ,name-sym)
                              (make-logger :one.two))
                           :readably t)))
           (*package* (make-package name-sym))
           (form (read-from-string form-string))
           (logger2 (eval form)))
      (is (eq logger logger2)) 
      (test-pattern-layout "%g" (symbol-name name-sym)
                           :logger logger))))

