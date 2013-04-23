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

(in-package :log4cl-test.file-category)
(in-suite test)

(deftest test-file-category-5 ()
  (with-package-log-hierarchy
    (clear-logging-configuration)
    (is (not (log-debug)))
    (is (not (log-warn)))
    (let* ((logger-1 (make-logger :one.two.four))
           (logger-2 (logger-parent (logger-parent logger-1))))
      (dolist (logger (list logger-1 logger-2)) 
        (is (logger-file-logger logger))) 

      (is (equal "test-file-category-2.lisp" (logger-file-namestring logger-1)))
      (is (equal "test-file-category.lisp" (logger-file-namestring logger-2)))

      (is (not (eq (logger-file-logger logger-1)
                   (logger-file-logger logger-2))))

      (log-config *root-logger* :warn :console)

      (is (log-warn logger-1))
      (is (log-warn logger-2))

      (is (not (log-info logger-1)))
      (is (not (log-info logger-2)))

      ;; test that we inherit from file of the leaf logger
      (log-config (logger-file-logger logger-2) :info)

      (is (log-info logger-2))
      (is (not (log-info logger-1)))

      ;; configure this file logger to debug, see that logger
      ;; instantiated here is inheriting it
      (log-config (logger-file-logger logger-1) :debug)
      
      (is (not (log-debug logger-2)))
      (is (log-debug logger-1))
      (is (log-info logger-2))

      ;; Now make the <package>.one logger :warn, check that this overrides
      ;; anything set in the file

      (log-config logger-2 :warn)

      (is (not (log-info logger-1)))
      (is (not (log-info logger-2)))

      (is (log-warn logger-1))
      (is (log-warn logger-2))

      ;; back again

      (log-config logger-2 :unset)
      (is (not (log-debug logger-2)))
      (is (log-debug logger-1))
      (is (log-info logger-2))

      ;; unset file loggers, and verify we go back to root

      (log-config (logger-file-logger logger-1) :unset)
      (log-config (logger-file-logger logger-2) :unset)

      (is (log-warn logger-1))
      (is (log-warn logger-2))

      (is (not (log-info logger-1)))
      (is (not (log-info logger-2))))))
