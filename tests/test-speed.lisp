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

(in-package #:log4cl-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-root-suite) 
  (defsuite* test-speed))

;; Logging case, with default optimizations
;;
;; (time (log4cl-test::speed-test-to-file :iterations 10000000))
;;
;; Evaluation took:
;;   27.621 seconds of real time
;;   27.632799 seconds of total run time (27.606803 user, 0.025996 system)
;;   100.04% CPU
;;   37 lambdas converted
;;   66,169,657,401 processor cycles
;;   1,981,984 bytes consed
;;
;; java test 10000000  8.74s user 15.62s system 101% cpu 24.002 total
;;
;; Times with other configurations:
;;   - *print-pretty* NIL (speed 3) (safety 1), 25.5 secs, 1 slower then java
;;
;; No logging case (note 10x iterations the logging case)
;;
;; (time (log4cl-test::speed-test-to-file :iterations 100000000
;;   :root-logger-level :info))
;;
;; Evaluation took:
;;   1.099 seconds of real time
;;   1.097834 seconds of total run time (1.091835 user, 0.005999 system)
;;   99.91% CPU
;;   37 lambdas converted
;;   2,567,753,657 processor cycles
;;   1,079,664 bytes consed
;;
;; java -Droot.level=INFO test 100000000  15.07s user 0.85s system 101% cpu 15.757 total
;;

(deftest speed-test-to-file (&key (filespec "/dev/null")
                                  (external-format :default)
                                  (layout (make-instance 'simple-layout))
                                  (iterations 1000000)
                                  (root-logger-level :debug))
  (with-package-log-hierarchy
    (with-open-file (stream filespec :direction :output
                                     :if-exists :supersede
                                     :external-format external-format)
      (clear-logging-configuration)
      (add-appender *root-logger* (make-instance 'fixed-stream-appender
                                   :layout layout
                                   :stream stream
                                   :immediate-flush nil
                                   :flush-interval nil))
      (setf (logger-log-level *root-logger*) root-logger-level)
      (dotimes (cnt iterations)
        (log-debug :log4cl-test.category "iter=~d" cnt)))))
