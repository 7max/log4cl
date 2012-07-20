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

(in-suite log4cl-test:test)

(defsuite* test-file-categories)

;; (setup-logging)

(deftest test-file-category-1 ()
  (let ((no-file-logger (log4cl-impl::%get-logger '(one "file.lisp" two three) ":" nil nil t))
        (has-file-logger (log4cl-impl::%get-logger '(one "file.lisp" two four) ":" nil nil t '(1))))
    (is (null (logger-file no-file-logger)))
    (is (equal "file.lisp" (logger-file has-file-logger)))
    
    ;; cross test that %c and %C ignore each other stuff
    (test-pattern-layout "%c" (make-expected '(one "file.lisp" two four) ":")
                         :logger has-file-logger)

    (test-pattern-layout "%C" (make-expected '(one "file.lisp" two three) ":")
                         :logger no-file-logger)))

(deftest test-file-category-2 ()

  (let ((cats '(one two three four five six)))
    (dolist (file-insert-pos  (list 0 1 2 (length cats))) 
      (let* ((cats+file (append (subseq cats 0 file-insert-pos)
                                (list "thefile.lisp")
                                (subseq cats file-insert-pos)))
             (logger (log4cl-impl::%get-logger cats+file ":" nil nil t (list file-insert-pos))))
        (is (equal "thefile.lisp" (logger-file logger)))
    
        ;; now test that various combinations of %C completely ignore the file
        (test-pattern-layout "%C" (make-expected cats ":")
                             :logger logger)

        ;; try all combinations of %C{n}
        (loop for i from 0 to (length cats)
              do (test-pattern-layout (format nil "%C{~d}" i)
                                      (make-expected (subseq cats (- (length cats) i)) ":")
                                      :logger logger))
    
        ;; similarly for custom separator
        (loop for i from 0 to (length cats)
              do (test-pattern-layout (format nil "%C{~d}{---}" i)
                                      (make-expected (subseq cats (- (length cats) i)) "---")
                                      :logger logger))
    
        ;; all combinations of %C{start,count}
        (dotimes (start (length cats))
          (dotimes (num (length cats))
            (test-pattern-layout (format nil "%C{~d,~d}{---}" start num)
                                 (make-expected (subseq cats start (if (plusp num)
                                                                       (min (+ start num)
                                                                            (length cats)))) "---")
                                 :logger logger)))))))
