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

(defun ccl-get-block-name (env)
  (declare (ignore env))
  (labels ((maybe-fix-ccl-method (elem)
             (if (and (consp elem)
                      (= 3 (length elem))
                      (keywordp (first elem))
                      (symbolp (second elem))
                      (consp (third elem)))
                 `(,(second elem)
                   ,(first elem)
                   ,@(cddr elem))
                 elem)))
    (when ccl::*nx-current-function*
      (let ((name (ccl::afunc-name ccl::*nx-current-function*)))
        (fix-method-spec-list 
         (cond ((not (consp name)) name)
               ((eq :internal (first name))
                (let ((names (reverse (rest name))))
                  (append (list (first names))
                          (rest names))))
               (t (maybe-fix-ccl-method name))))))))

(defmethod enclosing-scope-block-name (package env)
  "Return the enclosing block name suitable for naming a logger"
  (declare (ignore package))
  (ccl-get-block-name env))
