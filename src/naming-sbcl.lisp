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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun safe-intern (name)
    (let* ((name (string-upcase name))
           (pos (position #\: name))
           (package-name (subseq name 0 pos))
           (symbol-name (subseq name (+ pos
                                        (if (char= #\: (char name (1+ pos)))
                                            2 1))))
           (pkg (find-package package-name)))
      (find-symbol symbol-name pkg))))

(sb-ext:defglobal +sbcl-wrapper-names+
  (remove nil (mapcar #'safe-intern
                      '("sb-c::hairy-arg-processor"
                        "sb-c::varargs-entry"
                        "sb-c::xep" "sb-c::tl-xep"
                        "sb-c::&more-processor"
                        "sb-c::top-level-form"
                        "sb-c::&optional-processor"))))

(sb-ext:defglobal +sbcl-wrapper-ignore+
    (remove nil (mapcar #'safe-intern
                        '("sb-c::.anonymous."
                          "sb-thread::with-mutex-thunk"))))


(defun include-block-debug-name? (debug-name)
  "Figures out if we should include the debug-name into the stack of
nested blocks..  Should return the symbol to use.

For now SBCL seems to use:

  SYMBOL => normal defun block
  (LABELS SYMBOL) => inside of labels function
  (FLET SYMBOL)   => inside of flet function
  (LAMBDA (arglist) => inside of anonymous lambda
  (SB-PCL::FAST-METHOD SYMBOL ...) for defmethod
  (SB-PCL::VARARGS-ENTRY (SB-PCL::FAST-METHOD SYMBOL )) for defmethod with &rest parametwer
  (SB-C::HAIRY-ARG-PROCESSOR SYMBOL) => for functions with complex lambda lists

In all of the above cases except LAMBDA we simply return SYMBOL, for
LAMBDA we return the word LAMBDA and NIL for anything else.

Example: As a result of this default logger name for SBCL for the
following form:

   (defmethod foo ()
     (labels ((bar ()
                (funcall (lambda ()
                           (flet ((baz ()
                                    (log-info \"test\")))
                             (baz))))))
       (bar)))

will be: package.foo.bar.baz

"
  (if (symbolp debug-name)
      (when (and (not (member debug-name +sbcl-wrapper-ignore+))
                 (symbol-package debug-name)
                 (not (equal 0 (search "CLEANUP-FUN-"
                                       (symbol-name debug-name)))))
        debug-name)
      (cond 
        ((member (first debug-name) '(flet labels lambda))
         (include-block-debug-name? (second debug-name)))
        ((eq 'labels (first debug-name))
         (include-block-debug-name? (second debug-name)))
        ((eq 'flet (first debug-name))
         (include-block-debug-name? (second debug-name)))
        ;; (lambda 'lambda)
        ((eq 'sb-pcl::fast-method (first debug-name))
         (rest debug-name))
        ((member (first debug-name) +sbcl-wrapper-names+)
         (include-block-debug-name? (second debug-name)))
        ((eq (first debug-name) 'setf)
         debug-name))))

(defun sbcl-get-block-name  (env)
  "Return a list naming SBCL lexical environment. For example when
compiling local function FOO inside a global function FOOBAR, will
return \(FOOBAR FOO\)"
  (let* ((names-from-lexenv
           (nreverse
            (loop
              with last = nil
              as lambda = (sb-c::lexenv-lambda env)
              then (sb-c::lambda-parent lambda)
              while lambda
              as debug-name = (include-block-debug-name? (sb-c::leaf-debug-name lambda))
              if (and debug-name (not (eq last debug-name)))
              collect debug-name
              and do (setq last debug-name))))
         (name (or names-from-lexenv sb-pcl::*method-name*)))
    (fix-method-spec-list name)))


(defmethod enclosing-scope-block-name (package env)
  "Return the enclosing block name suitable for naming a logger"
  (declare (ignore package))
  (when env (sbcl-get-block-name env)))
