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

(defpackage :log4cl.system
  (:use #:cl #:asdf)
  (:export #:load-log4slime))

(in-package :log4cl.system)

(defsystem :log4cl
  :version "1.1.0"
  :depends-on (:bordeaux-threads)
  :components
  ((module "src" :serial t
                 :components ((:file "impl-package")
                              (:file "defs")
                              (:file "naming")
                              #+sbcl (:file "naming-sbcl")
                              #+ccl (:file "naming-ccl")
                              (:file "appender-base")
                              (:file "hierarchy-base")
                              (:file "hierarchy")
                              (:file "logger")
                              (:file "logging-macros")
                              (:file "self-logger")
                              (:file "layout")
                              (:file "simple-layout")
                              (:file "pattern-layout")
                              (:file "appender")
                              (:file "watcher")
                              (:file "configurator")
                              (:file "property-parser")
                              (:file "property-configurator")
                              (:file "package")))))

(defsystem :log4cl-test
  :version "0.9.3"
  :depends-on (:log4cl :stefil)
  :components ((:module "tests"
                :serial t
                :components ((:file "test-defs")
                             (:file "test-logger")
                             (:file "test-category-separator")
                             (:file "test-layouts")
                             (:file "test-appenders")
                             (:file "test-configurator")
                             (:file "test-speed")
                             (:file "test-file-category")
                             (:file "test-compat")))))

(defmethod perform ((op test-op) (system (eql (find-system :log4cl))))
  (operate 'load-op :log4cl-test)
  (let ((*package* (find-package :log4cl-test)))
    (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'log4cl-test::test)")))
  (values))

(defmethod perform :after ((op load-op) (system (eql (find-system :log4cl))))
  (when (find-package :log4cl)
    (let ((*package* (find-package :log4cl))
          (foo (find-symbol (symbol-name '#:%fix-root-logger-check)
                            (find-package :log4cl))))
      (when foo
        (funcall foo))))
  (values))

(defun load-log4slime ()
  "Entry point from Emacs to trigger loading Log4Slime if connection
is restarted or such"
  (flet ((%load-log4slime-quietly ()
           ;; There is probably "more correct" way to pass parameter
           ;; to a system being loaded, but I'm a slowpoke
           (setf (get :log4slime :no-emacs-startup-message) t)
           (asdf:load-system :log4slime))) 
    (multiple-value-bind (ok err)
        (ignore-errors (%load-log4slime-quietly))
      (if ok :ok (princ-to-string err)))))



