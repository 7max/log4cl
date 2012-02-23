;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :log4cl.system
  (:use :cl :asdf))

(in-package :log4cl.system)

(defsystem :log4cl
  :version "0.9"
  :depends-on (:bordeaux-threads)
  :components
  ((module "src" :serial t
                 :components ((:file "impl-package")
                              (:file "defs")
                              (:file "naming")
                              #+sbcl (:file "naming-sbcl")
                              (:file "appender-base")
                              (:file "hierarchy-base")
                              (:file "hierarchy")
                              (:file "logger")
                              (:file "logging-macros")
                              (:file "self-logger")
                              (:file "layout")
                              (:file "simple-layout")
                              (:file "pattern-layout")
                              (:file "watcher")
                              (:file "appender")
                              (:file "configurator")
                              (:file "property-parser")
                              (:file "property-configurator")
                              (:file "package")))))

(defsystem :log4cl-test
  :version "0.9"
  :depends-on (:log4cl :stefil)
  :components ((:module "tests"
                :serial t
                :components ((:file "test-logger")
                             (:file "test-category-separator")
                             (:file "test-layouts")
                             (:file "test-appenders")
                             (:file "test-configurator")
                             (:file "test-speed")))))

(defmethod perform ((op test-op) (system (eql (find-system :log4cl))))
  (operate 'load-op :log4cl-test)
  (let ((*package* (find-package :log4cl-test)))
    (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'log4cl-test::test)")))
  (values))


