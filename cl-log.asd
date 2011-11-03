;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :cl-log.system
  (:use :cl :asdf))

(in-package :cl-log.system)

(defsystem :cl-log
  :serial t
  :version "1.0"
  :depends-on (:bordeaux-threads 
               :cl-ppcre
               :demacs)
  :components ((:file "package")
	       (:file "cl-log-util")
               (:file "cl-log-test")
               ;; TODO do this dynamically only if demacs
               ;; package is present
               (:file "demacs-integration")
	       (:file "appender")
	       (:file "console-appender")
               (:module "test")))

(defsystem :cl-log.test
  :serial t
  :version "1.0"
  :depends-on (:cl-log :stefil)
  :components ((:file "test/logger")
               (:file "test/speed")))

(defmethod perform ((op test-op) (system (eql (find-system :cl-log))))
  (operate 'load-op :cl-log.test)
  (in-package :cl-log.test)
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'cl-log.test:test)"))
  (values))
