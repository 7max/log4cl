;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)
(asdf:defsystem :cl-log
  :serial t
  :version 1.0
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
	       (:file "console-appender")))

