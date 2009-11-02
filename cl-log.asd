;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)
(asdf:defsystem :cl-log
  :serial t
  :version 1.0
  :depends-on (:bordeaux-threads 
               :closer-mop 
               :demacs
               :cl-ppcre
               :anaphora)
  :components ((:file "package")
	       (:file "cl-log-util")
               (:file "cl-log-test")
               (:file "demacs-integration")
	       (:file "appender")
	       (:file "console-appender")))

