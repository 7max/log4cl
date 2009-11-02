;;
;; Package declaration for cl-log
;;

(cl:defpackage :cl-log)
(cl:in-package :cl-log)
(cl:defpackage :cl-log
  (:use :asdf
	:closer-common-lisp
	:closer-mop
	:bordeaux-threads
	:demacs
        :cl-ppcre
        :anaphora
        #+sbcl :sb-gray
        #+clisp :gray
        )
  (:export +log-level-unset+ +log-level-unset+ +log-level-user9+
	   +log-level-user8+ +log-level-user7+ +log-level-user6+
	   +log-level-user5+ +log-level-user4+ +log-level-user3+
	   +log-level-user2+ +log-level-user1+ +log-level-trace+
	   +log-level-debug+ +log-level-info+ +log-level-warn+
	   +log-level-error+ +log-level-fatal+ +log-level-off+
	   ;; logging macros
	   log-fatal log-error log-warn log-info log-debug log-trace
	   log-user1 log-user2 log-user3 log-user4 log-user5 log-user6
	   log-user7 log-user8 log-user9
           log-indented
	   ;; logger access functions
	   get-logger set-log-level add-appender
	   ;; special variables
	   *log-app-number* *root-logger* *default-logger-name* *log-indent*
	   ;; application
	   add-application
	   get-application))

