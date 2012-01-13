;;
;; Package declaration for log4cl
;;


(cl:defpackage :log4cl
  (:use :cl
   :bordeaux-threads
   :cl-ppcre
   :demacs
   #+sbcl :sb-gray
   #+clisp :gray)
  (:export
   ;; log levels
   :+log-level-unset+ :+log-level-unset+ :+log-level-user9+
   :+log-level-user8+ :+log-level-user7+ :+log-level-user6+
   :+log-level-user5+ :+log-level-user4+ :+log-level-user3+
   :+log-level-user2+ :+log-level-user1+ :+log-level-trace+
   :+log-level-debug+ :+log-level-info+ :+log-level-warn+
   :+log-level-error+ :+log-level-fatal+ :+log-level-off+
   ;; logging macros
   :log-fatal :log-error :log-warn :log-info :log-debug :log-trace
   :log-user1 :log-user2 :log-user3 :log-user4 :log-user5 :log-user6
   :log-user7 :log-user8 :log-user9 :log-sexp :log-trace-sexp
   :log-indented
   ;; logger access functions
   :make-log-level :make-logger
   :set-log-level
   :logger-log-level
   :logger-appenders 
   :effective-log-level
   :effective-appenders
   :add-appender
   :appender-added
   :appender-removed
   :logger-added
   :logger-removed
   :stream-appender
   :log-config
   :logger-name
   :logger-category
   :naming-option
   :reset-logging-configuration
   :clear-logging-configuration
   ;; special variables
   :*hierarchy* :*root-logger* :*default-logger-name* :*log-indent*
   ;; hierarchy
   :hierarchy-index
   :with-log-hierarchy
   :in-log-hierarchy
   :with-package-log-hierarchy
   :in-package-log-hierarchy
   ;; layouts & appenders 
   :layout :layout-to-stream :appender-do-append
   ;; standard layouts
   :default-layout :simple-layout
   ;; standard appenders
   :appender
   :stream-appender
   :console-appender))



