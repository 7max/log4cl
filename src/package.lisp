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

(cl:in-package #:log4cl-impl)

(macrolet ((log4cl-defpackage ()
             (labels ((reexport-from (name names)
                        `((:import-from ,name ,@names)
                          (:export ,@names)))
                      (level-expr-syms ()
                        ;; make SEXP-<LEVEL> symbols for all debug levels
                        (loop for sym in +log-level-macro-symbols+
                              collect (make-symbol (format nil "~a-~a"
                                                           (string '#:sexp)
                                                           (string sym)))))
                      (shadow-and-export (syms)
                        `((:shadow ,@syms)
                          (:export ,@syms))))
               `(defpackage #:log4cl
                  (:use)
                  (:nicknames #:log)
                  ,@(reexport-from
                     '#:log4cl-impl
                     '(;; class names
                       #:fixed-stream-appender
                       #:console-appender
                       #:file-appender
                       #:daily-file-appender
                       #:property-configurator
                       #:simple-layout
                       #:pattern-layout
                       #:clear-logging-configuration
                       #:reset-logging-configuration
                       ;; utility stuff
                       #:make-logger
                       #:configure
                       #:add-appender
                       #:remove-appender
                       #:remove-all-appenders
                       #:logger-additivity
                       #:logger-appenders
                       #:effective-appenders
                       #:effective-log-level
                       #:logger-category
                       #:logger-name
                       #:logger-parent
                       #:logger-children
                       #:logger-descendants
                       #:logger-depth
                       #:logger-log-level
                       #:log-sexp-with-level
                       ;; customization
                       #:naming-option
                       #:log-level-from-object
                       #:resolve-logger-form
                       #:resolve-default-logging-form
                       #:enclosing-scope-block-name
                       #:reset-logging-configuration
                       #:clear-logging-configuration
                       #:naming-option
                       #:package-wrapper
                       #:map-logger-children
                       #:map-logger-descendants
                       #:start-hierarchy-watcher-thread 
                       #:stop-hierarchy-watcher-thread
                       #:add-watch-token
                       #:remove-watch-token
                       #:watch-token-check
                       #:log4cl-error
                       #:log4cl-error
                       ;; variables
                       #:*root-logger*
                       ;; quick save/restore of configurations
                       #:save
                       #:restore
                       #:*configurations-file*
                       #:*save-configurations-to-file*
                       #:*default-logging-configuration-scope*
                       #:*max-configurations*
                       #:*configurations*))
                  (:import-from :cl #:in-package)
                  ,@(shadow-and-export
                     `(#:sexp #:expr #:config #:make ,@+log-level-symbols+ ,@(level-expr-syms)
                              #:with-hierarchy
                              #:push #:pop
                              #:with-package-hierarchy
                              #:in-package-hierarchy
                              #:in-hierarchy
                              #:with-indent))
                  ;; one letter logging macro forwarders
                  (:shadow #:f #:e #:w #:i #:d #:u1 #:u2 #:u3 #:u4 #:t #:u5 #:u6 #:u7 #:u8 #:u9 #:c #:s)
                  (:export #:f #:e #:w #:i #:d #:u1 #:u2 #:u3 #:u4 #:t #:u5 #:u6 #:u7 #:u8 #:u9 #:c #:s)))))
  (log4cl-defpackage))

(defmacro forward-macro (name from-name)
  `(progn
     (setf (documentation ',name 'function) (documentation ',from-name 'function))
     (setf (macro-function ',name) (macro-function ',from-name))))

(defmacro forward-function (name from-name)
  `(progn
     (setf (documentation ',name 'function) (documentation ',from-name 'function))
     (setf (fdefinition ',name) (fdefinition ',from-name))))

(defmacro forward-levels (levels)
  (let ((defs
          (loop for level in levels
                as macro-name = (intern (symbol-name level) :log4cl)
                as forward-name = (or (find-symbol (format nil "~A-~A"
                                                           (string '#:log)
                                                           (string level))
                                                   :log4cl-impl) 
                                      (error "Unable to find logging macro for ~S" level))
                collect `(forward-macro ,macro-name ,forward-name))))
    `(progn
       ,@defs)))

(defmacro forward-sexp-levels (levels)
  (let ((defs
          (loop for level in levels
                ;; sexp-debug, sexp-info etc
                as sexp-macro-name = (intern (format nil "~A-~A"
                                                     (string '#:sexp)
                                                     (string level))
                                             :log4cl)
                ;; in impl package they are called LOG-SEXP-DEBUG LOG-SEXP-INFO ETC
                as sexp-forward-name = (or (find-symbol (format nil "~A-~A"
                                                                (string'#:log-sexp)
                                                                (string level))
                                                        :log4cl-impl) 
                                      (error "Unable to find logging macro for ~S" level))
                collect `(forward-macro ,sexp-macro-name ,sexp-forward-name))))
    `(progn
       ,@defs)))

(forward-levels #.+log-level-macro-symbols+) 
(forward-sexp-levels #.+log-level-macro-symbols+) 
(forward-macro log:sexp log-sexp) 

;; make (log:expr) same as (log:sexp) and (log:make) shortcut for (log:make-logger)
(forward-macro log:expr log4cl-impl:log-sexp)
(forward-macro log:make log4cl-impl:make-logger) 

;; one letter logging macros
(forward-macro log:f log4cl-impl:log-fatal)
(forward-macro log:e log4cl-impl:log-error)
(forward-macro log:w log4cl-impl:log-warn)
(forward-macro log:i log4cl-impl:log-info)

(forward-macro log:d log4cl-impl:log-debug)
(forward-macro log:u1 log4cl-impl:log-user1)
(forward-macro log:u2 log4cl-impl:log-user2)
(forward-macro log:u3 log4cl-impl:log-user3)
(forward-macro log:u4 log4cl-impl:log-user4)
(forward-macro log:t log4cl-impl:log-trace)
(forward-macro log:u5 log4cl-impl:log-user5)
(forward-macro log:u6 log4cl-impl:log-user6)
(forward-macro log:u7 log4cl-impl:log-user7)
(forward-macro log:u8 log4cl-impl:log-user8)
(forward-macro log:u9 log4cl-impl:log-user9)
(forward-macro log:s log4cl-impl:log-sexp)

(forward-macro log:with-hierarchy log4cl-impl:with-log-hierarchy)
(forward-macro log:with-package-hierarchy log4cl-impl:with-package-log-hierarchy)
(forward-macro log:in-hierarchy log4cl-impl:in-log-hierarchy)
(forward-macro log:in-package-hierarchy log4cl-impl:in-package-log-hierarchy)
(forward-macro log:with-indent log4cl-impl:with-log-indent)
(forward-macro log:push save)

(forward-function log:config log-config)
(forward-function log:c log-config)
(forward-function log:pop restore)


