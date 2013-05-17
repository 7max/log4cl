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

(cl:in-package #:log4cl)

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
               `(defpackage #:log
                  (:use)
                  ,@(reexport-from
                     '#:log4cl
                     '(                 ;; class names
                       ;; #:fixed-stream-appender
                       ;; #:console-appender
                       ;; #:file-appender
                       ;; #:daily-file-appender
                       ;; #:property-configurator
                       ;; #:simple-layout
                       ;; #:pattern-layout

                       ;;  
                       ;; #:clear-logging-configuration
                       ;; #:reset-logging-configuration

                       ;; utility stuff
                       ;; #:configure
                       ;; #:add-appender
                       ;; #:remove-appender
                       ;; #:remove-all-appenders
                       ;; #:logger-additivity
                       ;; #:logger-appenders
                       ;; #:effective-appenders
                       ;; #:effective-log-level
                       ;; #:logger-category
                       ;; #:logger-name
                       ;; #:logger-parent
                       ;; #:logger-children
                       ;; #:logger-descendants
                       ;; #:logger-ancestors
                       ;; #:logger-depth
                       ;; #:logger-log-level
                       ;; #:log-sexp-with-level
                       ;; customization
                       ;; #:naming-option
                       ;; #:log-level-from-object
                       ;; #:resolve-logger-form
                       ;; #:resolve-default-logging-form
                       ;; #:enclosing-scope-block-name
                       ;; #:reset-logging-configuration
                       ;; #:clear-logging-configuration
                       ;; #:naming-option
                       ;; #:package-wrapper
                       ;; #:map-logger-children
                       ;; #:map-logger-descendants
                       ;; #:start-hierarchy-watcher-thread 
                       ;; #:stop-hierarchy-watcher-thread
                       ;; #:add-watch-token
                       ;; #:remove-watch-token
                       ;; #:watch-token-check
                       ;; #:log4cl-error
                       ;; #:log4cl-error
                       ;; variables
                       ;; #:*root-logger*
                       ;; quick save/restore of configurations
                       #:save
                       #:restore
                       #:package-options
                       #:with-ndc
                       ;; #:configuration-element
                       ;; #:configuration
                       ;; #:same-configuration-p
                       ;; #:all-configurations
                       ;; #:list-configurations
                       ;; #:*configurations-file*
                       ;; #:*save-configurations-to-file*
                       ;; #:*max-configurations*
                       ))
                  (:import-from :cl #:in-package)
                  ,@(shadow-and-export
                     `(#:sexp
                       #:expr #:config  ,@+log-level-macro-symbols+ ,@(level-expr-syms)
                       #:make #:category
                       #:with-hierarchy
                       #:push #:pop
                       #:with-package-hierarchy
                       #:in-package-hierarchy
                       #:in-hierarchy
                       #:with-indent
                       ;; one letter logging macro forwarders
                       #:f #:e #:w #:i #:d #:d1 #:d2 #:d3 #:d4
                       #:t #:d5 #:d6 #:d7 #:d8 #:d9 #:c #:s))
                  
                  (:export
                   #:f #:e #:w #:i #:d #:d1 #:d2 #:d3 #:d4 #:t #:d5 #:d6 #:d7 #:d8 #:d9 #:c #:s)))))
  (log4cl-defpackage))

(defmacro forward-macro (name from-name &optional depreciate replacement)
  (if depreciate 
      (let ((replacement
              ;; we want stuff to print as LOG4CL:SOMETHING rather
              ;; then LOG4CL-IMPL:SOMETHING but can't change package
              ;; name right now
              (when replacement
                (cond
                  ((and (symbolp replacement)
                        (eq *package* (symbol-package replacement)))
                   (format nil "~A:~A" '#:log4cl replacement))
                  ((and (consp replacement)
                        (endp (cddr replacement))) 
                   (format nil "~A:~A" (first replacement) (second replacement)))
                  (t 
                   (format nil "~S" replacement)))))) 
        `(progn
           (setf (documentation ',name 'function) (documentation ',from-name 'function))
           (setf (macro-function ',name)
                 (lambda (&rest args)
                   (log4cl-style-warning "Macro ~S is depreciated~^. Use ~A instead" ',name
                                         ,@(when replacement `(',replacement)))
                   (apply (macro-function ',from-name) args)))))
      `(progn
         (setf (documentation ',name 'function) (documentation ',from-name 'function))
         (setf (macro-function ',name) (macro-function ',from-name)))))

(defmacro forward-function (name from-name)
  `(progn
     (setf (documentation ',name 'function) (documentation ',from-name 'function))
     (setf (fdefinition ',name) (fdefinition ',from-name))))

(defmacro forward-levels (levels)
  (let ((defs
          (loop for level in levels
                as macro-name = (intern (symbol-name level) :log)
                as forward-name = (or (find-symbol (format nil "~A-~A"
                                                           (string '#:log)
                                                           (string level))
                                                   :log4cl) 
                                      (error "Unable to find logging macro for ~S" level))
                collect `(forward-macro ,macro-name ,forward-name))))
    `(progn
       ,@defs)))

(defmacro forward-sexp-levels (levels)
  (let ((defs
          (loop for level in levels
                ;; sexp-debug, sexp-info etc
                as macro-name = (intern (symbol-name level) :log)
                as sexp-macro-name = (intern (format nil "~A-~A"
                                                     (string '#:sexp)
                                                     (string level))
                                             :log)
                ;; in impl package they are called LOG-SEXP-DEBUG LOG-SEXP-INFO ETC
                as sexp-forward-name = (or (find-symbol (format nil "~A-~A"
                                                                (string'#:log-sexp)
                                                                (string level))
                                                        :log4cl) 
                                           (error "Unable to find logging macro for ~S" level))
                collect `(forward-macro ,sexp-macro-name ,sexp-forward-name t ,macro-name))))
    `(progn
       ,@defs)))

(forward-levels #.+log-level-macro-symbols+) 
(forward-sexp-levels #.+log-level-macro-symbols+) 
(forward-macro log:sexp log4cl:log-sexp) 

;; make (log:expr) same as (log:sexp) and (log:make) shortcut for (log:make-logger)
(forward-macro log:expr log4cl:log-sexp)

(forward-macro log:category log4cl:make-logger) 
(forward-macro log:make log4cl:make-logger t log:category) 

;; one letter logging macros
(forward-macro log:f log4cl:log-fatal)
(forward-macro log:e log4cl:log-sexp)
(forward-macro log:w log4cl:log-warn)
(forward-macro log:i log4cl:log-info)

(forward-macro log:d log4cl:log-debug)
(forward-macro log:d1 log4cl:log-debu1)
(forward-macro log:d2 log4cl:log-debu2)
(forward-macro log:d3 log4cl:log-debu3)
(forward-macro log:d4 log4cl:log-debu4)
(forward-macro log:t log4cl:log-trace)
(forward-macro log:d5 log4cl:log-debu5)
(forward-macro log:d6 log4cl:log-debu6)
(forward-macro log:d7 log4cl:log-debu7)
(forward-macro log:d8 log4cl:log-debu8)
(forward-macro log:d9 log4cl:log-debu9)
(forward-macro log:s log4cl:log-sexp)

(forward-macro log:with-indent log4cl:with-log-indent)

(forward-function log:config log-config)
(forward-function log:c log-config)
(forward-function log:pop restore)
(forward-function log:push save)

;; depreciated forwards
(forward-macro log:with-hierarchy log4cl:with-log-hierarchy t log4cl:with-log-hierarchy)
(forward-macro log:with-package-hierarchy log4cl:with-package-log-hierarchy t log4cl:with-package-log-hierarchy)
(forward-macro log:in-hierarchy log4cl:in-log-hierarchy t log4cl:in-log-hierarchy)
(forward-macro log:in-package-hierarchy log4cl:in-package-log-hierarchy t log4cl:in-package-log-hierarchy)

(forward-macro log4cl:with-ndc-context log4cl:with-ndc t log4cl:with-ndc)


