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

;; Implementation package for LOG4CL, that can be included in the :USE
;; list of other packages. All logging functions that otherwise would
;; conflict with CL package or with common words are named with LOG-
;; prefix
;;
;; Use this package if you are extending LOG4CL or writing your own
;; appenders

(defpackage #:log4cl-impl-fixer
  (:use :cl))

(in-package #:log4cl-impl-fixer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((p1 (find-package '#:log4cl))
         (p2 (find-package '#:log4cl-impl)))
    (when (and p1 p2 (not (eq p1 p2)))
      (delete-package p1))))

(macrolet
    ((%define-log4cl-package ()
       (let* ((p2 (find-package '#:log4cl-impl)))
         (let* ((old-exports (when p2
                               (let (list)
                                 (do-external-symbols (s p2 list)
                                   (push s list)))))
                (new-exports
                  '(
                    ;; log levels
                    #:+log-level-unset+ #:+log-level-unset+ #:+log-level-debu9+
                    #:+log-level-debu8+ #:+log-level-debu7+ #:+log-level-debu6+
                    #:+log-level-debu5+ #:+log-level-debu4+ #:+log-level-debu3+
                    #:+log-level-debu2+ #:+log-level-debu1+ #:+log-level-trace+
                    #:+log-level-debug+ #:+log-level-info+ #:+log-level-warn+
                    #:+log-level-error+ #:+log-level-fatal+ #:+log-level-off+
                    ;; logging macros
                    #:log-fatal #:log-error #:log-warn #:log-info #:log-debug #:log-trace
                    #:log-debu1 #:log-debu2 #:log-debu3 #:log-debu4 #:log-debu5 #:log-debu6
                    #:log-debu7 #:log-debu8 #:log-debu9 #:log-sexp #:log-sexp-with-level
                    ;; sexp version of logging macros
                    #:log-sexp-fatal #:log-sexp-error #:log-sexp-warn #:log-sexp-info #:log-sexp-debug #:log-sexp-trace
                    #:log-sexp-debu1 #:log-sexp-debu2 #:log-sexp-debu3 #:log-sexp-debu4 #:log-sexp-debu5 #:log-sexp-debu6
                    #:log-sexp-debu7 #:log-sexp-debu8 #:log-sexp-debu9 
                    #:log-indented
                    ;; logger access functions
                    #:make-log-level #:make-logger
                    #:set-log-level
                    #:logger-parent
                    #:logger-log-level
                    #:logger-appenders 
                    #:effective-log-level
                    #:effective-appenders
                    #:add-appender
                    #:appender-added
                    #:appender-removed
                    #:logger-added
                    #:logger-removed
                    #:stream-appender
                    #:log-config
                    #:logger-name
                    #:logger-category
                    #:logger-depth
                    #:naming-option
                    #:log-level-from-object
                    #:resolve-logger-form
                    #:resolve-default-logging-form
                    #:enclosing-scope-block-name
                    #:reset-logging-configuration
                    #:clear-logging-configuration
                    ;; special variables
                    #:*hierarchy* #:*root-logger* #:*default-logger-name* #:*log-indent*
                    #:*ndc-context* #:*global-console*
                    ;; hierarchy
                    #:hierarchy-index
                    #:with-log-hierarchy
                    #:in-log-hierarchy
                    #:with-package-log-hierarchy
                    #:in-package-log-hierarchy
                    ;; layouts & appenders 
                    #:layout
                    #:layout-to-stream
                    #:appender-do-append
                    ;; standard layouts
                    #:default-layout
                    #:simple-layout
                    ;; standard appenders
                    #:appender
                    #:stream-appender
                    #:console-appender
                    #:serialized-appender
                    #:fixed-stream-appender
                    #:appender-stream
                    #:pattern-layout
                    #:pattern-layout-error
                    #:+min-log-level+
                    #:+max-log-level+
                    #:log-level-to-string
                    #:with-ndc-context
                    #:with-ndc
                    #:with-log-indent
                    #:logger-additivity
                    #:appender-error
                    #:handle-appender-error
                    #:file-appender-base
                    #:file-appender
                    #:rolling-file-appender-base
                    #:time-rolling-file-appender
                    #:maybe-roll-file
                    #:backup-log-file
                    #:appender-logger-count
                    #:close-appender
                    #:remove-appender
                    #:remove-all-appenders
                    #:appender-filename
                    #:daily-file-appender
                    #:+self-logger+
                    #:package-wrapper
                    #:logger-categories
                    #:property-parser
                    #:parse-property-stream
                    #:property-configurator
                    #:conversion-pattern
                    #:property-parser-error
                    #:configure
                    #:logger-children
                    #:logger-descendants
                    #:map-logger-children
                    #:map-logger-descendants
                    #:start-hierarchy-watcher-thread 
                    #:stop-hierarchy-watcher-thread
                    #:add-watch-token
                    #:remove-watch-token
                    #:watch-token-check
                    #:log4cl-error
                    #:save
                    #:*configurations-file*
                    #:*save-configurations-to-file*
                    #:*max-configurations*
                    #:restore
                    #:same-configuration-p
                    #:all-configurations
                    #:list-configurations
                    #:configuration-element
                    #:configuration
                    #:*logger-truename*
                    #:logger-file
                    #:*default-naming-configuration*
                    #:*naming-configuration*
                    #:naming-configuration
                    #:appender-next-backup-file
                    #:appender-last-backup-file
                    #:logger-file-namestring
                    #:logger-file-logger
                    #:logger-ancestors
                    #:inherited-log-level
                    #:+self-meta-logger+
                    #:appender-layout
                    #:appender-last-error
                    #:appender-last-ignored-error
                    #:appender-error-count
                    #:appender-ignored-error-count
                    #:appender-message-count
                    #:appender-enabled-p
                    #:counting-appender
                    #:this-console-appender
                    #:temp-appender
                    #:temp-appender-error-type
                    #:appender-loggers
                    #:old-logging-macros
                    #:packge-options
                    #:appender-do-flush
                    #:flush-appender
                    #:flush-all-appenders
                    #:save-appender
                    #:all-appenders
                    #:+expr-format-simple+
                    #:+expr-format-fancy+
                    #:category-separator
                    #:category-case
                    #:expr-print-format
                    #:join-categories
                    #:make-package-categories
                    #:%get-logger
                    #:with-package-naming-configuration
                    #:fix-method-spec-list
                    #:tricky-console-appender))
                ;; avoid SBCL (also exports) error
                (removed-exports
                  (set-difference old-exports
                                  new-exports
                                  :test #'string=
                                  :key #'string))
                (defpackage-form
                  `(defpackage #:log4cl-impl
                     (:nicknames #:log4cl)
                     (:use #:cl #:bordeaux-threads)
                     (:export ,@new-exports))))
           (when (and p2 removed-exports)
             (unexport removed-exports p2))
           defpackage-form))))
  (%define-log4cl-package))



