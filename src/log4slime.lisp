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

(defpackage #:log4cl.slime
  (:use :cl :log4cl)
  (:export #:emacs-helper #:get-buffer-log-menu))

(in-package #:log4cl.slime)

(defun emacs-helper (info)
  (handler-case 
      (destructuring-bind (&key package file rest package-offset action level root
                                rest-all) info
        (log:expr package file rest package-offset action level rest-all)
        (labels ((frob (logger display-name)
                   (log:expr logger display-name)
                   (when logger 
                     (cond 
                       ((eq :info action)
                        `(:package ,package
                          :file ,file
                          :rest ,rest
                          :root ,root
                          :level ,(logger-log-level logger)
                          :inherited-level  ,(inherited-log-level logger)
                          :children-level-count ,(count-if-not #'null (logger-descendants logger)
                                                               :key #'logger-log-level)
                          :display-name ,display-name
                          :package-offset ,package-offset))
                       ((and (eq :set action) (eq :reset level))
                        (log-config logger :clear)
                        "Child loggers had been reset")
                       ((eq :set action)
                        (log-config logger (or level :unset))
                        (cond 
                          (root (format nil "Root log level set to ~a"
                                        (log-level-to-string (effective-log-level logger)))) 
                          ((and level (not (eq level :unset)))
                           (format nil "~a level set to ~a"
                                   (logger-name-for-emacs logger)
                                   (log-level-to-string (effective-log-level logger))))
                          (t
                           (format nil "~a now inherits level ~a"
                                   (logger-name-for-emacs logger)
                                   (log-level-to-string (effective-log-level logger))))))
                       ((eq :get-location action)
                        (let* ((cats (mapcar #'read-from-string
                                             (log4cl::split-string (or rest-all "") " ")))
                               ;; Below uses swank-backend package for
                               ;; defun/defmethod and such in case
                               ;; swank ever starts using closer-mop
                               defs
                               (defs-all (when cats (swank::find-definitions (first cats))))
                               (defs-novar
                                 (remove 'swank-backend::defvar
                                         (swank::find-definitions (first cats))
                                         :key #'caar)))
                          (log:expr cats)
                          (cond ((<= (length defs-all) 1) (setq defs defs-all))
                                ((= (length defs-novar) 1) (setq defs defs-novar))
                                (t (setq defs (remove cats defs-novar
                                                      :test-not #'equal
                                                      :key
                                                      (lambda (x)
                                                        (log:sexp (cdar x) (remove t (cdar x)))
                                                        (remove t (cdar x)))))))
                          (log:sexp defs)
                          (mapcar #'swank::xref>elisp defs)))
                       (t (error "Invalid action ~s" action)))))
                 ;
                 (logger-name-for-emacs (logger)
                   (format nil "Category ~a" (logger-category logger))))
          (cond ((not root)
                 (let* ((pkg (when package
                               (or (find-package package)
                                  (find-package (string-upcase package))
                                  (find-package (string-downcase package))
                                  (swank::parse-package package)
                                  (let* ((str (ignore-errors
                                               (string (read-from-string package)))))
                                    (when str 
                                      (or (find-package str)
                                          (find-package (string-upcase str))
                                          (find-package (string-downcase str)))))))))
                   (log:expr pkg)
                   (when (or pkg (not package)) 
                     (swank::with-buffer-syntax (pkg)
                       (log4cl::with-package-naming-configuration (*package*) 
                         (let* ((just-file-p (and file (not rest)))
                                (package-categories (log4cl::make-package-categories *package*))
                                (sep (naming-option *package* :category-separator))
                                ;; handle right click in the middle of package
                                ;; name that was split into multiple categories
                                (package-categories
                                  (if (or (null package-offset)
                                          (< (length package-categories) 2))
                                      package-categories
                                      (loop with i = package-offset
                                            for cat in package-categories
                                            collect cat 
                                            until (<= i (length cat))
                                            do (decf i (+ (length cat)
                                                          (length sep))))))
                                (logger
                                  (if just-file-p
                                      (log4cl::%get-logger
                                       `(,@package-categories ,file)
                                       (naming-option *package* :category-separator)
                                       (naming-option *package* :category-case)
                                       nil nil
                                       nil nil nil
                                       just-file-p)
                                      (log4cl::%get-logger
                                       (append package-categories
                                               (when rest
                                                 (mapcar #'read-from-string
                                                         (log4cl::split-string rest " "))))
                                       (naming-option *package* :category-separator)
                                       (naming-option *package* :category-case)
                                       nil nil
                                       nil nil nil
                                       just-file-p))))
                           (log:expr logger)
                           (frob logger (or rest file
                                            (log4cl::join-categories sep package-categories)))))))))
                (root
                 (frob *root-logger* "+ROOT+")))))
    (error (condition)
      (log:sexp-error condition)
      nil)))

(defun get-buffer-log-menu (&key package file defun)
  (log:expr package file defun)
  (list
   (emacs-helper
    `(:action :info
      :root t))
   (when package 
     (emacs-helper
      `(:action :info
        :package ,package)))
   (when file 
     (emacs-helper
      `(:action :info
        :package ,package
        :file ,file)))
   (when defun 
     (emacs-helper
      `(:action :info
        :package ,package
        :rest ,defun)))))

;;
;; Support for snippets compiled via C-c C-c correctly identifying the source file
;; 
(defvar *old-compile-string-for-emacs*
  (fdefinition 'swank::compile-string-for-emacs))

;; patch the COMPILE-STRING-FOR-EMACS
(setf (fdefinition 'swank::compile-string-for-emacs)
      (lambda (string buffer position filename policy)
        (let ((*logger-truename* filename))
          (funcall *old-compile-string-for-emacs*
                   string buffer position filename policy))))


