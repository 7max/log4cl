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
  (:nicknames #:log4slime)
  (:use #:cl #:log4cl)
  (:export #:install #:emacs-helper #:get-buffer-log-menu
           #:*quicklisp-directory*
           #:*system-directory*))

(in-package #:log4cl.slime)
(log:package-options :shortest-nickname nil)

;; Accessors for slime (DSPEC LOC) 
(defun slime-loc-type (loc) (first (first loc)))
(defun slime-loc-name (loc) (second (first loc)))

(defun slime-loc-type-figurer (type)
  "Parse Slime DSPEC type, and figure if its a DEFUN, DEFMETHOD or
something else.. Should return DEFUN or DEFMETHOD, or NIL if can't
figure it out

 So far it looks like SBCL returns what we need already, but CCL
 returns :METHOD for methods, and actual function object for functions

"
  (cond ((eq type 'defmethod) 'defmethod)
        ((eq type 'defun) 'defun)
        ((and (consp type)
              (eq (first type) 'function))
         'defun)
        ((eq type :method)
         'defmethod)
        (t (let ((repr (string-upcase (princ-to-string type)))) 
             (cond 
               ((search "METHOD" repr) 'defmethod) 
               ((search "DEFUN" repr) 'defun)
               ((search "FUNCTION" repr) 'defun))))))

(defun slime-loc-defun-p (loc) (eq (slime-loc-type loc) 'swank-backend::defun))
(defun slime-loc-defvar-p (loc) (eq (slime-loc-type loc) 'swank-backend::defvar))

(defun find-best-location-match (categories definitions)
  "User had left-clicked on a log message coming list of CATEGORIES
try to find matching source definition. It it can definitely find one,
return it, otherwise return all of them, so Slime will pop the xref
window"
  (log:trace categories (mapcar 'first definitions))
  ;; if there is one or none, nothing to do
  (when (<= (length definitions) 1)
    (log:debug "just one" (car definitions))
    (return-from find-best-location-match definitions))
  ;; if have a defun, thats where its coming from
  (log:debug "here")
  (let* ((tmp (mapcar (lambda (x)
                        (cons (slime-loc-type-figurer (slime-loc-type x))
                              x))
                      definitions))
         (defuns (progn
                   (log:debug tmp)
                   (mapcar #'cdr (remove 'defun tmp :key #'car :test-not #'eq))))
         (methods (mapcar #'cdr (remove 'defmethod tmp :key #'car :test-not #'eq))))
    (log:debug (length defuns) (length methods))
    (when (= (length defuns) 1)
      (return-from find-best-location-match defuns))
    (when (plusp (length defuns)) 
      (log:warn "SLIME found more then one DEFUN for ~S" (first categories))
      ;; return all of them since we don't know what is going on
      (return-from find-best-location-match definitions))
    (dolist (method methods)
      (let* ((dspec (first method))
             (specs (cdr dspec)))
        (log:debug "orig" specs)
        (setq specs (fix-method-spec-list specs))
        (log:debug "after fixing EQL" specs)
        (when (equal specs categories)
          (return-from find-best-location-match (list method)))))
    definitions))


(defun emacs-helper (info)
  "This function grew up from a small helper, and desperately needs to
be split into multiple ones, but I have no time right now"
  (handler-case 
      (destructuring-bind (&key package file rest package-offset action level root
                                rest-all) info
        (let (package-categories) 
          (log:expr package file rest package-offset action level rest-all) 
          (labels
              ((frob (logger display-name)
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
                        :children-level-count ,(children-level-count logger)
                        :display-name ,display-name
                        :package-offset ,package-offset))
                     ((and (eq :set action) (eq :reset level))
                      (log-config logger :clear)
                      (list (logger-log-level logger)
                            (inherited-log-level logger)))
                     ((eq :set action)
                      (log-config logger (or level :unset))
                      (list (logger-log-level logger)
                            (inherited-log-level logger)))
                     ((eq :get-location action)
                      (get-locations))
                     (t (error "Invalid action ~s" action)))))
               (get-locations ()
                 (let* ((cats (mapcar #'read-from-string
                                      (log4cl::split-string
                                       (or rest-all "") " ")))
                        (defs (when cats 
                                (if (and (eq (first cats) 'setf)
                                         (second cats))
                                    (swank::find-definitions (subseq cats 0 2))
                                    (swank::find-definitions (first cats))))))
                   (mapcar #'swank::xref>elisp (find-best-location-match cats defs))))
               (logger-name-for-emacs (logger)
                 (format nil "Category ~a" (logger-category logger)))
               (children-level-count (logger)
                 (count-if-not #'null (logger-descendants logger t)
                               :key #'logger-log-level))
               (find-package-categories ()
                 (setq package-categories (make-package-categories *package*))
                 (let ((sep (category-separator))) 
                   (log:debug package-categories)
                   ;; handle right click in the middle of package
                   ;; name that was split into multiple categories
                   (unless (or (null package-offset)
                               (< (length package-categories) 2))
                     (setq package-categories 
                           (loop with i = package-offset
                                 for cat in package-categories
                                 collect cat 
                                 until (<= i (length cat))
                                 do (decf i (+ (length cat)
                                               (length sep)))))
                     (log:debug "Was a click in the middle of package" package-offset
                                package-categories))))
               (find-logger ()
                 (cond ((and file (not rest))
                        (log:debug "file logger" file) 
                        (values
                         (%get-logger `(,@package-categories ,file)
                                      (category-separator)
                                      (category-case)
                                      nil nil
                                      nil nil nil t)
                         file))
                       ((not (null rest))
                        (log:debug "finding for" rest) 
                        (do* ((cat-strings (log4cl::split-string rest " ") 
                                           (butlast cat-strings)) 
                              (cat-syms (mapcar #'read-from-string cat-strings)
                                        (butlast cat-syms)))
                             ((null cat-syms))
                          (log:debug "trying" cat-syms)
                          (let ((logger 
                                    (%get-logger
                                     (append package-categories cat-syms)
                                     (category-separator)
                                     (category-case)
                                     nil nil
                                     nil nil nil nil)))
                            (when logger
                              (return
                                (values logger
                                        (join-categories " " cat-strings)))))))
                       (t
                        (log:debug "package or root logger")
                        (values 
                         (%get-logger package-categories
                                      (category-separator)
                                      (category-case)
                                      nil nil
                                      nil nil nil
                                      nil)
                         (join-categories (category-separator)
                                          package-categories))))))
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
                         (with-package-naming-configuration (*package*) 
                           (find-package-categories)
                           (multiple-value-bind (logger display-name) (find-logger) 
                             (log:expr logger display-name)
                             (frob logger display-name)))))))
                  (root
                   (frob *root-logger* "+ROOT+"))))))
    (error (condition)
      (log:error "~a" condition)
      nil)))

(defun get-buffer-log-menu (&rest args)
  (log:expr args)
  (handler-case 
      (destructuring-bind (&key package file defun) args 
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
    (error (condition)
      (log:error "~a" condition)
      nil)))

;;
;; Support for snippets compiled via C-c C-c correctly identifying the source file
;; 
(defvar *old-compile-string-for-emacs*
  (fdefinition 'swank::compile-string-for-emacs))

;; Patch the COMPILE-STRING-FOR-EMACS to bind *LOGGER-TRUENAME* to the file
;; name that C-c C-c snippet is from
(setf (fdefinition 'swank::compile-string-for-emacs)
      (lambda (string buffer position filename policy)
        (let ((*logger-truename*
                (when filename (ignore-errors (parse-namestring filename)))))
          (funcall *old-compile-string-for-emacs*
                   string buffer position filename policy))))

;; In case SWANK was patched with the "thread stopper" patch that defines
;; protocol for starting/stopping threads around calls to fork(), register
;; a callback for the watcher thread
(let ((rss-foo (find-symbol (symbol-name '#:register-thread-stopper) (find-package :swank))))
  (and rss-foo (funcall rss-foo :log4cl #'log4cl::start/stop-watcher-hook)))
;;
;; Some copy-paste from CLHS package
;; 

(defparameter *system-directory*
  (make-pathname :name nil :type nil
		 :defaults #.(or *logger-truename* *compile-file-truename* *load-truename*)))

(defun %try-getting-authoritative-quicklisp-directory ()
  (let ((ql (find-package '#:quicklisp)))
    (and ql
         (let ((home-symbol (find-symbol (string '#:*quicklisp-home*) ql)))
           (and home-symbol
                (boundp home-symbol)
                (symbol-value home-symbol))))))

(defvar *quicklisp-directory*
  (or (%try-getting-authoritative-quicklisp-directory)
      (make-pathname
       :name nil :type nil
       :defaults (merge-pathnames (make-pathname
                                   :directory '(:relative "quicklisp"))
                                  (user-homedir-pathname)))))

(defun homepath (path)
  "Format PATH replacing home dir with ~"
  (let ((s (princ-to-string path))
        (home (princ-to-string (user-homedir-pathname))))
    (log4cl::replace-in-string
     s home
     (princ-to-string (make-pathname :directory '(:relative "~"))))))

(defun install (&key force
                     (destination-directory *quicklisp-directory*)
                     just-message)
  (let ((*print-pretty* t)
        have-file-p
        right-path-in-file-p
        (dir (merge-pathnames (make-pathname
                               :directory '(:relative :up "elisp"))
                              *system-directory*)))
    (flet ((print-dot-emacs-message (filename)
             (format t "~%Add the following two statements to your ~~/.emacs file~%") 
             (format t "------------------------------------------------------~%") 
             (format t "(load ~S)~%" (homepath filename))
             (format t "(global-log4slime-mode 1)~%")
             (format t "------------------------------------------------------~%"))
           (print-generate-file-message (filename)
             (format t "~%~
            ~@<File ~A does not exist. Use ~(~:<~A:~A~:>~) to generate it~
            ~%~:@>"
                     (homepath filename)
                     `(#:log4slime #:install)))
           (print-wrong-path-message (filename dir)
             (format t "~%~@<File ~A exist but does not contain the right path to ~A. ~
                             Use ~(~:<~A:~A ~:_~S ~S~:>~) to overwrite it~%~:@>"
                     (homepath filename)
                     (homepath dir)
                     `(#:log4slime #:install :force t)))
           (print-right-path-message (filename dir)
             (format t "~%~
            ~@<File ~A seem to already contain the right path to ~A. ~
            You can still use ~(~:<~A:~A ~_~S ~S~:>~) to overwrite it~
            ~%~:@>"
                     (homepath filename)
                     (homepath dir)
                     `(#:log4slime #:install :force t)))
           (generate-the-file (filename dir)
             (with-open-file (out filename
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (format out ";; load Log4slime support~%~%")
               (format out "(add-to-list 'load-path ~S)~%"
                       (homepath dir))
               (format out "(require 'log4slime)~%"))
             (format t "Wrote ~A~%" (homepath filename)))
           (check-directories ()
             (let ((ok t)) 
               (when (not (probe-file destination-directory))
                 (let ((directory (directory-namestring destination-directory)))
                   (if (pathname-match-p destination-directory *quicklisp-directory*)
                       (format t "~@<The following doesn't seem to be ~
                       the correct QuickLisp directory location: ~
                       \"~A\" does not exist. Try ~(~:<~A ~A:~A ~S~:@>~) ~:@>"
                               (homepath directory)
                               `(setf :log4slime *quicklisp-directory*
                                      ,(directory-namestring
                                        (make-pathname
                                         :directory
                                         '(:absolute "path" "to" "quicklisp")))))
                       (format t "Destination directory \"~A\" does not exist."
                               (homepath directory)))
                   (setq ok nil)))
               (when (not (probe-file dir))
                 (format t "~@<The following doesn't seem to be ~
                       the correct Log4CL source elisp directory location: ~
                       \"~A\" does not exist. Try ~(~:<~A ~A:~A ~S~:@>~) ~:@>"
                         (homepath dir)
                         `(setf :log4slime *system-directory*
                                ,(directory-namestring
                                  (make-pathname
                                   :directory
                                   '(:absolute "path" "to" "log4cl" "src")))))
                 (setq ok nil))
               (or ok (return-from install (values)))
               (setq dir (truename dir))
               (setq destination-directory (truename destination-directory)))))
      (check-directories)
      (let* ((filename (make-pathname :name "log4slime-setup" :type "el"
                                      :defaults destination-directory))
             
             (dir-string (homepath dir))
             (alt-str (princ-to-string dir))
             (force (when (not just-message) force)))
        (setq have-file-p (probe-file filename))
        (when have-file-p
          (setq right-path-in-file-p
                (with-open-file (fin filename :direction :input)
                  (loop for line = (read-line fin nil)
                        while line
                        do (let (start)
                             (dotimes (i (length line))
                               (unless (position (char line i) " \t" :test #'char=)
                                 (setq start i)
                                 (return)))
                             (when (and start
                                        (not (char= #\; (char line start)))) 
                               (when (or (search dir-string line)
                                         (search alt-str line))
                                 (return t))))))))
        (cond (just-message
               (cond ((null have-file-p) 
                      (print-generate-file-message filename))
                     ((null right-path-in-file-p)
                      (print-wrong-path-message filename dir))))
              (force
               (generate-the-file filename dir)
               (print-dot-emacs-message filename))
              ((null have-file-p)
               (generate-the-file filename dir)
               (print-dot-emacs-message filename))
              ((null right-path-in-file-p)
               (print-wrong-path-message filename dir))
              (t
               (print-right-path-message filename dir)
               (print-dot-emacs-message filename)))
        (values)))))

(unless (get :log4slime :no-emacs-startup-message)
  (install :just-message t))
