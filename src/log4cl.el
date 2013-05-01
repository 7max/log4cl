;;; -*- Mode: Emacs-Lisp; -*-
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

(require 'slime)
(require 'font-lock)

(defface log4cl-package-face
    '((((background dark)) (:foreground "#7474FFFFFFFF" :background "DimGray")) ; ~ cyan
      (t (:foreground "DarkRed")))
  "*Face for displaying package name category
logging event originated in"
  :group 'log4cl :group 'faces)

(defface log4cl-file-category-face
    '((((background dark)) (:foreground "#7474FFFFFFFF" :background "DimGray")) ; ~ cyan
      (t (:foreground "DarkRed")))
  "*Face used for a category corresponding to the file name where
event originated in"
  :group 'log4cl :group 'faces)

(defface log4cl-function-category-face
    '((((background dark)) (:foreground "Yellow"))
      (t                   (:foreground "Blue")))
  "*Face used for categories after the package and file name (usually the function/method name)"
  :group 'log4cl :group 'font-lock-highlighting-faces)

(defface log4cl-level-category-face
    '((t                   (:weight bold)))
  "*Face used for categories after the package and file name (usually the function/method name)"
  :group 'log4cl :group 'font-lock-highlighting-faces)

(defvar log4cl-log-level-names
  '("Off" "Fatal" "Error" "Warn" "Info" "Debug" 
    "Debu1" "Debu2" "Debu3" "Debu4" "Trace" "Debu5" "Debu6"
    "Debu7" "Debu8" "Debu9"))

(defun log4cl-level-name (level)
  (if level 
      (nth level log4cl-log-level-names)
    "Parent"))

;; Logger info is used to communicate information about a logger both from Emacs to Lisp
;; and the other way around. The first three elements below are used to identify or find
;; the logger in question, the rest of the elements are returned back from Lisp, if logger
;; exists
;; 
;; :package              STRING
;;
;;     Name of the logger's package
;;
;; :file                 STRING
;;
;;     Filename  or NIL
;;
;; :rest                 STRING
;;
;;     Space separated list of log categories after the package if any
;;     
;; :display-name         STRING
;;
;;     What to display on the bottom of the menu in (Changing log level for)
;;
;; :level                NUMBER
;;
;;     Logger's own log level, NIL if unset
;; 
;; :inherited-level      NUMBER
;;
;;     The log level that logger would have if its own level is unset
;;
;; :children-level-count  NUMBER
;;
;;     Number of child loggers with their own log level
;;
;;
;; Example: if there is a logger com.openchat.log4cl.test.foo.bar with the
;; package being com.openchat.log4cl.test, then values will be as follows
;;
;;   :package         "com.openchat.log4cl.test"
;;   :rest            "foo bar"
;;   :file            "whatever.lisp"
;;   :display-name    "foo bar"
;;   
;; For the "com.openchat" logger values will be:
;; 
;;   :package         "com.openchat.log4cl.test"
;;   :rest            nil
;;   :file            nil
;;   :display-name    "com.openchat"
;;
;;
;; For the source-file-logger of whatever.lisp values will be:
;; 
;;   :package         "com.openchat.log4cl.test"
;;   :rest            nil
;;   :file            "whatever.lisp"
;;   :display-name    "whatever.lisp"
;;

(defun log4cl-logger-package (info)
  (getf info :package))
(defun log4cl-logger-file (info)
  (getf info :file))
(defun log4cl-logger-rest (info)
  (getf info :rest))
(defun log4cl-logger-display-name (info)
  (getf info :display-name))
(defun log4cl-logger-level (info)
  (getf info :level))
(defun log4cl-logger-inherited-level (info)
  (getf info :inherited-level))
(defun log4cl-children-level-count (info)
  (getf info :children-level-count))
(defun log4cl-package-offset (info)
  (getf info :package-offset))
(defun log4cl-logger-id (info)
  (let ((package (log4cl-logger-package info))
        (file (log4cl-logger-file info))
        (rest (log4cl-logger-rest info))
        (package-offset (log4cl-package-offset info)))
    `(:package ,package :file ,file :rest ,rest :package-offset ,package-offset
               :root ,(getf info :root))))

(defun log4cl-info-symbol-to-string (info-symbol)
  (ecase info-symbol
    (log4cl-root-logger "root")
    (log4cl-package-logger "package")
    (log4cl-file-logger "file")
    (log4cl-defun-logger "defun")))

(defun log4cl-level-menu-item (info-symbol level)
  "Create the easy-menu menu item that toggles the log level"
  (let* ((S `(eql ,level (log4cl-logger-level ,info-symbol)))
         (T (if level (log4cl-level-name level)
              `(format "Parent (%s)"
                       (log4cl-level-name
                        (log4cl-logger-inherited-level ,info-symbol)))))
         (cmd 
          (if (eq info-symbol 'log4cl-popup-logger)
              `((log4cl-set-level ',info-symbol ,level)) 
            `(,(intern (format "log4cl-%s-%s" 
                               (log4cl-info-symbol-to-string info-symbol)
                               (downcase (log4cl-level-name level))))))))
    `[,(or level :unset)
      ,@cmd
      :label ,T
      :style radio
      :selected ,S
      :active (not ,S)
      ]))

(defun log4cl-set-level (info-symbol level)
  "COMMAND for menu items that will change the log level"
  (let* ((info (symbol-value info-symbol))
         (id (log4cl-logger-id info)))
    ;; (log-sexp info id level)
    (let ((result 
           (log4cl-eval `(log4cl.slime:emacs-helper
                         '(,@id :action :set :level ,level)))))
      (when (stringp result)
        (message result)))))

(defun log4cl-eval (form)
  "Wrapper around `slime-eval' that ignores errors on the lisp side"
  (when (log4cl-check-connection) 
    (slime-eval `(cl:ignore-errors ,form))))

(defvar log4cl-mode-map (make-sparse-keymap))

(defvar log4cl-root-map (make-sparse-keymap) "Keymap for changing level of the root logger")
(defvar log4cl-package-map (make-sparse-keymap) "Keymap for changing level of the package logger")
(defvar log4cl-file-map (make-sparse-keymap) "Keymap for changing level of the file logger")
(defvar log4cl-defun-map (make-sparse-keymap) "Keymap for changing level of the current defun logger")
(defvar log4cl-section-map (make-sparse-keymap)
  "Keymap that dispatches to one of `log4cl-root-map'
`log4cl-package-map' `log4cl-file-map' or
`log4cl-defun-map'")

(defvar log4cl-last-prefix-keys nil)

(defun log4cl-init-logging-maps ()
  (setq log4cl-section-map (make-sparse-keymap))
  (loop
   for section in '("root" "package" "file" "defun")
   for section-map-sym in '(log4cl-root-map
                            log4cl-package-map
                            log4cl-file-map
                            log4cl-defun-map)
   for section-char = (aref section 0)
   for section-map = (make-sparse-keymap)
   do (progn
        (dolist (level '("parent" "off" "fatal" "error" "warn" "info" "debug"
                         "debu1" "debu2" "debu3" "debu4" "trace"
                         "debu5" "debu6" "debu7" "debu8" "debu9"
                         "reset"))
          (let ((level-char (aref level (if (string-match "debu[0-9]" level)
                                            4 0)))
                (cmd (unless (and (equal section "root")
                                  (equal level "parent")) 
                       (intern (format "log4cl-%s-%s" section level)))))
            (when cmd 
              (define-key section-map (read-kbd-macro (format "C-%c" level-char)) cmd)
              (define-key section-map (read-kbd-macro (format "%c" level-char)) cmd))))
        (set section-map-sym section-map)
        (define-key log4cl-section-map (read-kbd-macro (format "C-%c" section-char)) section-map)
        (define-key log4cl-section-map (read-kbd-macro (format "%c" section-char)) section-map))))

(defun log4cl-init-keymaps ()
  (let ((old (if (listp log4cl-last-prefix-keys)
                 log4cl-last-prefix-keys
               (list log4cl-last-prefix-keys)))
        (new (if (listp log4cl-prefix-keys) log4cl-prefix-keys
               (list log4cl-prefix-keys))))
    (log4cl-init-logging-maps)
    (dolist (key old)
      (define-key log4cl-mode-map key nil))
    (dolist (key new)
      (define-key log4cl-mode-map key log4cl-section-map))))

(defun log4cl-set-prefix-keys (sym val)
  (set sym val)
  (log4cl-init-keymaps)
  (setq log4cl-last-prefix-keys val))

(defcustom log4cl-prefix-keys '("\C-c\C-g")
  "Prefix key sequence for Log4CL commands. Can be a list of
multiple prefixes"
  :set 'log4cl-set-prefix-keys
  :group 'log4cl)

(log4cl-init-keymaps)

;; Probably there are some smarter ways to do it, but it was faster
;; for me to cut-n-paste
(progn
  (defun log4cl-root-off   (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 0))
  (defun log4cl-root-fatal (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 1))
  (defun log4cl-root-error (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 2))
  (defun log4cl-root-warn  (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 3))
  (defun log4cl-root-info  (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 4))
  (defun log4cl-root-debug (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 5))
  (defun log4cl-root-debu1 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 6))
  (defun log4cl-root-debu2 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 7))
  (defun log4cl-root-debu3 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 8))
  (defun log4cl-root-debu4 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 9))
  (defun log4cl-root-trace (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 10))
  (defun log4cl-root-debu5 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 11))
  (defun log4cl-root-debu6 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 12))
  (defun log4cl-root-debu7 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 13))
  (defun log4cl-root-debu8 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 14))
  (defun log4cl-root-debu9 (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg 15))
  (defun log4cl-root-reset (&optional arg) (interactive "P") (log4cl-cmd-set-root-level arg :reset))

  (defun log4cl-package-parent   (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg nil))
  (defun log4cl-package-off   (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 0))
  (defun log4cl-package-fatal (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 1))
  (defun log4cl-package-error (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 2))
  (defun log4cl-package-warn  (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 3))
  (defun log4cl-package-info  (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 4))
  (defun log4cl-package-debug (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 5))
  (defun log4cl-package-debu1 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 6))
  (defun log4cl-package-debu2 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 7))
  (defun log4cl-package-debu3 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 8))
  (defun log4cl-package-debu4 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 9))
  (defun log4cl-package-trace (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 10))
  (defun log4cl-package-debu5 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 11))
  (defun log4cl-package-debu6 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 12))
  (defun log4cl-package-debu7 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 13))
  (defun log4cl-package-debu8 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 14))
  (defun log4cl-package-debu9 (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg 15))
  (defun log4cl-package-reset (&optional arg) (interactive "P") (log4cl-cmd-set-package-level arg :reset))

  (defun log4cl-file-parent   (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg nil))
  (defun log4cl-file-off   (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 0))
  (defun log4cl-file-fatal (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 1))
  (defun log4cl-file-error (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 2))
  (defun log4cl-file-warn  (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 3))
  (defun log4cl-file-info  (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 4))
  (defun log4cl-file-debug (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 5))
  (defun log4cl-file-debu1 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 6))
  (defun log4cl-file-debu2 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 7))
  (defun log4cl-file-debu3 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 8))
  (defun log4cl-file-debu4 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 9))
  (defun log4cl-file-trace (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 10))
  (defun log4cl-file-debu5 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 11))
  (defun log4cl-file-debu6 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 12))
  (defun log4cl-file-debu7 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 13))
  (defun log4cl-file-debu8 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 14))
  (defun log4cl-file-debu9 (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg 15))
  (defun log4cl-file-reset (&optional arg) (interactive "P") (log4cl-cmd-set-file-level arg :reset))

  (defun log4cl-defun-parent   (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg nil))
  (defun log4cl-defun-off   (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 0))
  (defun log4cl-defun-fatal (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 1))
  (defun log4cl-defun-error (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 2))
  (defun log4cl-defun-warn  (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 3))
  (defun log4cl-defun-info  (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 4))
  (defun log4cl-defun-debug (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 5))
  (defun log4cl-defun-debu1 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 6))
  (defun log4cl-defun-debu2 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 7))
  (defun log4cl-defun-debu3 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 8))
  (defun log4cl-defun-debu4 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 9))
  (defun log4cl-defun-trace (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 10))
  (defun log4cl-defun-debu5 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 11))
  (defun log4cl-defun-debu6 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 12))
  (defun log4cl-defun-debu7 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 13))
  (defun log4cl-defun-debu8 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 14))
  (defun log4cl-defun-debu9 (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg 15))
  (defun log4cl-defun-reset (&optional arg) (interactive "P") (log4cl-cmd-set-defun-level arg :reset)))

(defun log4cl-cmd-set-root-level (arg level)
  (if log4cl-root-logger (log4cl-set-level 'log4cl-root-logger level)
    (let ((log4cl-root-logger
           (log4cl-eval `(log4cl.slime:emacs-helper '(:root t :action :info)))))
      (if log4cl-root-logger
          (log4cl-set-level 'log4cl-root-logger level)
        (error "Root logger not found")))))

(defun log4cl-cmd-set-package-level (arg level)
  (if log4cl-package-logger (log4cl-set-level 'log4cl-package-logger level)
    (let* ((p (slime-current-package))
           (p (and p (slime-pretty-package-name p)))
           (p (if (and p (null arg)) p 
                (slime-read-package-name "Package: " p)))
           (log4cl-package-logger
            (log4cl-eval `(log4cl.slime:emacs-helper '(:package ,p :action :info)))))
      (if log4cl-package-logger
          (log4cl-set-level 'log4cl-package-logger level)
        (error "Logger for package %s not found" p)))))

(defun log4cl-cmd-set-file-level (arg level)
  (if log4cl-file-logger (log4cl-set-level 'log4cl-file-logger level)
    (when (buffer-file-name)
      (let* ((p (slime-current-package))
             (p (and p (slime-pretty-package-name p)))
             (p (if (and p (null arg)) p 
                  (slime-read-package-name "Package: " p)))
             (file (buffer-file-name))
             (log4cl-file-logger
              (log4cl-eval `(log4cl.slime:emacs-helper '(:package ,p :file ,file :action :info)))))
        (if log4cl-file-logger
            (log4cl-set-level 'log4cl-file-logger level)
          (error "Logger for package %s and file %s not found" p file))))))

(defun log4cl-cmd-set-defun-level (arg level)
  (if log4cl-package-logger (log4cl-set-level 'log4cl-defun-logger level)
    (let* ((d (add-log-current-defun))
           (d (if (and d (null arg)) d 
                (slime-read-symbol-name "Defun: " d)))
           (log4cl-defun-logger
            (log4cl-eval `(log4cl.slime:emacs-helper '(:rest ,d :action :info)))))
      (if log4cl-defun-logger
          (log4cl-set-level 'log4cl-defun-logger level)
        (error "Logger for category %s not found" d)))))


(defun log4cl-set-menu-levels (sym val)
  (set sym val)
  (when (fboundp 'log4cl-redefine-menus)
    (log4cl-redefine-menus)))

(defcustom log4cl-menu-levels '(0 1 2 3 4 5 6 10 :reset)
  "Log levels shown in the menu"
  :type '(set (const :tag "Off" 0)
              (const :tag "Fatal" 1)
              (const :tag "Error" 2)
              (const :tag "Warn" 3)
              (const :tag "Info" 4)
              (const :tag "Debug" 5)
              (const :tag "Debu1" 6)
              (const :tag "Debu2" 7)
              (const :tag "Debu3" 8)
              (const :tag "Debu4" 9)
              (const :tag "Trace" 10)
              (const :tag "Debu5" 11)
              (const :tag "Debu6" 12)
              (const :tag "Debu7" 13)
              (const :tag "Debu8" 14)
              (const :tag "Debu9" 15)
              (const :tag "Reset" :reset))
  :set 'log4cl-set-menu-levels
  :group 'log4cl)

(defun log4cl-make-levels-menu (info-symbol &optional nodisplay noinherit)
  "Create a levels menu for logger specified in the value of the INFO-SYMBOL"
  (let* ((C `(and (slime-connected-p) ,info-symbol))
         (menu `(nil :active ,C
                     ,@(unless nodisplay
                         `("--space" 
                           [nil nil :label (log4cl-logger-display-name ,info-symbol)] 
                           "--space" 
                           "--single-line"))
                     ;; inherit
                     ,@(unless noinherit 
                         `(,(log4cl-level-menu-item info-symbol nil)))
                     ;; fatal through debug
                     ,@(loop for level from 0 to 15
                             when (member level log4cl-menu-levels)
                             collect (log4cl-level-menu-item info-symbol level))
                     ;; reset children
                     ,@(when (member :reset log4cl-menu-levels)
                         `(["--single-line" nil :visible (and (plusp (log4cl-children-level-count ,info-symbol)))] 
                           ["--space" nil :visible (plusp (log4cl-children-level-count ,info-symbol))] 
                           [:reset
                            ,(if (eq info-symbol 'log4cl-popup-logger)
                                 `(log4cl-set-level ',info-symbol :reset) 
                               (intern (format "log4cl-%s-reset" 
                                               (log4cl-info-symbol-to-string info-symbol))))
                            ;; (log4cl-set-level ,info-symbol :reset)
                            :label "Reset children"
                            :suffix (format "(%d)  " (log4cl-children-level-count ,info-symbol))
                            :visible (plusp (log4cl-children-level-count ,info-symbol))])))))
    menu))


(defvar log4cl-popup-logger nil)
(defvar log4cl-root-logger nil)
(defvar log4cl-package-logger nil)
(defvar log4cl-file-logger nil)
(defvar log4cl-defun-logger nil)

(defun log4cl-setup-buffer ()
  (when (slime-connected-p) 
    (let ((pkg (slime-current-package))
          (file (buffer-file-name))
          (current-defun (add-log-current-defun)))
      ;; (log-sexp pkg file current-defun log4cl-root-logger)
      (when (null log4cl-root-logger) 
        (let ((result (log4cl-eval `(log4cl.slime:get-buffer-log-menu
                                    :package ,pkg :file ,file :defun ,current-defun))))
          (setq log4cl-root-logger (first result)
                log4cl-package-logger (second result)
                log4cl-file-logger (third result)
                log4cl-defun-logger (fourth result)))))))

(defun log4cl-log-category-menu (event)
  (interactive "e")
  (let* ((point (if (featurep 'xemacs) (event-point event) 
                  (posn-point (event-end event))))
         (window (if (featurep 'xemacs) (event-window event) (caadr event)))
         (buffer (window-buffer window))
         (click-face (get-text-property point 'face buffer))
         package file rest
         package-offset
         click-target)
    (with-current-buffer buffer
      (save-excursion 
        (goto-char point)
        (let ((start (line-beginning-position))
              (end (line-end-position))
              (prop 'face)
              (object nil)
              next)
          (while (/= start end)
            (setq next (next-single-property-change start prop object end)
                  prev (get-text-property start prop object))
            (cond ((eq prev 'log4cl-package-face)
                   (setq package (buffer-substring-no-properties start next))
                   (when (and (>= point start) (< point next))
                     (setq click-target :package
                           package-offset (- point start))))
                  ((eq prev 'log4cl-file-category-face)
                   (setq file (buffer-substring-no-properties start next))
                   (when (and (>= point start) (< point next))
                     (setq click-target :file)))
                  ((eq prev 'log4cl-function-category-face)
                   (when (and (>= point start) (< point next)) 
                     (setq rest (buffer-substring-no-properties
                                 start
                                 (save-excursion
                                   (goto-char point)
                                   (if (re-search-forward "[ )]" end 'noerror)
                                       (1- (point))
                                       next)))
                           click-target :rest))))
            (setq start next)))
        (let* ((id
                (ecase click-target
                  (:package `(:package ,package :package-offset ,package-offset))
                  (:file `(:package ,package :file ,file))
                  (:rest `(:package ,package :rest ,rest))))
               (log4cl-popup-logger (log4cl-eval `(log4cl.slime:emacs-helper
                                                     '(,@id :action :info))))
               (result (and log4cl-popup-logger
                            (log4cl-popup-menu event))))
          (when result
            (log4cl-set-level 'log4cl-popup-logger (first result))))))))

(defvar log4cl-category-mouse-map (make-sparse-keymap))

(define-key log4cl-category-mouse-map [mouse-3] 'log4cl-log-category-menu)

(defvar log4cl-category-package-properties)
(defvar log4cl-category-file-properties)
(defvar log4cl-category-function-properties)
(defvar log4cl-category-level-properties)

(setq log4cl-category-package-properties (list 'face 'log4cl-package-face
                                                  'keymap log4cl-category-mouse-map
                                                  'pointer 'hand) 
      log4cl-category-file-properties (list 'face 'log4cl-file-category-face
                                               'keymap log4cl-category-mouse-map
                                               'pointer 'hand) 
      log4cl-category-function-properties (list 'face 'log4cl-function-category-face
                                                   'keymap log4cl-category-mouse-map
                                                   'pointer 'hand)
      log4cl-category-level-properties (list 'face 'log4cl-level-category-face))

(defvar log4cl-log-level-regexp)
(defvar log4cl-log-level-base-regexp)

(defvar log4cl-timestamp-regexp)
(defvar log4cl-package-regexp)
(defvar log4cl-file-regexp)
(defvar log4cl-rest-regexp)

(setq log4cl-log-level-regexp
      (concat "[[(< ]?"
              "\\(\\b"
              (regexp-opt 
               log4cl-log-level-names)
              "\\b\\)"
              "[])> ]?")
      log4cl-timestamp-regexp "\\[[[:alnum:]:,-]+\\(?: [[:alnum:]:,-]+\\)?\\]"
      log4cl-package-regexp "\\(\\<[^:() \n]+\\>\\)"
      log4cl-file-regexp "\\([^ \n]+\\.lisp\\)"
      log4cl-rest-regexp "(\\([^ \n()]+[^\n()]*\\|\\))")

;; More freeform timestamp allowing %D{%c} or [Tue 13 Nov 2012 13:49:28 -0500] type format
;; (setq log4cl-timestamp-regexp "\\[[[:alnum:]:, +-]+\\]") 

(defvar log4cl-enabled t)
(setq log4cl-enabled t)

(defun log4cl-highlight-log-message (start end)
  (when log4cl-enabled
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (if (not (eq 'slime-repl-output-face (get-text-property (point) 'face)))
            (goto-char (next-single-property-change (point) 'face nil end))
          (let ((next (next-single-property-change (point) 'face nil end))
                (case-fold-search t))
            (while (re-search-forward log4cl-log-level-regexp next t)
              (let ((bol (line-beginning-position))
                    (lim (min (line-beginning-position 2)
                              next))
                    (level-re-beg (match-beginning 0))
                    (level-re-end (match-end 0))
                    (level-beg (match-beginning 1))
                    (level-end (match-end 1))
                    time-beg time-end
                    pkg-beg pkg-end
                    file-beg file-end
                    rest-beg rest-end
                    last-point
                    done)
                (goto-char bol)
                (while (not done)
                  (skip-chars-forward " :/-" lim)
                  (if (and (>= (point) level-re-beg)
                           (< (point) level-re-end))
                      (goto-char level-re-end) 
                    (cond ((and (not time-beg) (looking-at log4cl-timestamp-regexp))
                           (setq time-beg (match-beginning 1))
                           (setq time-end (match-end 1))
                           (goto-char (match-end 0)))
                          ((and (not file-beg) (looking-at log4cl-file-regexp))
                           (setq file-beg (match-beginning 1))
                           (setq file-end (match-end 1))
                           (goto-char (match-end 0)))
                          ((and (not pkg-beg) (looking-at log4cl-package-regexp))
                           (setq pkg-beg (match-beginning 1))
                           (setq pkg-end (match-end 1))
                           (goto-char (match-end 0)))
                          ((and (not rest-beg) (looking-at log4cl-rest-regexp))
                           (setq rest-beg (match-beginning 1))
                           (setq rest-end (match-end 1))
                           (goto-char (match-end 0)))
                          (t (setq done t)))))
                ;; (when (and pkg-beg file-beg
                ;;            (save-excursion
                ;;              (goto-char (max bol (- file-end 5)))
                ;;              (looking-at "\\.lisp")))
                ;;   ;; swap package and file
                ;;   (rotatef pkg-beg file-beg)
                ;;   (rotatef pkg-end file-end))
                (setq last-point (point))
                (goto-char lim)
                (when (and rest-beg pkg-beg) 
                  (remove-text-properties bol last-point '(face nil))
                  (when file-beg 
                    (add-text-properties file-beg file-end log4cl-category-file-properties))
                  (when pkg-beg
                    (add-text-properties pkg-beg pkg-end log4cl-category-package-properties))
                  (when rest-beg
                    (add-text-properties rest-beg rest-end log4cl-category-function-properties))
                  (when level-beg
                    (add-text-properties level-beg level-end log4cl-category-level-properties)))))
            (goto-char next))))))) 


(eval-after-load 'slime-repl
  '(defadvice slime-repl-emit (around highlight-logging-category activate compile)
     (with-current-buffer (slime-output-buffer)
       (if log4cl-mode 
           (let ((start (marker-position slime-output-end)))
             (setq ad-return-value ad-do-it)
             (log4cl-highlight-log-message start (marker-position slime-output-end)))
         (setq ad-return-value ad-do-it)))))

(defun log4cl-make-menubar-menu ()
  (let ((C '(slime-connected-p)))
    ;; First level is dynamic
    ;; :filter function 
    ;;    determine current defun
    ;;    make call to slime helper for the following info
    ;;      root logger log level
    ;;      package logger log level
    ;;      file logger log level
    ;;      current defun log level
    ;; set special variable cached with that info
    ;;
    ;; generate root menu with
    ;;   root logger submenu, with current level checked
    ;;   package submenu, with current level checked
    ;;   file submenu, with current level checked
    ;;   current defun submenu with current level checked
    ;;
    `("Log4CL"
      :active ,C
      :filter log4cl-filter-top
      ("+ROOT+" :filter log4cl-filter-root
       ,@(rest (log4cl-make-levels-menu 'log4cl-root-logger t t)))
      ("Package" :filter log4cl-filter-package
       ,@(rest (log4cl-make-levels-menu 'log4cl-package-logger)))
      ("Source File" :filter log4cl-filter-file
       ,@(rest (log4cl-make-levels-menu 'log4cl-file-logger)))
      ("Current Form" :filter log4cl-filter-defun
       ,@(rest (log4cl-make-levels-menu 'log4cl-defun-logger))))))

(defun log4cl-filter-root (args)
  ;; (message "log4cl-filter-root called")
  (log4cl-setup-buffer)
  args)

(defun log4cl-filter-package (args)
  ;; (message "log4cl-filter-package called")
  (log4cl-setup-buffer)
  args)

(defun log4cl-filter-file (args)
  ;; (message "log4cl-filter-file called")
  (log4cl-setup-buffer)
  args)

(defun log4cl-filter-defun (args)
  ;; (message "log4cl-filter-defun called")
  (log4cl-setup-buffer)
  args)

(defun log4cl-filter-top (args)
  ;; (message "log4cl-filter-top called")
  (setq log4cl-root-logger nil
        log4cl-package-logger nil
        log4cl-file-logger nil
        log4cl-defun-logger nil)args)

(defun log4cl-check-connection ()
  "Load the :log4cl system on inferior-lisp side"
  (when (slime-connected-p)
    (let* ((conn (slime-current-connection)))
      (when conn 
        (let ((try (process-get conn 'log4cl-loaded))) 
          (cond ((eq try t) t)
                ((eq try 'closed) nil)
                ((not (eq (process-status conn) 'open))
                 (process-put conn 'log4cl-loaded 'closed)
                 nil)
                ;; Tried more then 5 seconds ago, to prevent
                ;; `global-log4cl-mode' trying to load log4cl
                ;; that errors out for all buffers
                ((or (null try)
                     (and (numberp try)
                          (>= (- (float-time) try) 5)))
                 ;; mark it that we trying to do it
                 (process-put conn 'log4cl-loaded (float-time)) 
                 (if (not (slime-eval `(cl:ignore-errors (asdf:load-system :log4slime))))
                     (progn 
                       (process-put conn 'log4cl-loaded (float-time))
                       (message "Failed to load log4cl lisp support"))
                   (message "Successfully loaded log4cl lisp support")
                   (process-put conn 'log4cl-loaded t)
                   t))))))))

(define-minor-mode log4cl-mode
  "\\<log4cl-mode-map>\
Support mode integrating Log4CL logging system with SLIME

Commands to configure the Root Logger
\\[log4cl-root-off]		- Set log level to OFF 
\\[log4cl-root-fatal]		- Set log level to FATAL 
\\[log4cl-root-error]		- Set log level to ERROR
\\[log4cl-root-warn]		- Set log level to WARN
\\[log4cl-root-info]		- Set log level to INFO
\\[log4cl-root-debug]		- Set log level to DEBUG
\\[log4cl-root-trace]		- Set log level to TRACE
\\[log4cl-root-reset]		- Clear log level on any child loggers

\\[log4cl-root-debu1]		- Set log level to DEBU1

Commands to configure the current package logger
\\[log4cl-package-off]		- Set log level to OFF 
\\[log4cl-package-fatal]	- Set log level to FATAL 
\\[log4cl-package-error]	- Set log level to ERROR
\\[log4cl-package-warn]		- Set log level to WARN
\\[log4cl-package-info]		- Set log level to INFO
\\[log4cl-package-debug]	- Set log level to DEBUG
\\[log4cl-package-trace]	- Set log level to TRACE
\\[log4cl-package-reset]	- Clear log level on any child loggers

Commands to configure the file Logger
\\[log4cl-file-off]		- Set log level to OFF 
\\[log4cl-file-fatal]		- Set log level to FATAL 
\\[log4cl-file-error]		- Set log level to ERROR
\\[log4cl-file-warn]		- Set log level to WARN
\\[log4cl-file-info]		- Set log level to INFO
\\[log4cl-file-debug]		- Set log level to DEBUG
\\[log4cl-file-trace]		- Set log level to TRACE
\\[log4cl-file-reset]		- Clear log level on any child loggers

Commands to configure the current form Logger
\\[log4cl-defun-off]		- Set log level to OFF 
\\[log4cl-defun-fatal]		- Set log level to FATAL 
\\[log4cl-defun-error]		- Set log level to ERROR
\\[log4cl-defun-warn]		- Set log level to WARN
\\[log4cl-defun-info]		- Set log level to INFO
\\[log4cl-defun-debug]		- Set log level to DEBUG
\\[log4cl-defun-trace]		- Set log level to TRACE
\\[log4cl-defun-reset]		- Clear log level on any child loggers

There are also 8 extra debug levels, DEBU1..DEBU4 are more specific then DEBUG
but less specific then TRACE, and DEBU5..DEBU9 come after TRACE.

By default they do not show up in the menus, but you can customize the variable
`log4cl-menu-levels' to show them.
"
  nil
  nil
  log4cl-mode-map
  (when log4cl-mode
    (log4cl-check-connection)))

(defun log4cl-redefine-menus () 
  (easy-menu-define log4cl-popup-menu nil nil (log4cl-make-levels-menu 'log4cl-popup-logger)) 
  (easy-menu-define log4cl-menu log4cl-mode-map "Log4CL" (log4cl-make-menubar-menu)))

(log4cl-redefine-menus)

(defun turn-on-log4cl-mode ()
  "Turn on `log4cl-mode' in the current buffer if appropriate"
  (interactive)
  (if (member major-mode '(lisp-mode slime-repl-mode))
      (log4cl-mode 1)
    (when (interactive-p)
      (message "This buffer does not support log4cl mode"))))

(define-globalized-minor-mode global-log4cl-mode log4cl-mode turn-on-log4cl-mode)

(provide 'log4cl)
