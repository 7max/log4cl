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
(require 'cl)

(defface log4cl-package-face
    '((((background dark)) (:foreground "#7474FFFFFFFF" :background "DimGray")) ; ~ cyan
      (t (:foreground "DarkRed")))
  "*Face for displaying package name category
logging event originated in"
  :group 'log4cl :group 'log4cl-faces)

(defface log4cl-file-category-face
    '((((background dark)) (:foreground "#7474FFFFFFFF" :background "DimGray")) ; ~ cyan
      (t (:foreground "DarkRed")))
  "*Face used for a category corresponding to the file name where
event originated in"
  :group 'log4cl :group 'log4cl-faces)

(defface log4cl-function-category-face
    '((((background dark)) (:foreground "Yellow"))
      (t                   (:foreground "Blue")))
  "*Face used for categories after the package and file name (usually the function/method name)"
  :group 'log4cl :group 'log4cl-faces)

(defface log4cl-level-category-face
    '((t                   (:weight bold)))
  "*Face used for the log level"
  :group 'log4cl :group 'log4cl-faces)

(defface log4cl-level-selection-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold t))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold t))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold t))))
  "Face for log level in fast logger selection window"
  :group 'log4cl :group 'log4cl-faces)

(defface log4cl-level-inherited-face
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold nil :slant italic))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold nil :slant italic))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold nil :slant italic)))
  "Face for inherited log level in fast logger selection window"
  :group 'log4cl :group 'log4cl-faces)


(defvar log4cl-log-level-names
  '("Off" "Fatal" "Error" "Warn" "Info" "Debug" 
    "Debu1" "Debu2" "Debu3" "Debu4" "Trace" "Debu5" "Debu6"
    "Debu7" "Debu8" "Debu9"))

(defun log4cl-level-name (level)
  (if level 
      (nth level log4cl-log-level-names)
    "unset"))

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

(defun log4cl-make-keys-string (info-symbol level)
  "Return a string describing keyboard shortcut"
  (let* ((1st-level-char 
          (ecase info-symbol
            (log4cl-root-logger ?r)
            (log4cl-package-logger ?p)
            (log4cl-file-logger ?f)
            (log4cl-defun-logger ?d)))
         (name (log4cl-level-name level))
         (2nd-level-char
          (downcase (aref name
                          (if (string-match "debu[0-9]" name)
                              4 0))))
         (result 
          (format "\\[log4cl-fast-level-selection] %c %c" 
                  1st-level-char 2nd-level-char)))
    (log-expr result)
    result))

(defun log4cl-level-menu-item (info-symbol level)
  "Create the easy-menu menu item that toggles the log level"
  (let* ((S `(eql ,level (log4cl-logger-level ,info-symbol)))
         (T (if level (log4cl-level-name level)
              `(format "Inherit - %s"
                       (log4cl-level-name
                        (log4cl-logger-inherited-level ,info-symbol)))))
         (cmd `(log4cl-set-level ',info-symbol ,level)))
    `[,(or level :unset)
      ,cmd
      :label ,T
      :style radio
      :selected ,S
      ,@(unless (eq info-symbol 'log4cl-popup-logger) 
          `(:keys ,(log4cl-make-keys-string info-symbol level)))
      ;; :active (not ,S)
      ]))

(defun log4cl-set-level (info-symbol level)
  "COMMAND for menu items that will change the log level"
  (let* ((info (symbol-value info-symbol))
         (id (log4cl-logger-id info)))
    ;; (log-expr info id level)
    (let ((result 
           (log4cl-eval `(log4cl.slime:emacs-helper
                         '(,@id :action :set :level ,level)))))
      (when (stringp result)
        (message result)))))

(defun log4cl-eval (form)
  "Wrapper around `slime-eval' that ignores errors on the lisp side"
  (when (log4cl-check-connection) 
    (slime-eval `(cl:ignore-errors ,form))))

(defvar log4cl-last-prefix-keys nil)

(defvar log4cl-goto-definition-window nil
  "Passed as WHERE to `slime-pop-to-location', can be 'WINDOW or 'FRAME too")

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
  "A list that controls which log levels should be displayed in
the menus, and in the fast keyboard selection. Each element of the list can be a log level
number, or a keyword :reset for the \"Reset child loggers\" action"
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
  (let* ((C
          ;; For menubar loggers `log4cl-check-connection' is done in parent menu
          (if (eq info-symbol 'log4cl-popup-logger)
              `(and ,info-symbol)
            `(and (log4cl-check-connection) ,info-symbol)))
         (menu `(nil :active ,C
                     ,@(unless nodisplay
                         `( 
                           [nil nil :label (log4cl-logger-display-name ,info-symbol)]
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
                            (log4cl-set-level ',info-symbol :reset)
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
  "Call backend to get information for root,package,file and
defun loggers based on current Emacs context. Sets the
log4cl-xxx-logger variables with returned info."
  (when (slime-connected-p) 
    (let ((pkg (slime-current-package))
          (file (buffer-file-name))
          (current-defun (add-log-current-defun)))
      ;; (log-expr pkg file current-defun log4cl-root-logger)
      (when (null log4cl-root-logger) 
        (let ((result (log4cl-eval `(log4cl.slime:get-buffer-log-menu
                                    :package ,pkg :file ,file :defun ,current-defun))))
          (setq log4cl-root-logger (first result)
                log4cl-package-logger (second result)
                log4cl-file-logger (third result)
                log4cl-defun-logger (fourth result)))))))

(defun log4cl-logger-id-at-mouse (event)
  "Find the package,file,and logger name at the site of the mouse click,
returning them in the plist form, suitable for Lisp side
EMACS-HELPER."
  (let* ((point (if (featurep 'xemacs) (event-point event) 
                  (posn-point (event-end event))))
         (window (if (featurep 'xemacs) (event-window event) (caadr event)))
         (buffer (window-buffer window))
         (click-face (get-text-property point 'face buffer))
         package file rest
         package-offset
         click-target
         rest-all)
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
                   (unless rest-all 
                     (setq rest-all (buffer-substring-no-properties
                                     start next)))
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
        (ecase click-target
          (:package `(:package ,package :package-offset ,package-offset :rest-all ,rest-all))
          (:file `(:package ,package :file ,file :rest-all ,rest-all))
          (:rest `(:package ,package :rest ,rest :rest-all ,rest-all)))))))

(defun log4cl-log-category-menu (event)
  (interactive "e")
  (let ((id (log4cl-logger-id-at-mouse event))) 
    ;; (log-expr id)
    (let* ((log4cl-popup-logger (log4cl-eval `(log4cl.slime:emacs-helper
                                               '(,@id :action :info))))
           (result (and log4cl-popup-logger
                        (log4cl-popup-menu event))))
      (when result
        (log4cl-set-level 'log4cl-popup-logger (first result))))))

(defun log4cl-goto-definition (event)
  (interactive "e")
  (let ((id (log4cl-logger-id-at-mouse event))) 
    (let* ((name (log4cl-logger-rest id))
           (package (log4cl-logger-package id))
           (where log4cl-goto-definition-window)
           (xrefs (log4cl-eval `(log4cl.slime:emacs-helper
                                 '(,@id :action :get-location)))))
      ;; Pasted from `slime-edit-definition-cont'
      (destructuring-bind (1loc file-alist) (slime-analyze-xrefs xrefs)
        (cond ((null xrefs) 
               (if name (error "No known definition for: %s (in %s)" name package)
                 (error "No definition found")))
              (1loc
               (slime-push-definition-stack)
               (slime-pop-to-location (slime-xref.location (car xrefs)) where)
               (let ((end (save-excursion (forward-sexp) (point))))
                 (when (re-search-forward "^[ \t]*\\((log:\\)" end t)
                   (goto-char (match-beginning 1)))))
              ((slime-length= xrefs 1)  ; ((:error "..."))
               (error "%s" (cadr (slime-xref.location (car xrefs)))))
              (t
               (slime-push-definition-stack)
               (slime-show-xrefs file-alist 'definition name
                                 package)))))))

(defvar log4cl-category-mouse-map (make-sparse-keymap))

(define-key log4cl-category-mouse-map [mouse-3] 'log4cl-log-category-menu)
(define-key log4cl-category-mouse-map [mouse-1] 'log4cl-goto-definition)

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

(defvar log4cl-mode-map (make-sparse-keymap))

(define-key log4cl-mode-map "\C-c\C-g" 'log4cl-fast-level-selection)

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
  (let ((C '(log4cl-check-connection)))
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
      ("Defun" :filter log4cl-filter-defun
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
        log4cl-defun-logger nil)
  args)

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
                 (let* ((result (slime-eval `(cl:multiple-value-bind
                                              (ok err) (cl:ignore-errors 
                                                        (asdf:load-system :log4slime))
                                              (cl:if ok :ok (cl:prin1-to-string err)))))) 
                   ;; (log-expr result)
                   (if (not (eq :ok result))
                       (progn 
                         (process-put conn 'log4cl-loaded (float-time))
                         (message "Can't load log4cl lisp support: %s" result)
                         nil)
                     (message "Successfully loaded log4cl lisp support")
                     (process-put conn 'log4cl-loaded t)
                     t)))))))))

(define-minor-mode log4cl-mode
  "\\<log4cl-mode-map>\
Support mode integrating Log4CL logging system with SLIME

\\[log4cl-fast-level-selection]		- Set log level fast via keyboard

Only \"standard\" log levels show up in the menu and keyboard bindings.

There are also 8 extra debug levels, DEBU1..DEBU4 are more specific then DEBUG
but less specific then TRACE, and DEBU5..DEBU9 come after TRACE.

To make them show up in the menu, but you can customize the
variable `log4cl-menu-levels'.
"
  nil
  nil
  log4cl-mode-map
  (when log4cl-mode
    (log4cl-check-connection)))

(defun log4cl-redefine-menus (&optional global) 
  "Redefine Log4CL menus. If GLOBAL is true, make dropdown menu
global instead of local to files with `log4cl-mode' active"
  (easy-menu-define log4cl-popup-menu nil nil (log4cl-make-levels-menu 'log4cl-popup-logger)) 
  (if global 
      (easy-menu-define log4cl-menu (current-global-map) "Log4CL" (log4cl-make-menubar-menu))
    (easy-menu-define log4cl-menu log4cl-mode-map "Log4CL" (log4cl-make-menubar-menu))))

(log4cl-redefine-menus)

(defun turn-on-log4cl-mode ()
  "Turn on `log4cl-mode' in the current buffer if appropriate"
  (interactive)
  (if (member major-mode '(lisp-mode slime-repl-mode))
      (log4cl-mode 1)
    (when (interactive-p)
      (message "This buffer does not support log4cl mode"))))

(define-globalized-minor-mode global-log4cl-mode log4cl-mode turn-on-log4cl-mode)


(defun log4cl-format-eff-level (info)
  "Format effective log level of a logger, marked with asterisk
if its inherited from parent, and apply font property"
  (let* ((level (log4cl-logger-level info)))
    (if level
        (log4cl-format '(face log4cl-level-selection-face)
                       "%s" (upcase (log4cl-level-name level)))
      (log4cl-format '(face log4cl-level-inherited-face)
                     "%s" (upcase (log4cl-level-name
                                   (log4cl-logger-inherited-level info)))))))


(defun can-lay-down-in-columns (table nsubcol width maxcol col-pad)
  "Given a table like ((aa bbb ccc) (aaaa bbbb ccc)) calculate how many columns we can
  lay it down with in a given width, so each element aligns below the other"
  (let* ((cw '())
         (ncol (if maxcol (min (length table)
                               maxcol)
                 (length table)))
         ret)
    (while (plusp ncol)
      (catch 'exit 
        (setq cw (loop repeat ncol collect (loop repeat nsubcol collect 0))) 
        ;; lay down
        (let ((row 0)
              (col 0)
              (subcol 0))
          (dolist (e table)
            (setq subcol 0)
            (dolist (e e) 
              (let* ((len (max
                           (nth subcol (nth col cw))
                           (length e))))
                (setf (nth subcol (nth col cw)) len)
                (incf subcol)))
            (incf col)
            (when (= col ncol)
              (setq col 0)
              (incf row)))
          ;; Count total width
          (let ((total 0))
            (dolist (c1 cw)
              (dolist (c2 c1)
                (incf total c2)))
            (incf total (* (1- ncol) col-pad))
            ;; See if exeeds max, and try with less columns if it does
            (when (> total width) 
              (decf ncol)
              (throw 'exit nil))
            ;; Found number of columns that is sufficient
            (setq ncol 0 ret cw) 
            (throw 'exit nil)))))
    cw))



(defun log4cl-format (&rest args)
  (if (not (listp (first args)))
      (apply 'format args)
    (let ((text (apply 'format (rest args))))
      (add-text-properties 0 (length text) (first args) text)
      text)))

(defun log4cl-fast-level-selection (&optional arg)
  "Set log level interactively"
  (interactive)

  ;; (figure out the list of things to display)
  (let ((log4cl-root-logger nil)
        (log4cl-package-logger nil)
        (log4cl-file-logger nil)
        (log4cl-defun-logger nil)
        ;; need to copy them, because above variables are set
        ;; from menu filter, and seems getting overriten somehow
        root-info file-info package-info defun-info
        expert e c logger is-root-p type
        level-keys
        name)
    (log4cl-setup-buffer)
    ;; (log-expr log4cl-root-logger log4cl-package-logger)
    (let* ((root
            (when (setq root-info log4cl-root-logger) 
              (list
               "[R] Root" ""
               (format " - %s" (log4cl-format-eff-level log4cl-root-logger)))))
           (package
            (when (setq package-info log4cl-package-logger) 
              (list
               "[P] Package "
               (log4cl-format '(face log4cl-package-face)
                              "%s" (log4cl-logger-display-name log4cl-package-logger))
               (format " - %s" (log4cl-format-eff-level log4cl-package-logger)))))
           (file
            (when (setq file-info log4cl-file-logger) 
              (list "[F] File "
                    (log4cl-format '(face log4cl-file-category-face)
                                   "%s" (log4cl-logger-display-name log4cl-file-logger))
                    (format " - %s" (log4cl-format-eff-level log4cl-file-logger)))))
           (dfun
            (when (setq defun-info log4cl-defun-logger) 
              (list "[D] Defun "
                    (log4cl-format '(face log4cl-function-category-face)
                                   "%s" (log4cl-logger-display-name log4cl-defun-logger))
                    (format " - %s" (log4cl-format-eff-level log4cl-defun-logger)))))
           (choices (remove nil (list
                                 root package file dfun)))
           (cols (can-lay-down-in-columns choices 3 (- (frame-width) 6) 2 10))
           (ncol (length cols))
           (col 0)
           (nrows 0))
      ;; (log-expr choices)
      ;; (log-expr cols)
      ;; (create temp buffer)
      (save-window-excursion
        (when (get-buffer " *Select logger*") 
          (kill-buffer " *Select logger*"))
        (set-buffer (get-buffer-create " *Select logger*"))
        (erase-buffer)
        (insert "Current levels:      ")
        (insert
         "(" (log4cl-format '(face log4cl-level-selection-face) "set") "/"
         (log4cl-format '(face log4cl-level-inherited-face) "inherited)"))
        (insert "\n\n")
        (incf nrows 2)
        ;; (print text to the buffer)
        (while (setq e (pop choices))
          (let ((cw1 (nth 0 (nth col cols)))
                (cw2 (nth 1 (nth col cols)))
                (cw3 (nth 2 (nth col cols)))) 
            (when (zerop col)
              (insert-char ?  6))
            (insert (first e))
            (insert-char ?  (- cw1 (length (first e))))
            (insert (second e))
            (insert-char ?  (- cw2 (length (second e))))
            (insert (third e))
            (insert-char ?  (- cw3 (length (third e))))
            (incf col) 
            (if (< col ncol)
                (insert (make-string 10 ?\ ))
              (insert "\n")
              (setq col 0)
              (incf nrows))))
        ;; (insert "\n")
        (unless (zerop col)
          (insert "\n")
          (incf nrows))
        (goto-char (point-min))
        (let ((note "(*) inherited level"))
          
          (setq mode-line-format
                (format "%s%s"
                        (make-string (/ (- (frame-width) 5 (length note)) 2) ?\ )
                        note))
          (setq mode-line-format nil))
        
        (unless expert
          (let* ((window-min-height 2)
                 (window-tree))
            ;; (log-expr nrows (window-height) 
            ;;           (- (window-height) 1 nrows))
            
            (delete-other-windows)
            (sit-for 0)
            (setq window (split-window nil (- (window-height) (1+ nrows))))
            (set-window-buffer window (get-buffer-create " *Select logger*"))))
        (message "Choose logger: [%s%s%s%s] or ([q] to quit)? "
                 (if dfun "d" "")
                 (if package "p" "")
                 (if file "f" "")
                 (if root "r" ""))
        (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
        (cond ((and dfun (or (eql (downcase c) ?d) (eql c "\C-d")))
               (setq logger defun-info type 'dfun
                     name (concat "Defun "
                                  (log4cl-format '(face log4cl-function-category-face)
                                                 "%s" (log4cl-logger-display-name logger)))))
              ((and package (or (eql (downcase c) ?p) (eql c "\C-p")))
               (setq logger package-info
                     type 'package
                     name (concat "Package "
                                  (log4cl-format '(face log4cl-package-face)
                                                 "%s" (log4cl-logger-display-name logger)))))
              ((and file (or (eql (downcase c) ?f) (eql c "\C-f")))
               (setq logger file-info
                     type 'file
                     name (concat "File "
                                  (log4cl-format '(face log4cl-file-category-face)
                                                 "%s" (log4cl-logger-display-name logger)))))
              ((and root (or (eql (downcase c) ?r) (eql c "\C-r")))
               (setq logger root-info
                     type 'root
                     name "Root logger"))
              ((or (eql (downcase c) ?q) (eql c "\C-q"))
               (setq logger 'quit)))
        (unless (eq logger 'quit)
          (or logger (error "Invalid input '%c'" c))
          (setq choices 
                (loop for level from 0 to 15
                      for name = (log4cl-level-name level)
                      when (member level log4cl-menu-levels)
                      collect (let ((level-char (aref name
                                                      (if (string-match "debu[0-9]" name)
                                                          4 0))))
                                (list
                                 level-char level
                                 (format "[%c]:" level-char)
                                 (log4cl-format '(face log4cl-level-selection-face)
                                                "%s"
                                                (log4cl-level-name level))))))
          
          (setq level-keys (mapcar (lambda (x)
                                     (cons (downcase (first x)) (second x)))
                                   choices)
                choices (mapcar 'cddr choices))
          ;; (log-expr choices)
          (setq nrows 0)
          (erase-buffer)
          (insert name)
          (if (log4cl-logger-level logger)
              (progn 
                (insert " - " (upcase
                               (log4cl-format
                                '(face log4cl-level-selection-face) "%s"
                                (log4cl-level-name (log4cl-logger-level logger)))))
                (if (eq type 'root)
                    (insert "\n\n")
                  (insert "   [U] to unset, will inherit parent level "
                          (log4cl-format '(face log4cl-level-selection-face) "%s"
                                         (upcase (log4cl-level-name (log4cl-logger-inherited-level logger))))
                          "\n\n")
                  (push (cons ?u nil) level-keys))
                (incf nrows 2))
            (insert " --- Inherited level - "
                    (upcase
                     (log4cl-format
                      '(face log4cl-level-inherited-face) "%s"
                      (log4cl-level-name (log4cl-logger-inherited-level logger))))
                    "\n\n")
            (incf nrows 2))
          (setq cols (can-lay-down-in-columns choices 2 (- (frame-width) 6) 100 3))
          (setq ncol (length cols))
          (setq col 0)
          ;; (log-expr ncol)
          (while (setq e (pop choices))
            (when (zerop col)
              (insert-char ?  6))
            (let ((cw1 (nth 0 (nth col cols)))
                  (cw2 (nth 1 (nth col cols)))) 
              (insert (first e))
              (insert-char ?  (- cw1 (length (first e))))
              (insert (second e))
              (insert-char ?  (- cw2 (length (second e))))
              (incf col) 
              (if (< col ncol)
                  (insert (make-string 3 ?\ ))
                (insert "\n")
                (setq col 0)
                (incf nrows))))
          (unless (zerop col)
            (insert "\n")
            (incf nrows))
          (when (and (member :reset log4cl-menu-levels)
                     (plusp (log4cl-children-level-count logger)))
            (insert (format "      [R]:%s (%d) children"
                            (log4cl-format '(face log4cl-level-selection-face)
                                           "%s" "Reset")
                            (log4cl-children-level-count logger))
                    "\n")
            (incf nrows 1)
            (push (cons ?r :reset) level-keys))
          (delete-window window)
          ;; (log-expr nrows)
          (setq window (split-window nil (- (window-height) (1+ nrows))))
          (set-window-buffer window (get-buffer-create " *Select logger*"))
          (goto-char (point-min))
          (message "Choice: ([q] to quit)? ")
          (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
          (unless (assoc c level-keys) (error "Invalid input %c" c))
          (setq c (cdr (assoc c level-keys)))
          (log4cl-set-level 'logger c))))))

(provide 'log4cl)



