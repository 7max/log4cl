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

(defgroup log4slime nil
  "Customization for log4slime Slime integration"
  :prefix "log4slime-")

(defface log4slime-package-face
  '((((background dark)) (:foreground "#7474FFFFFFFF" :background "DimGray")) ; ~ cyan
    (t (:foreground "DarkRed")))
  "*Face for displaying package name category
logging event originated in"
  :group 'log4slime)

(defface log4slime-file-face
  '((((background dark)) (:foreground "#7474FFFFFFFF" :background "DimGray")) ; ~ cyan
    (t (:foreground "DarkRed")))
  "Face used for a file name category"
  :group 'log4slime)

(defface log4slime-function-face
  '((((background dark)) (:foreground "Yellow"))
    (t                   (:foreground "Blue")))
  "*Face used for categories after the package and file name"
  :group 'log4slime)

(defface log4slime-level-face
  '((t                   (:weight bold)))
  "*Face used for the log level"
  :group 'log4slime)

(defface log4slime-level-selection-face
  '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold t))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:bold t)))
  "Face for log level in `log4slime-level-selection' window"
  :group 'log4slime)

(defface log4slime-level-inherited-face
  '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold nil :slant italic))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold nil :slant italic))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:bold nil :slant italic)))
  "Face for inherited log level in `log4slime-level-selection' window"
  :group 'log4slime)


;; These are used for naming menu items and displaying, if any of them
;; change, (log4slime-redefine-menus) needs to be called. Could be made
;; defcustom later
(defvar log4slime-log-level-names
  '("Off" "Fatal" "Error" "Warn" "Info" "Debug" 
    "Debu1" "Debu2" "Debu3" "Debu4" "Trace" "Debu5" "Debu6"
    "Debu7" "Debu8" "Debu9"))

(defvar log4slime-other-command-names '("Unset" "Reset"))

(defvar log4slime-unset-menu-item-format "Inherit - %s"
  "The string used as format for the Unset menu, receieves single
argument, the parent effective log level (string)")

;; these have to match what backend prints, they are used in highlighting regexp
(defvar log4slime-real-level-names
  '("Off" "Fatal" "Error" "Warn" "Info" "Debug" 
    "Debu1" "Debu2" "Debu3" "Debu4" "Trace" "Debu5" "Debu6"
    "Debu7" "Debu8" "Debu9"))


(defun log4slime-level-name (level)
  (cond ((integerp level) (nth level log4slime-log-level-names))
        ((or (null level)
             (eq level :unset)) (nth 0 log4slime-other-command-names))
        ((eq level :reset) (nth 1 log4slime-other-command-names))
        (error "Invalid level %S" level)))

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
;; Example: if there is a logger com.openchat.log4slime.test.foo.bar with the
;; package being com.openchat.log4slime.test, then values will be as follows
;;
;;   :package         "com.openchat.log4slime.test"
;;   :rest            "foo bar"
;;   :file            "whatever.lisp"
;;   :display-name    "foo bar"
;;   
;; For the "com.openchat" logger values will be:
;; 
;;   :package         "com.openchat.log4slime.test"
;;   :rest            nil
;;   :file            nil
;;   :display-name    "com.openchat"
;;
;;
;; For the source-file-logger of whatever.lisp values will be:
;; 
;;   :package         "com.openchat.log4slime.test"
;;   :rest            nil
;;   :file            "whatever.lisp"
;;   :display-name    "whatever.lisp"
;;

(defun log4slime-logger-package (info)
  (getf info :package))
(defun log4slime-logger-file (info)
  (getf info :file))
(defun log4slime-logger-rest (info)
  (getf info :rest))
(defun log4slime-logger-display-name (info)
  (getf info :display-name))
(defun log4slime-logger-level (info)
  (getf info :level))
(defun log4slime-logger-inherited-level (info)
  (getf info :inherited-level))
(defun log4slime-children-level-count (info)
  (getf info :children-level-count))
(defun log4slime-package-offset (info)
  (getf info :package-offset))

(defun log4slime-logger-id (info)
  "Shorten up INFO to parts needed to identify the logger"
  (let ((package (log4slime-logger-package info))
        (file (log4slime-logger-file info))
        (rest (log4slime-logger-rest info))
        (package-offset (log4slime-package-offset info)))
    `(:package ,package :file ,file :rest ,rest :package-offset ,package-offset
               :root ,(getf info :root))))

(defun log4slime-logger-type (info)
  "Figure out type of INFO logger, return :root/package/file/function"
  (cond ((getf info :root)
         :root)
        ((and (getf info :file) (not (getf info :rest)))
         :file)
        ((getf info :rest)
         :function)
        (t :package)))

(defun log4slime-make-keys-string (info-symbol level)
  "Return a string describing keyboard shortcut"
  (let* ((1st-level-char 
          (ecase info-symbol
            (log4slime-root-logger (nth 0 log4slime-logger-keys))
            (log4slime-package-logger (nth 1 log4slime-logger-keys))
            (log4slime-file-logger (nth 2 log4slime-logger-keys))
            (log4slime-defun-logger (nth 3 log4slime-logger-keys))))
         (2nd-level-char
          (car (rassoc level log4slime-menu-levels))))
    (when (and 1st-level-char 2nd-level-char) 
      (format "\\[log4slime-level-selection] %c %c" 
              1st-level-char 2nd-level-char))))

(defun log4slime-level-menu-item (info-symbol level)
  "Create the easy-menu menu item that toggles the log level"
  (let* ((level (or level :unset))
         (S
          (if (eq :unset level) 
              `(memq (log4slime-logger-level ,info-symbol) '(:unset nil)) 
            `(eql ,level (log4slime-logger-level ,info-symbol))))
         (T (if (eq :unset level)
                `(format log4slime-unset-menu-item-format
                         (log4slime-level-name
                          (log4slime-logger-inherited-level ,info-symbol)))
              (log4slime-level-name level)))
         (cmd `(log4slime-set-level ',info-symbol ,level)))
    `[,level
      ,cmd
      :label ,T
      :style radio
      :selected ,S
      ,@(unless (eq info-symbol 'log4slime-popup-logger) 
          `(:keys ,(log4slime-make-keys-string info-symbol level)))
      ;; :active (not ,S)
      ]))

(defun log4slime-set-level (info-symbol level)
  "COMMAND for menu items that will change the log level"
  (let* ((info (symbol-value info-symbol))
         (id (log4slime-logger-id info))
         (level (or level :unset)))
    ;; (log-expr info id level)
    (let ((result 
           (log4slime-eval `(log4slime:emacs-helper
                          '(,@id :action :set :level ,level))))
          (type (log4slime-logger-type id)))
      (with-current-buffer (get-buffer-create " *log4slime-message*")
        (erase-buffer)
        (destructuring-bind (&optional new-level new-inherited-level) result
          ;; some sanity checks as I change protocol
          (assert (or (null new-level)
                      (integerp new-level)))
          (assert (or (eq type :root)
                      (integerp new-inherited-level)))
          ;; update in place, so formatting functions show new info
          (setf (getf info :level) new-level
                (getf info :inherited-level) new-inherited-level)
          (if (and (eq type :root) (eq level :reset))
              (insert "All loggers set to inherit level " (log4slime-format-eff-level info)) 
            (insert
             (if (eq type :root) "Root logger" "Logger ")
             (log4slime-format-fontified-logger-type info t nil ""))
            (cond ((eq level :reset)
                   (insert " children set to inherit level " (log4slime-format-eff-level info)))
                  ((not (log4slime-logger-level info)) 
                   (insert " set to inherit level " (log4slime-format-eff-level info)))
                  (t 
                   (insert " set to level " (log4slime-format-eff-level info)))))
          (message "%s" (buffer-substring (point-min) (point-max))))))))

(defun log4slime-eval (form)
  "Wrapper around `slime-eval' that ignores errors on the lisp side"
  (when (log4slime-check-connection) 
    ;; I swear it something in slime-eval screws with point sometimes
    (save-excursion
      (let ((slime-current-thread t)) 
        (slime-eval `(cl:ignore-errors ,form))))))

(defvar log4slime-goto-definition-window nil
  "Passed as WHERE to `slime-pop-to-location', can be 'WINDOW or 'FRAME too")

(defun log4slime-set-and-redefine-menus (sym val)
  (set sym val)
  (when (fboundp 'log4slime-redefine-menus)
    (log4slime-redefine-menus)))

(defcustom log4slime-menu-levels
  '((?u . :unset)
    (?o . 0)
    (?f . 1)
    (?e . 2)
    (?w . 3)
    (?i . 4)
    (?d . 5)
    (?t . 10)
    (?r . :reset))
  "
        A list that controls which log levels and other commands are
displayed in the menu and keyboard selection.

Each element of the list is a (KEY . LEVEL) cons, where KEY is
one letter character used in `log4slime-level-selection'

LEVEL is one of:
    1. Numeric level number from 0 to 15
    2. Keyword :unset for \"Unset/Inherit\" menu option
    3. Keyword :reset for \"Reset children\" menu option
"

  :tag "Shown level(s) + Shortcut"
  :type ' (set
           :entry-format "  %b %v"
           (cons :format "Unset %v" (character :format "--------- %v" ?o) (const :tag "" :unset))
           (cons :format "  Off %v" (character :format "--------- %v" ?o) (const :tag "" 0))
           (cons :format "Fatal %v" (character :format "--------- %v" ?f) (const :tag "" 1))
           (cons :format "Error %v" (character :format "--------- %v" ?e) (const :tag "" 2))
           (cons :format " Warn %v" (character :format "--------- %v" ?w) (const :tag "" 3))
           (cons :format " Info %v" (character :format "--------- %v" ?i) (const :tag "" 4))
           (cons :format "Debug %v" (character :format "--------- %v" ?d) (const :tag "" 5))
           (cons :format "Debu1 %v" (character :format "--------- %v" ?1) (const :tag "" 6))
           (cons :format "Debu2 %v" (character :format "--------- %v" ?2) (const :tag "" 7))
           (cons :format "Debu3 %v" (character :format "--------- %v" ?3) (const :tag "" 8))
           (cons :format "Debu4 %v" (character :format "--------- %v" ?4) (const :tag "" 9))
           (cons :format "Trace %v" (character :format "--------- %v" ?t) (const :tag "" 10))
           (cons :format "Debu5 %v" (character :format "--------- %v" ?5) (const :tag "" 11))
           (cons :format "Debu6 %v" (character :format "--------- %v" ?6) (const :tag "" 12))
           (cons :format "Debu7 %v" (character :format "--------- %v" ?7) (const :tag "" 13))
           (cons :format "Debu8 %v" (character :format "--------- %v" ?8) (const :tag "" 14))
           (cons :format "Debu9 %v" (character :format "--------- %v" ?9) (const :tag "" 15))
           (cons :format "Reset %v" (character :format "--------- %v" ?9) (const :tag "" :reset)))
  :set 'log4slime-set-and-redefine-menus
  :group 'log4slime
  :require 'log4slime)

(defcustom log4slime-logger-keys '(?r ?p ?f ?d)
  "A list of four characters, that are used to select root,
  package, file and defun loggers in `log4slime-level-selection'"
  :type ' (list
           (character :tag "  Root    shortcut:"  ?r)
           (character :tag "  Package shortcut:"  ?p)
           (character :tag "  File    shortcut:"  ?f)
           (character :tag "  Defun   shortcut:"  ?d))
  :set 'log4slime-set-and-redefine-menus
  :group 'log4slime
  :require 'log4slime)

(defcustom log4slime-level-selection-single-key t
  "Non-nil means level selection exits after first change.
When nil, you have to press q to exit it.  This variable can also
have the value `expert'.  In this case, the window displaying the
levels menu is not even shown"
  :group 'log4slime
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (const :tag "Expert" expert))
  :require 'log4slime)

(defun log4slime-make-levels-menu (info-symbol &optional nodisplay noinherit)
  "Create a levels menu for logger specified in the value of the INFO-SYMBOL"
  (let* ((C
          ;; For menubar loggers `log4slime-check-connection' is done in parent menu
          (if (eq info-symbol 'log4slime-popup-logger)
              `(and ,info-symbol)
            `(and (log4slime-check-connection) ,info-symbol)))
         (menu `(nil :active ,C
                     ,@(unless nodisplay
                         `([nil nil :label (log4slime-logger-display-name ,info-symbol)]
                           "--single-line"))
                     ;; inherit
                     ,@(when (and (not noinherit) 
                                  (rassoc :unset log4slime-menu-levels))
                         `(,(log4slime-level-menu-item info-symbol :unset)))
                     ;; fatal through debug
                     ,@(loop for level from 0 to 15
                             when (rassoc level log4slime-menu-levels)
                             collect (log4slime-level-menu-item info-symbol level))
                     ;; reset children
                     ,@(when (rassoc :reset log4slime-menu-levels)
                         `(["--single-line" nil :visible (and (plusp (log4slime-children-level-count ,info-symbol)))] 
                           ["--space" nil :visible (plusp (log4slime-children-level-count ,info-symbol))] 
                           [:reset
                            (log4slime-set-level ',info-symbol :reset)
                            ;; (log4slime-set-level ,info-symbol :reset)
                            :label "Reset children"
                            :suffix (format "(%d)  " (log4slime-children-level-count ,info-symbol))
                            :visible (plusp (log4slime-children-level-count ,info-symbol))])))))
    menu))


(defvar log4slime-popup-logger nil)
(defvar log4slime-root-logger nil)
(defvar log4slime-package-logger nil)
(defvar log4slime-file-logger nil)
(defvar log4slime-defun-logger nil)


(defvar log4slime-current-defun-function 'log4slime-lisp-current-defun
  "A current defun name extractor. Should return a value that is
as close as possible to what log4slime category auto-naming would
make of the form name.. For example for methods, should attempt
to return method name, followed by qualifier, and all non T
specializers.
")


(defun log4slime-lisp-current-defun ()
  "foobar :after strings strings crap")

(defun log4slime-lisp-current-defun ()
  "Extract the current defun name in the manner that is similar
to log4slime auto-naming of log categories. Handles methods, with
normal and constant EQL specializers, as well as CL-DEF /
CL-DEFINER syntax for (def (method :after) method-name ()).

Example:
  (defmethod foobar :after (a (b some-class) c (d (eql :blah))))

will return \"foobar :after some-class :blah\"

This also can be used is same as as a better alternative to
default `add-log-current-defun-function' for CL code"
  (save-excursion 
    (let ((location (point))
          name defp methodp type qualifiers specializers
          skipped-eql-p
          first-word)
      ;; find outer form
      (or (eobp) (forward-char 1))
      (beginning-of-defun)
      ;; Make sure we are really inside the defun found,
      ;; not after it.
      (when 
          (and (looking-at "\\s(")
               (progn (end-of-defun)
                      (< location (point)))
               (progn (forward-sexp -1)
                      (>= location (point))))
        (if (looking-at "\\s(")
            (forward-char 1))

        ;; find first word
        (setq first-word (slime-symbol-at-point))
        (setq defp (equalp first-word "def")
              methodp (string-match "defmethod$" first-word))
        (forward-sexp)
        ;; now after 1st word, before name
        (when defp
          ;; next sexp is type
          (goto-char (scan-sexps (point) 1)) 
          (save-excursion 
            (goto-char (scan-sexps (point) -1)) 
            (if (not (looking-at "("))
                (setq type (slime-symbol-at-point))
              (forward-char 1)
              (setq type (slime-symbol-at-point))
              (when (equalp type "method")
                (ignore-errors 
                  (while t
                    (goto-char (scan-sexps (scan-sexps (point) 2) -1))
                    (when (looking-at ":")
                      (push (slime-symbol-at-point) qualifiers)))))))
          ;; we are now after type, before name
          (setq methodp (equalp type "method")))
        ;; back to common case
        (goto-char (scan-sexps (scan-sexps (point) 1) -1))
        (setq name 
              (if (looking-at "(")
                  (save-excursion
                    (forward-char 1)
                    (goto-char (scan-sexps (scan-sexps (point) 1) -1))
                    (slime-sexp-at-point))
                (slime-sexp-at-point)))
        (goto-char (scan-sexps (scan-sexps (point) 2) -1))
        ;; now after name
        (if (not methodp) name
          (goto-char (scan-sexps (scan-sexps (point) 1) -1)) 
          (ignore-errors 
            (while (looking-at ":") 
              (push (slime-symbol-at-point) qualifiers)
              (goto-char (scan-sexps (scan-sexps (point) 2) -1))))
          ;; now at the lambda list, descend into it
          (if (not (looking-at "(")) name 
            (forward-char 1) 
            (ignore-errors
              (while t
                ;; beginning of arg
                (goto-char (scan-sexps (scan-sexps (point) 1) -1))
                (when (looking-at "(") 
                  (save-excursion
                    ;; down into specializer
                    (goto-char (scan-lists (point) 1 -1))
                    ;; beginning of symbol
                    (goto-char (scan-sexps (scan-sexps (point) 2) -1))
                    (if (not (looking-at "("))
                        (push (slime-symbol-at-point) specializers)
                      ;; Could be an EQL specializer, down into it 
                      (goto-char (scan-lists (point) 1 -1))
                      (goto-char (scan-sexps (scan-sexps (point) 1) -1))
                      ;; stop scanning specializers if not EQL
                      (unless (equalp (slime-symbol-at-point) "eql")
                        (error ""))
                      ;; down into what it is
                      (goto-char (scan-sexps (scan-sexps (point) 2) -1))
                      (backward-prefix-chars)
                      ;; only take it if its a constant, otherwise stop
                      (cond ((looking-at "'\\_<")
                             (forward-char 1)
                             (slime-symbol-at-point)
                             (push (slime-symbol-at-point) specializers))
                            ((looking-at ":")
                             (push (slime-symbol-at-point) specializers))
                            (t (error ""))))))
                (forward-sexp 1)))
            (mapconcat #'identity
                       `(,name ,@(nreverse qualifiers)
                               ,@(nreverse specializers)) " ")))))))


(defun log4slime-setup-context ()
  "Call backend to get information for root,package,file and
defun loggers based on current Emacs context. Sets the
log4slime-xxx-logger variables with returned info."
  (save-excursion 
    (when (slime-connected-p) 
      (let ((pkg (slime-current-package))
            (file (buffer-file-name))
            (current-defun (ignore-errors
                             (funcall (or log4slime-current-defun-function
                                          'log4slime-lisp-current-defun)))))
        ;; (log-expr pkg file current-defun log4slime-root-logger)
        (when (null log4slime-root-logger) 
          (let ((result (log4slime-eval
                         `(log4slime:get-buffer-log-menu
                           :package ,pkg :file ,file :defun ,current-defun))))
            (setq log4slime-root-logger (first result)
                  log4slime-package-logger (second result)
                  log4slime-file-logger (log4slime-fix-relative-file (third result))
                  log4slime-defun-logger (fourth result))))))))

(defun log4slime-logger-id-at-mouse (event)
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
            (cond ((eq prev 'log4slime-package-face)
                   (setq package (buffer-substring-no-properties start next))
                   (when (and (>= point start) (< point next))
                     (setq click-target :package
                           package-offset (- point start))))
                  ((eq prev 'log4slime-file-face)
                   (setq file (buffer-substring-no-properties start next))
                   (when (and (>= point start) (< point next))
                     (setq click-target :file)))
                  ((eq prev 'log4slime-function-face)
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
        (let ((common-args `(:package ,package :rest-all ,rest-all)))
          (ecase click-target
            (:package `(,@common-args :package-offset ,package-offset))
            (:file `(,@common-args :file ,file))
            (:rest `(,@common-args :rest ,rest))))))))


(defun log4slime-fix-relative-file (info)
  "If its a file logger, change display name to be relative to the current buffer"
  (let ((file (log4slime-logger-file info))) 
    (when (and file (file-name-absolute-p file))
      (setf (getf info :display-name) (file-relative-name file))))
  info)

(defun log4slime-log-category-menu (event)
  (interactive "e")
  (let ((id (log4slime-logger-id-at-mouse event))) 
    ;; (log-expr id)
    (let* ((log4slime-popup-logger
            (log4slime-fix-relative-file
             (log4slime-eval `(log4slime:emacs-helper
                            '(,@id :action :info)))))
           (result (and log4slime-popup-logger
                        (log4slime-popup-menu event))))
      (when result
        (log4slime-set-level 'log4slime-popup-logger (first result))))))

(defun log4slime-goto-definition (event)
  "Go to the definition where log statement comes from and scroll
to the first log statement"
  (interactive "e")
  (let ((id (log4slime-logger-id-at-mouse event))) 
    (let* ((name (log4slime-logger-rest id))
           (package (log4slime-logger-package id))
           (where log4slime-goto-definition-window)
           (xrefs (log4slime-eval `(log4slime:emacs-helper
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

(defvar log4slime-category-mouse-map (make-sparse-keymap))

(define-key log4slime-category-mouse-map [mouse-3] 'log4slime-log-category-menu)
(define-key log4slime-category-mouse-map [mouse-1] 'log4slime-goto-definition)

(defvar log4slime-category-package-properties)
(defvar log4slime-category-file-properties)
(defvar log4slime-category-function-properties)
(defvar log4slime-category-level-properties)

(setq log4slime-category-package-properties (list 'face 'log4slime-package-face
                                               'keymap log4slime-category-mouse-map
                                               'pointer 'hand) 
      log4slime-category-file-properties (list 'face 'log4slime-file-face
                                            'keymap log4slime-category-mouse-map
                                            'pointer 'hand) 
      log4slime-category-function-properties (list 'face 'log4slime-function-face
                                                'keymap log4slime-category-mouse-map
                                                'pointer 'hand)
      log4slime-category-level-properties (list 'face 'log4slime-level-face))

(defvar log4slime-log-level-regexp)
(defvar log4slime-log-level-base-regexp)

(defvar log4slime-timestamp-regexp)
(defvar log4slime-package-regexp)
(defvar log4slime-file-regexp)
(defvar log4slime-rest-regexp)

(defvar log4slime-mode-map (make-sparse-keymap))

(define-key log4slime-mode-map "\C-c\C-g" 'log4slime-level-selection)

(setq log4slime-log-level-regexp
      (concat "[[(< ]?"
              "\\(\\b"
              (regexp-opt 
               log4slime-real-level-names)
              "\\b\\)"
              "[])> ]?")
      log4slime-timestamp-regexp "\\[[[:alnum:]:,-]+\\(?: [[:alnum:]:,-]+\\)?\\]"
      log4slime-package-regexp "\\(\\<[^:() \n]+\\>\\)"
      log4slime-file-regexp "\\([^ \n]+\\.lisp\\)"
      log4slime-rest-regexp "(\\([^ \n()]+[^\n()]*\\|\\))")

;; More freeform timestamp allowing %D{%c} or [Tue 13 Nov 2012 13:49:28 -0500] type format
;; (setq log4slime-timestamp-regexp "\\[[[:alnum:]:, +-]+\\]") 

(defvar log4slime-enabled t)
(setq log4slime-enabled t)

(defun log4slime-highlight-log-message (start end)
  (when log4slime-enabled
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (if (not (eq 'slime-repl-output-face (get-text-property (point) 'face)))
            (goto-char (next-single-property-change (point) 'face nil end))
          (let ((next (next-single-property-change (point) 'face nil end))
                (case-fold-search t))
            (while (re-search-forward log4slime-log-level-regexp next t)
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
                    (cond ((and (not time-beg) (looking-at log4slime-timestamp-regexp))
                           (setq time-beg (match-beginning 1))
                           (setq time-end (match-end 1))
                           (goto-char (match-end 0)))
                          ((and (not file-beg) (looking-at log4slime-file-regexp))
                           (setq file-beg (match-beginning 1))
                           (setq file-end (match-end 1))
                           (goto-char (match-end 0)))
                          ((and (not pkg-beg) (looking-at log4slime-package-regexp))
                           (setq pkg-beg (match-beginning 1))
                           (setq pkg-end (match-end 1))
                           (goto-char (match-end 0)))
                          ((and (not rest-beg) (looking-at log4slime-rest-regexp))
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
                    (add-text-properties file-beg file-end log4slime-category-file-properties))
                  (when pkg-beg
                    (add-text-properties pkg-beg pkg-end log4slime-category-package-properties))
                  (when rest-beg
                    (add-text-properties rest-beg rest-end log4slime-category-function-properties))
                  (when level-beg
                    (add-text-properties level-beg level-end log4slime-category-level-properties)))))
            (goto-char next))))))) 


(eval-after-load 'slime-repl
  '(defadvice slime-repl-emit (around highlight-logging-category activate compile)
     (with-current-buffer (slime-output-buffer)
       (if log4slime-mode 
           (let ((start (marker-position slime-output-end)))
             (setq ad-return-value ad-do-it)
             (log4slime-highlight-log-message start (marker-position slime-output-end)))
         (setq ad-return-value ad-do-it)))))

(defun log4slime-make-menubar-menu ()
  (let ((C '(log4slime-check-connection)))
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
      :filter log4slime-filter-top
      ("+ROOT+" :filter log4slime-filter-root
       ,@(rest (log4slime-make-levels-menu 'log4slime-root-logger t t)))
      ("Package" :filter log4slime-filter-package
       ,@(rest (log4slime-make-levels-menu 'log4slime-package-logger)))
      ("Source File" :filter log4slime-filter-file
       ,@(rest (log4slime-make-levels-menu 'log4slime-file-logger)))
      ("Defun" :filter log4slime-filter-defun
       ,@(rest (log4slime-make-levels-menu 'log4slime-defun-logger))))))

(defun log4slime-filter-root (args)
  ;; (message "log4slime-filter-root called")
  (log4slime-setup-context)
  args)

(defun log4slime-filter-package (args)
  ;; (message "log4slime-filter-package called")
  (log4slime-setup-context)
  args)

(defun log4slime-filter-file (args)
  ;; (message "log4slime-filter-file called")
  (log4slime-setup-context)
  args)

(defun log4slime-filter-defun (args)
  ;; (message "log4slime-filter-defun called")
  (log4slime-setup-context)
  args)

(defun log4slime-filter-top (args)
  ;; (message "log4slime-filter-top called")
  (setq log4slime-root-logger nil
        log4slime-package-logger nil
        log4slime-file-logger nil
        log4slime-defun-logger nil)
  args)

(defun log4slime-check-connection (&optional from-mode)
  "Load the :log4slime system on inferior-lisp side"
  ;; weird, point in current buffer was moved on error, wondering what
  ;; is doing in?
  (save-excursion 
    (when (slime-connected-p)
      (let* ((conn (slime-current-connection)))
        (when conn 
          (let ((try (process-get conn 'log4slime-loaded))) 
            (cond ((eq try t) t)
                  ((eq try 'closed) nil)
                  ((not (eq (process-status conn) 'open))
                   (process-put conn 'log4slime-loaded 'closed)
                   nil)
                  ;; Tried more then 5 seconds ago, to prevent
                  ;; `global-log4slime-mode' trying to load log4slime
                  ;; that errors out for all buffers
                  ((or (null try)
                       (and (numberp try)
                            (>= (- (float-time) try)
                                (if from-mode 5 300))))
                   ;; mark it that we trying to do it
                   (process-put conn 'log4slime-loaded (float-time)) 
                   (let* ((result (slime-eval
                                   `(cl:multiple-value-bind
                                     (ok err)
                                     (cl:ignore-errors 
                                      (cl:setf (cl:get :log4slime :no-emacs-startup-message) t)
                                      (asdf:load-system :log4slime))
                                     (cl:if ok :ok (cl:princ-to-string err)))))) 
                     ;; (log-expr result)
                     (if (not (eq :ok result))
                         (progn 
                           (process-put conn 'log4slime-loaded (float-time))
                           (message "Can't load log4slime lisp support: %s." result)
                           nil)
                       ;; This hides Slime's "go forth and hack" MOTD
                       ;; on restart, so comment it out
                       ;; (message "Successfully loaded log4slime lisp support")
                       (process-put conn 'log4slime-loaded t)
                       t))))))))))

(define-minor-mode log4slime-mode
  "\\<log4slime-mode-map>\
Support mode integrating log4slime logging system with SLIME

\\[log4slime-level-selection]		- Set log level fast via keyboard

Only \"standard\" log levels show up in the menu and keyboard bindings.

There are also 8 extra debug levels, DEBU1..DEBU4 are more specific then DEBUG
but less specific then TRACE, and DEBU5..DEBU9 come after TRACE.

To make them show up in the menu, but you can customize the
variable `log4slime-menu-levels'.
"
  nil
  nil
  log4slime-mode-map
  (when log4slime-mode
    (log4slime-check-connection t)))

(defun log4slime-redefine-menus (&optional global) 
  "Redefine log4slime menus. If GLOBAL is true, make dropdown menu
global instead of local to files with `log4slime-mode' active"
  (easy-menu-define log4slime-popup-menu nil nil (log4slime-make-levels-menu 'log4slime-popup-logger)) 
  (if global 
      (easy-menu-define log4slime-menu (current-global-map) "Change Log4CL log levels" (log4slime-make-menubar-menu))
    (easy-menu-define log4slime-menu log4slime-mode-map "Change Log4CL log levels" (log4slime-make-menubar-menu))))

(log4slime-redefine-menus)

(defun turn-on-log4slime-mode ()
  "Turn on `log4slime-mode' in the current buffer if appropriate"
  (interactive)
  (if (member major-mode '(lisp-mode slime-repl-mode))
      (log4slime-mode 1)
    (when (interactive-p)
      (message "This buffer does not support log4slime mode"))))

(define-globalized-minor-mode global-log4slime-mode log4slime-mode turn-on-log4slime-mode)


(defun log4slime-format-eff-level (info)
  "Format effective log level of a logger, marked with asterisk
if its inherited from parent, and apply font property"
  (let* ((level (log4slime-logger-level info)))
    (if level
        (log4slime-format '(face log4slime-level-selection-face)
                       "%s" (upcase (log4slime-level-name level)))
      (log4slime-format '(face log4slime-level-inherited-face)
                     "%s" (upcase (log4slime-level-name
                                   (log4slime-logger-inherited-level info)))))))


(defun log4slime-layout-columns (table nsubcol width maxcol col-pad)
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


(defun log4slime-format (&rest args)
  (if (not (listp (first args)))
      (apply 'format args)
    (let ((text (apply 'format (rest args))))
      (add-text-properties 0 (length text) (first args) text)
      text)))

(defun log4slime-format-fontified-logger-type (info &optional notypep noupcase root-name)
  "Make fortified description of what kind of logger INFO, for
showing in the level selection window and messages"
  
  (or root-name (setq root-name "Root logger"))
  (let ((logger-type (log4slime-logger-type info))
        type name)
    (ecase logger-type
      (:function
       (setq type "Defun "
             name (log4slime-format '(face log4slime-function-face)
                                 "%s" (log4slime-logger-display-name info))))
      (:package
       (setq type "Package "
             name (log4slime-format '(face log4slime-package-face)
                                 "%s" (log4slime-logger-display-name info))))
      (:file
       (setq type "File "
             name (log4slime-format '(face log4slime-file-face)
                                 "%s" (log4slime-logger-display-name info))))
      (:root
       (setq type "" name root-name)))
    (when noupcase (setq type (downcase type)))
    (concat (unless notypep type) name)))


(defun log4slime-keys-case-sensitive-p (list)
  "Return if a list of keys or (key . value) pairs is case sensitive"
  (let* ((list-1 (mapcar (lambda (x)
                           (if (consp x) (car x) x)) 
                         list))
         (list-2 (delete-duplicates (mapcar 'downcase list-1))))
    (/= (length list-1) (length list-2))))

(defun log4slime-uncontrol-char (c) 
  "If C is control character, return lower case version of regular one"
  (if (and (> c 0) (<= c 26)
           (not (eql c ?\C-g)))
      (+ c (- ?a ?\C-a))
    c))

(defun log4slime-case (thing case-sensitive &optional downcase)
  "Conditionally upcase THING"
  (if case-sensitive thing
    (if downcase (downcase thing)
      (upcase thing))))

(defun log4slime-find-key (key keys)
  "Find key in alist, being generous with case and control chars"
  (let ((ckey (log4slime-uncontrol-char key))
        res alt) 
    (cond ((setq res (assoc key keys))
           (cdr res))
          ((and (/= ckey key) (setq res (assoc ckey keys)))
           (cdr res))
          ((and (/= key (downcase key))
                (setq res (assoc (downcase key) keys)))
           (cdr res))
          ((and (/= key (upcase key))
                (setq res (assoc (upcase key) keys)))
           (cdr res)))))

(defun log4slime-level-selection (&optional arg)
  "Set log level interactively by using single keys, by asking
the user to press two keys, first selecting a logger to change
and second selecting the action to perform.

Customizing `log4slime-level-selection-single-key' variable can
be used to control if this command exits after a single level
change, or continues to query until user exits by pressing \"q\"
or C-g.

It is possible to customize which log levels are displayed in the
menu, by customizing `log4slime-menu-levels' variable`
"

  (interactive)

  ;; (figure out the list of things to display)

  (let ((log4slime-root-logger nil)
        (log4slime-package-logger nil)
        (log4slime-file-logger nil)
        (log4slime-defun-logger nil)
        ;; need to copy them, because above variables are set
        ;; from menu filter, and seems getting overriten somehow
        root-info file-info package-info defun-info
        (expert log4slime-level-selection-single-key)
        e c cc logger
        level-keys
        name
        current-level done result
        (root-key (nth 0 log4slime-logger-keys))
        (package-key (nth 1 log4slime-logger-keys))
        (file-key (nth 2 log4slime-logger-keys))
        (defun-key (nth 3 log4slime-logger-keys))
        (unset-key (when (rassoc :unset log4slime-menu-levels)
                     (car (rassoc :unset log4slime-menu-levels))))
        (reset-key (when (rassoc :reset log4slime-menu-levels)
                     (car (rassoc :reset log4slime-menu-levels)))))
    (save-excursion 
      (save-window-excursion 
        (while (not done) 
          (setq log4slime-root-logger nil
                log4slime-package-logger nil
                log4slime-file-logger nil
                log4slime-defun-logger nil)
          (log4slime-setup-context)
          ;; (log-expr log4slime-root-logger log4slime-package-logger)
          (setq root-info log4slime-root-logger
                package-info log4slime-package-logger
                file-info log4slime-file-logger
                defun-info log4slime-defun-logger)
          (save-excursion 
            (let* ((orig-buffer (current-buffer))
                   (logger-keys (append
                                 (when root-info (list (cons root-key :root)))
                                 (when package-info (list (cons package-key :package)))
                                 (when file-info (list (cons file-key :file)))
                                 (when defun-info (list (cons defun-key :defun)))))
                   ;; Make menu prettier by upcasing keys like [D]: Debug 
                   ;; but disable if there are both cases of same char
                   (case1 (log4slime-keys-case-sensitive-p logger-keys))
                   (root
                    (when root-info 
                      (list
                       (format "[%c] Root logger" (log4slime-case root-key case1)) ""
                       (format " - %s" (log4slime-format-eff-level root-info)))))
                   (package
                    (when package-info 
                      (list
                       (format "[%c] Package " (log4slime-case package-key case1))
                       (log4slime-format '(face log4slime-package-face)
                                      "%s" (log4slime-logger-display-name package-info))
                       (format " - %s" (log4slime-format-eff-level package-info)))))
                   (file
                    (when file-info 
                      (list (format "[%c] File " (log4slime-case file-key case1))
                            (log4slime-format '(face log4slime-file-face)
                                           "%s" (log4slime-logger-display-name file-info))
                            (format " - %s" (log4slime-format-eff-level file-info)))))
                   (dfun
                    (when defun-info 
                      (list (format "[%c] Defun " (log4slime-case defun-key case1))
                            (log4slime-format '(face log4slime-function-face)
                                           "%s" (log4slime-logger-display-name defun-info))
                            (format " - %s" (log4slime-format-eff-level defun-info)))))
                   (choices (remove nil (list
                                         root package file dfun)))
                   (cols (log4slime-layout-columns choices 3 (- (frame-width) 6) 2 10))
                   (ncol (length cols))
                   (col 0)
                   (nrows 0)
                   is-root-p)
              ;; (log-expr choices)
              ;; (log-expr cols)
              ;; (create temp buffer)
              (when (get-buffer " *Select logger*") 
                (kill-buffer " *Select logger*"))
              (set-buffer (get-buffer-create " *Select logger*"))
              (erase-buffer)
              (insert "Current levels:      ")
              (insert
               "(" (log4slime-format '(face log4slime-level-selection-face) "set") "/"
               (log4slime-format '(face log4slime-level-inherited-face) "inherited)"))
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
          
              (unless (eq expert 'expert)
                (let* ((window-min-height 2)
                       (window-tree))
                  ;; (log-expr nrows (window-height) 
                  ;;           (- (window-height) 1 nrows))
              
                  (delete-other-windows)
                  (setq window (split-window nil (- (window-height) (1+ nrows))))
                  (set-window-buffer window (get-buffer-create " *Select logger*"))))
              (message "Choose logger: [%s] ([q]:quit [h]:help)? "
                       (log4slime-case (apply 'string (mapcar 'car logger-keys)) case1 t))
              (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
              (setq result (log4slime-find-key c logger-keys))
              (setq cc (downcase (log4slime-uncontrol-char c)))
              (cond ((eq result :defun) (setq logger defun-info))
                    ((eq result :package) (setq logger package-info))
                    ((eq result :file) (setq logger file-info))
                    ((eq result :root) (setq logger root-info is-root-p t))
                    ((or (eql cc ?q) (eql c ?\C-g))
                     (setq done 'quit))
                    ((or (eql cc ?h)
                         (eql cc ??))
                     (setq done 'help)))
              (unless (or (eq done 'quit) (eq done 'help))
                (or logger (error "Invalid input '%c'" c))
                (setq name (log4slime-format-fontified-logger-type logger)
                      choices
                      (loop for level in
                            (let ((levels (mapcar 'cdr log4slime-menu-levels)))
                              ;; Remove Unset since logger has no own level
                              (if (or is-root-p (not (log4slime-logger-level logger)))
                                  (setq levels (remove :unset levels))
                                ;; move Unset towards the end of list
                                ;; here, since we got it already listed in
                                ;; the header
                                (when unset-key
                                  (setq levels (append (remove :unset levels)
                                                       (list :unset)))))
                              ;; Remove reset if logger has set children
                              (unless (plusp (log4slime-children-level-count logger))
                                (setq levels (remove :reset levels)))
                              levels)
                            for name = (log4slime-level-name level)
                            for shortcut = (car (rassoc level log4slime-menu-levels))
                            when shortcut
                            collect (cons shortcut level)))
                (setq level-keys (copy-list choices))
                ;; Include unset key anyway in the actual keys, its silly to give user
                ;; error if hes trying to unset already unset logger, even if we don't
                ;; show option in the menu, indicating that its already unset
                (when (and unset-key (not (assoc unset-key level-keys))) 
                  (push (cons unset-key :unset) level-keys))

                ;; Make menu prettier by upcasing keys like [D]: Debug 
                ;; but disable if there are both cases of same char
                (setq case1 (log4slime-keys-case-sensitive-p level-keys))
                (log-expr case1 choices)
                (setq choices
                      (loop for (key . level) in choices 
                            do (log-expr key level)
                            collect
                            (list 
                             (format "[%c]:" (log4slime-case key case1)) 
                             (log4slime-format '(face log4slime-level-selection-face)
                                            "%s"
                                            (log4slime-level-name level)))))
                ;; (log-expr choices)
                (setq nrows 0)
                (erase-buffer)
                (insert name)
                (if (log4slime-logger-level logger)
                    (progn 
                      (setq current-level
                            (upcase 
                             (log4slime-format
                              '(face log4slime-level-selection-face) "%s"
                              (log4slime-level-name (log4slime-logger-level logger)))))
                      (insert " - " current-level)
                      (if is-root-p (insert "\n\n")
                        (insert
                         (format "   %s, will inherit parent level " 
                                 (if unset-key (format "[%c] to unset" (log4slime-case unset-key case1))
                                   "If unset"))
                         (log4slime-format '(face log4slime-level-selection-face) "%s"
                                        (upcase (log4slime-level-name (log4slime-logger-inherited-level logger))))
                         "\n\n"))
                      (incf nrows 2))
                  (setq current-level (upcase
                                       (log4slime-format
                                        '(face log4slime-level-inherited-face) "%s"
                                        (log4slime-level-name (log4slime-logger-inherited-level logger)))))
                  (insert " --- Inherited level - " current-level "\n\n")
                  (incf nrows 2))
                (setq cols (log4slime-layout-columns choices 2 (- (frame-width) 6) 100 3))
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
                (when (and reset-key (plusp (log4slime-children-level-count logger)))
                  (insert (format "      [%c]:%s (%d) children"
                                  (log4slime-case reset-key case1) 
                                  (log4slime-format '(face log4slime-level-selection-face)
                                                 "%s" (log4slime-level-name :reset))
                                  (log4slime-children-level-count logger))
                          "\n")
                  (incf nrows 1))
                (unless (eq expert 'expert) 
                  (delete-window window) 
                  ;; (log-expr nrows)
                  (setq window (split-window nil (- (window-height) (1+ nrows))))
                  (set-window-buffer window (get-buffer-create " *Select logger*")))
                (goto-char (point-min))
                (message "%s[%s] ([q]:quit [h]:help)? "
                         (if (eq expert 'expert)
                             (format "Change %s to: " current-level)
                           "Level: ")
                         (log4slime-case (apply 'string (mapcar 'car level-keys)) case1 t))
                (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
                (setq result (log4slime-find-key c level-keys))
                (setq cc (downcase (log4slime-uncontrol-char c)))
                (cond
                 (result (with-current-buffer orig-buffer
                           (log4slime-set-level 'logger result))
                         (when expert (setq done t)))
                 ((or (eql cc ?q) (eql c ?\C-g))
                  (when expert (setq done t)))
                 ((or (eql cc ?h)
                      (eql cc ??))
                  (setq done 'help))
                 (t 
                  (error "Invalid input %c" c)))))))))
    (when (eq done 'help)
      (describe-function 'log4slime-level-selection))))

(provide 'log4slime)



