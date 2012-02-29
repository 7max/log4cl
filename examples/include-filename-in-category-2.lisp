;;
;; If you had run include-filename-in-category-1 example, you may have noticed that
;; if you use SLIME C-c C-c key to recompile the TEST function, the category name
;; that it logs to changes from
;; 
;; [11:53:33] [ info] <example-1:filename-in-logger-name:test>
;;   * Hello World
;;
;; to something like
;;
;; [11:58:04] [ info] <example-1:fileny25yx:test>
;;   * Hello World
;;
;; This is because C-c C-c puts the form into a temporary file,
;; /tmp/fileny25yx and compiles that.
;;
;; Its possible to fix that, by patching Slime's COMPILE-STRING-FOR-EMACS function
;; to bind the filename the form comes from to a variable, and reference that
;; in our PACKAGE-WRAPPER method
;;
;; The code between the (IN-PACKAGE :SWANK) and (IN-PACKAGE :EXAMPLE-2) can
;; be put into ~/.swank.lisp initialization file, so it can be reused
;; in multiple systems and packages
;; 

(defpackage :example-2
  (:use :cl)
  (:export :test))

;; ------------------ begin copy me to ~/.swank.lisp ------------------------
(in-package :swank)

(defvar *compile-string-filename* nil
  "File where form being compiled by Slime `slime-compile-defun' comes from")

(export '*compile-string-filename*)

(defvar *old-compile-string-for-emacs*
  (fdefinition 'compile-string-for-emacs))

;; patch the COMPILE-STRING-FOR-EMACS
(setf (fdefinition 'compile-string-for-emacs)
      (lambda (string buffer position filename policy)
        (let ((*compile-string-filename* filename))
          (funcall *old-compile-string-for-emacs*
                   string buffer position filename policy))))

;; ------------------ end of copy me to ~/.swank.lisp ------------------------
(in-package :example-2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod log:package-wrapper ((pkg (eql *package*))
                                categories
                                explicit-p)
  "Include file name of the default log category prefix"
    (declare (ignore explicit-p categories))
  (let ((list (call-next-method))
        (path (or
               (and swank::*compile-string-filename*
                    (pathname swank::*compile-string-filename*))
               *compile-file-truename* *load-truename*)))
    (if (not path) list
        `(;; package shortest nickname
          ,(first list) 
          ;; need read-from-string, so that symbol respects
          ;; the readtable, otherwise we may get logger like PACKAGE:filename
          ;; rather then PACKAGE:FILENAME
          ,(read-from-string (pathname-name path)) 
          ,@(rest list))))))

(defun test ()
  (log:info "Hello World"))
