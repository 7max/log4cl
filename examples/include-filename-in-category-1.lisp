;;
;; Some projects separate out the functionality into separate lisp
;; files, rather then separate sub-packages.
;; 
;; If a project has two logical modules foo and baz, and places them
;; in the source files "foo.lisp" and "baz.lisp", its desirable that
;; logging could be configured separately for foo and baz
;; 
;; This example shows how to customize PACKAGE-WRAPPER method, so that
;; all loggers will be children of '(PACKAGE FILENAME) category rather
;; then simply '(PACKAGE), which would accomplish the above goal
;;
;; If you load this file and then run (EXAMPLE1:TEST) the output should be
;; something similar to:
;;
;; [11:53:33] [ info] <example-1:include-file-name-in-category-1:test>
;;   * Hello World

(defpackage :example-1
  (:use :cl)
  (:export :test))

(in-package :example-1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod log:package-wrapper ((pkg (eql *package*))
                                  categories
                                  explicit-p)
    "Include file name of the default log category prefix"
    (declare (ignore explicit-p categories))
    (let ((list (call-next-method))
          (path (or *compile-file-truename* *load-truename*)))
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
