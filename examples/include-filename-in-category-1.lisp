;;
;; Some projects put "like" functionality that in other languages would have
;; been in their own sub-package, into separate files instead
;;
;; If a project has two files, module-one.lisp and module-two.lisp its desirable
;; that logging can be configured independently for all log statements
;; in either-of these file.
;; 
;; This example shows how to customize PACKAGE-WRAPPER method, so that
;; all loggers will be children of '(PACKAGE FILENAME) category rather
;; then simply '(PACKAGE)
;;
;; If you load this file and then run (EXAMPLE1:TEST) the output should be
;;
;; [11:53:33] [ info] <example-1:filename-in-logger-name:test>
;; * Hello World
;;

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
