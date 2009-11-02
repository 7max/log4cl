;; This test is used to see which approach leads to the smallest
;; amount of waste as far as size of the code is concerned

(cl:defpackage :cl-log-test (:use :cl :cl-log))
(cl:in-package :cl-log-test)

(defun generate-test-file (path num-functions num-parameters with-logging)
  "Generate a /tmp/temp123.lisp file which contains `num-functions' functions
each having `num-parameters' paremeters. If `with-logging' is specified then 
the functions will log each of their parameters as well as all the parameters
together with log-debug function"
  (let ((*print-length* nil)
        (*print-pretty* t))
    (with-open-file (*standard-output* path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
      (prin1 `(in-package :cl-log-test))
      (terpri)
      (dotimes (f-num num-functions)
        (prin1 `(defun ,(intern (format nil "FUNCTION-~d" f-num))
                    (,@(loop for p-num from 0 below num-parameters
                          collect (intern (format nil "PARAMETER-~d" p-num))))
                  ,@(when with-logging
                          (append (loop for p-num from 0 below num-parameters
                                     as p-name = (format nil "PARAMETER-~d" p-num)
                                     collect `(log-debug ,(format nil "~a=~~a" p-name) ,(intern p-name)))))
                  ,@(when with-logging
                          (let ((fmt 
                                 (with-output-to-string (*standard-output*)
                                   (dotimes (p-num num-parameters)
                                     (when (plusp p-num)
                                       (princ #\Space))
                                     (format t "PARAMETER-~d=~~a" p-num))))
                                (params (loop for p-num from 0 below num-parameters
                                           collect (intern (format nil "PARAMETER-~d" p-num)))))
                            `((log-debug ,fmt ,@params))))
                  (list ,@(loop for p-num from 0 below num-parameters
                             collect (intern (format nil "PARAMETER-~d" p-num))))))
        (terpri)))))
