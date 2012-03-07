;;
;; Shows how you can customize (LOG:EXPR) to print expressions and
;; values in a different way
;;
;; EXAMPLE-3> (test)
;; 
;; [11:11:48] [debug] <example-3:test>
;;   * values are A: 1  B: (TWO THREE)
;;   C: (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
;;   


(defpackage :example-3
  (:use :cl)
  (:export :test))

(in-package :example-3)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod log:naming-option ((pkg (eql *package*))
                                option)
    "Switch (log:expr) to do A: foobar   B: foobaz"
    (declare (ignore pkg))
    (case option
      (:expr-value-separator ": ")
      (:expr-value-suffix "  ~:_")
      (t (call-next-method)))))

(defun test ()
  (let ((a 1) (b '(two three))
        (c (loop for i from 1 to 15 collect i)))
    (log:expr "values are" a b c)))


