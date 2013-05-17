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


(defpackage :customize-log-expr
  (:use :cl)
  (:export :test))

(in-package :customize-log-expr)

;; Use even prettier printing of expressions, at expanse
;; of slightly more code size/consing
;;
;; "~:_~<~(~W~): ~2I~_~W~:> "
;; 
(log:package-options :expr-print-format log4cl:+expr-format-fancy+)

(defun test ()
  (let ((a 1) (b '(two three))
        (c (loop for i from 1 to 15 collect i)))
    (log:info "values are" a b c)))


