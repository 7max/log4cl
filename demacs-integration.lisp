
(cl::in-package :cl-log)

(defgeneric wrap-with-logger-name (definer forms logger-name))

;; default method do not do anything
(defmethod wrap-with-logger-name ((definer definer) forms logger-name)
  (declare (ignore definer logger-name))
  forms)

;; for clisp wrapping into compiler-let works as intented
;; (ie very well)
#+clisp
(defmethod wrap-with-logger-name ((definer definer) forms logger-name)
  `(cl-user::compiler-let ((*default-logger-name* ,logger-name))
     ,forms))

;; In SBCL the compiler-let seems to be broken a bit.. Putting defclass/defstruct
;; defmacro/defconstant inside of (compiler-let) or (let) causes the corresponding
;; object to be only defined after the whole file is loaded. Its not to say that
;; the ANSI def* form must be on the top level, SBCL seems ok with these being inside
;; of (progn) but wrapping them into let/compiler-let breaks things.
;;
;; Therefore for SBCL define just these definers that we know do work
#+sbcl
(defmethod wrap-with-logger-name ((definer special-variable-definer) forms logger-name)
  (assert (and (eq (first forms) 'progn)
               (consp (second forms))
               (eq (caadr forms) 'defvar)))
  `(progn
     ,(cadr forms)
     (cl-user::compiler-let ((*default-logger-name* ,logger-name))
       ,@(cddr forms))))

#+sbcl
(defmethod wrap-with-logger-name ((definer function-definer) forms logger-name)
  `(cl-user::compiler-let ((*default-logger-name* ,logger-name))
    ,forms))
  
;; ;; its a subclass of function-definer, so have to overwrite it to do nothing
;; #+sbcl
;; (defmethod wrap-with-logger-name ((definer macro-definer) forms logger-name)
;;   forms)
;; ;; same thing for the method
;; #+sbcl
;; (defmethod wrap-with-logger-name ((definer method-definer) forms logger-name)
;;   forms)

(defmethod expand-definer :around ((definer definer))
  (let ((logger-name
         (let ((symb (demacs::name-of definer)))
           (format nil "~a.~a" 
                   (shortest-package-name (symbol-package symb))
                   (symbol-name symb)))))
    (let ((forms
           (call-next-method)))
      (wrap-with-logger-name definer forms logger-name))))




