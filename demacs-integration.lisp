
(cl:in-package :log4cl)

(defgeneric wrap-with-logger-name (definer forms logger-name))

(defgeneric create-logger-name (definer)
  (:documentation "Generate automatic logger name for the definer.")
  (:method ((definer definer))
    (let ((symb (demacs::name-of definer)))
      (format nil "~a.~a" 
              (shortest-package-name *package*)
              symb)))
  ;; for methods, append the types of non-T specializers after the method name
  #-sbcl
  (:method ((definer demacs:method-definer))
    (multiple-value-bind
          (specializers qualifiers)
        (if (keywordp (demacs::lambda-list-of definer))
            ;; handle the case of (def method foo :after
            ;; (&lambda-list)) in above case demacs should have given
            ;; an error but instead it has lambda-list as :after and
            ;; the actual lambda list is CAR of the body.
            ;;
            ;; Since I defined a lot of my after methods with above technically
            ;; invalid syntax, support it here
            (values
             (first (demacs::body-of definer))
             (list (demacs::lambda-list-of definer)))
            (values
             (demacs::lambda-list-of definer)
             (demacs::qualifiers-of definer)))
      (let (;; make a method description in the form of
            ;; (method-name qualifiers (types of args)
            ;; for example (a :after (STRING T (EQL 'BLAH)))
            (method-description
              `(,(demacs::name-of definer)
                 ,@qualifiers
                 ,(loop for arg in specializers
                        if (atom arg) collect t else
                        collect (second arg)))))
        ;; the method qualifiers will be appended with : after method name
        ;; ie logger for initialize-instance :around ((foo bar) &key) will be
        ;; package.initialize-instance:around.bar
        (flet ((ensure-string (atom)
                 (cond
                   ((keywordp atom) (prin1-to-string atom))
                   ((symbolp atom) (symbol-name atom))
                   ((stringp atom) atom)
                   (t (prin1-to-string atom)))))
          (reduce
           (lambda (name1 name2)
             (concatenate 'string (ensure-string name1)
                          "." (ensure-string name2)))
           (append (list (shortest-package-name *package*))
                   (list method-description))))))))

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
#+nil
(defmethod wrap-with-logger-name ((definer function-definer) forms logger-name)
  `(progn
     (eval-when (:compile-toplevel :execute)
       (setq *default-logger-name* ,logger-name)) 
     ,forms
     (eval-when (:compile-toplevel :execute)
       (setq *default-logger-name* nil))))
  
;; ;; its a subclass of function-definer, so have to overwrite it to do nothing
;; #+sbcl
;; (defmethod wrap-with-logger-name ((definer macro-definer) forms logger-name)
;;   forms)
;; ;; same thing for the method
;; #+sbcl
;; (defmethod wrap-with-logger-name ((definer method-definer) forms logger-name)
;;   forms)

#-sbcl
(defmethod expand-definer :around ((definer definer))
  (let ((logger-category (create-logger-name definer)))
    (let ((forms
           (call-next-method)))
      (wrap-with-logger-name definer forms logger-name))))




