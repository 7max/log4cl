(in-package :log4cl)

(defmacro with-log-indent ((&optional (indent '(1+ *log-indent*)))
                           &body body)
  "Executes forms in BODY with *LOG-INDENT* set to INDENT"
  `(let ((*log-indent* ,indent))
     ,@body))

(defmacro deflog-macros (&rest levels)
  (let (list)
    (dolist (level levels)
      (let ((log-macro-name (intern (format nil "LOG-~a" level)))
            (level-name (intern (format nil "+LOG-LEVEL-~a+" level))))
        (push `(defmacro ,log-macro-name (&rest args &environment env)
                 "Logs the message with the logger.

Synopsis:

 (defun foo ()
   ;; DEBUG - cl-user.foo: entered
   (log-debug \"entered\")

   ;; DEBUG - cl-user.bar: message
   (log-debug :bar \"message\")

   ;; INFO - full.logger.name: message
   (log-info :/full.logger.name \"message\")

   ;; DEBUG - cl-user.foo.test: entered
   ;; DEBUG - cl-user.foo.test: arg1=1 arg2=2
   (flet ((test (arg1 arg2)
            (log-debug \"enter\")
            (log-debug arg1 arg2)))
     (test 1 2)))

Logger category is found by examinining the first argument of the macro.

If first argument is a string then logger name is automatically determined
in a implementation dependend manner. On implementations that support it
the logger name would be package.block-name. On implementations where
it's not possible to determine the block name that is currently being compiled
the logger name would be just package.

"
                 (expand-log-with-level env ,level-name args))
              list)))
    `(progn
       ,@(nreverse list))))

(deflog-macros debug fatal error warn info user1 user2 user3 user4 
               trace user5 user6 user7 user8 user9)

(defmacro log-sexp (&rest args) 
  (let ((format 
          (with-output-to-string (*standard-output*)  
            (let ((first t))
              (dolist (arg args)
                (unless first
                  (write-string " "))
                (setf first nil)
                (format t "~s=~~s" arg))))))
    `(log-debug ,format ,@args)))

(defmacro log-trace-sexp (&rest args) 
  (let ((format 
          (with-output-to-string (*standard-output*)  
            (let ((first t))
              (dolist (arg args)
                (unless first
                  (write-string " "))
                (setf first nil)
                (format t "~s=~~s" arg))))))
    `(log-trace ,format ,@args)))


(defmacro with-log-hierarchy ((hierarchy) &body body)
  "Binds the *CURRENT-HIERARCHY* to the specified hierarchy for the
dynamic scope of BODY. HIERARCHY can be hierarchy index or name"
  `(let ((*hierarchy* (hierarchy-index ,hierarchy)))
     ,@body))

(defmacro with-package-log-hierarchy (&body body)
  "Binds the *CURRENT-HIERARCHY* to the unique hierarchy for the current
package for the dynamic scope of BODY."
  `(with-log-hierarchy (*package*) ,@body))

(defmacro in-log-hierarchy (&optional hierarchy)
  "Sets the *CURRENT-HIERARCHY* to specified hierarchy, or the default
  one when NIL"
  `(setq *hierarchy* (hierarchy-index (or ,hierarchy :default))))

(defmacro in-package-log-hierarchy ()
  "Sets the *CURRENT-HIERARCHY* to specified hierarchy, or the default
  one when NIL"
  `(in-log-hierarchy *package*))

(defmacro make-logger (&optional (arg nil arg-p) &environment env)
  (resolve-logger-form *package* env (if arg-p (list arg))))

(defmacro with-ndc-context ((context) &body body)
  "Execute forms in BODY with *NDC-CONTEXT* set to CONTEXT. The
context is printed by the %x pattern layout format"
  `(let ((*ndc-context* ,context))
     ,@body))

