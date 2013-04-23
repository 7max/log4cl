
(defpackage :log4cl-test
  (:use :cl :log4cl-impl :stefil)
  (:export
   :test :test-speed
   :test-pattern-layout
   :make-expected
   :subsuite-package
   :subsuite-start
   :rand-filename
   :*tests-dir*
   :*temp-dir*))

(in-package :log4cl-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn 
    #+sbcl (declaim (sb-ext:muffle-conditions stefil::test-style-warning)) 
    (in-root-suite) 
    (defsuite* test)))

(defmacro subsuite-package (name &body defpackage-extras)
  `(progn
     (defpackage ,name
       (:use :cl :log4cl :log4cl-test :stefil)
       (:shadow #:test)
       (:export #:test)
       ,@defpackage-extras)
     (in-package ,name)))

(defmacro subsuite-start ()
  (let ((name (find-symbol (string '#:test) *package*))) 
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       #+sbcl (declaim (sb-ext:muffle-conditions stefil::test-style-warning))
       (progn 
         (in-root-suite)
         (in-suite log4cl-test:test)
         (defsuite* ,name)))))

(defparameter *file-tests-random-state* (make-random-state t))

(defun rand-filename (&optional (num-chars 5))
  (with-output-to-string (s)
    (dotimes (cnt num-chars)
      (princ (code-char
              (+ (char-code #\a)
                 (random 26 *file-tests-random-state*)))
             s))))

(defun can-create-file-p (dir)
  (when (stringp dir)
    (unless (member (elt dir (1- (length dir)))
                    '(#\\ #\/))
      (setq dir (concatenate 'string dir "/")))
    (setq dir (parse-namestring dir))
    (let ((file (merge-pathnames (rand-filename) dir))
          ok)
      (handler-case
          (with-open-file (s file :direction :output :if-does-not-exist :create)
            (print "test" s)
            (setq ok dir)))
      (ignore-errors (delete-file file))
      ok)))

(defparameter *temp-dir*
  (or (can-create-file-p "/tmp")
      (can-create-file-p (asdf::getenv "TEMP"))
      (can-create-file-p (asdf::getenv "TMP"))
      (can-create-file-p ".")))

(defparameter *tests-dir*
  (ensure-directories-exist
   (merge-pathnames (format nil "~a/" (rand-filename))
                    *temp-dir*)))

