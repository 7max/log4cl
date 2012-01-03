
;; tests
;; 1. speed of creating loggers
;; 2. amount of memory used by loggers
;; 3. FASL size of the loggers
;; 4. speed of logging

;;;
;;; function to create a list of logger names
;;; 
;;; Logger tree looks like so
;;; 
;;; root => level1 => level2 => etc
;;;


(in-package :log4cl)
  
(defun create-loggers-list (levels)
  "Create a list of logger names. Levels must be a list where
each element is a number of logger at that level.

For example (create-logger-list '(2 3)) will return (one two
one.one one.two one.three two.one two.two two.three)
"
  (cond 
    ((null levels) nil)
    (t 
     (let 
         ((names (loop for i from 1 to (car levels)
                    collect (format nil "~R" i)))
          (children (create-loggers-list (cdr levels))))
       (append names 
               (mapcan (lambda (parent)
                         (loop for child in children
                            collect (concatenate 'string parent "." child)))
                       names))))))

(defun dynamic-usage ()
  (register-groups-bind (num-bytes)
      ("Dynamic space usage is:\\s+([\\d,]+) bytes"
       (with-output-to-string (*standard-output*) (room)))
    (nth-value 0 (parse-integer (remove #\, num-bytes)))))

(defun test-create-loggers (logger-names)
  (log4cl::reset-logging-configuration)
  #+sbcl(sb-ext:gc :full t)
  #+sbcl(sb-ext:gc :full t)
  #+sbcl(sb-ext:gc :full t)
  #+sbcl(sb-ext:gc :full t)
  (let* ((room-before (dynamic-usage))
         (num-loggers (length logger-names)))
    (time (dolist (category logger-names)
      (log4cl::get-logger category)))
    #+sbcl(sb-ext:gc :full t)
    #+sbcl(sb-ext:gc :full t)
    #+sbcl(sb-ext:gc :full t)
    #+sbcl(sb-ext:gc :full t)
    (let* ((room-after (dynamic-usage))
           (diff (- room-after room-before))
           (bytes-per-logger (/ diff (coerce num-loggers 'float))))
      (format t "Creating ~d loggers used ~d bytes ~d bytes per logger~%"
              num-loggers diff bytes-per-logger)))
  (values))



