
(defun get-logger-child (parent name)
  (find name (logger-children parent) :key #'logger-name :test #'equalp))

(defun extract-parent-name (name)
  "Return the part of the string before the last dot or nil"
  (awhen (position #\. name :from-end t)
    (subseq name 0 it))

(defun get-logger (name)
  "Get existing or create a new logger"
  (acond ((gethash name *loggers*) it)
         (t (let ((parent-name (extract-parent-name name))) 
              (let ((parent (get-logger parent-name)))
                (assert (null (get-logger-child parent name)))
                (let ((logger (make-logger :name name :parent parent)))
                  (setf (gethash name *loggers*) logger))))
            ))
