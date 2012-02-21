(in-package :log4cl)

#+sbcl
(declaim (sb-ext:always-bound
          *hierarchy* 
          *hierarchy-lock*
          *name-to-hierarchy*
          *hierarchy-max*
          *watcher-event-time*))

(declaim (type fixnum  *hierarchy-max* *hierarchy*)
	 (type hash-table *name-to-hierarchy*)
         (inline hierarchy-index
                 current-hierarchy))

(defun current-hierarchy ()
  "Return the currently active hierarchy"
  (aref *hierarchies* *hierarchy*))

(defmacro with-hierarchy-lock (&body body)
  `(with-recursive-lock-held (*hierarchy-lock*)
     ,@body))

(defun %hierarchy-index (name)
  (when (stringp name)
    (setq name (intern name)))
  (let ((h (or (gethash name *name-to-hierarchy*)
               (with-hierarchy-lock
                 (let ((h (make-instance 'hierarchy
                           :index *hierarchy-max*
                           :name name)))
                   (adjust-all-loggers-state (1+ *hierarchy-max*))
                   (setf (gethash name *name-to-hierarchy*) h)
                   (vector-push-extend h *hierarchies*)
                   (incf *hierarchy-max*)
                   h)))))
    (slot-value h 'index)))

(defun hierarchy-index (hierarchy)
  "Return the hierarchy index for the specified hierarchy. Hierarchy
must be already a number or a unique identifier suitable for comparing
using EQL. If hierarchy is a string, it will be interned in the current
package"
  (typecase hierarchy
    (number hierarchy)
    (hierarchy (slot-value hierarchy 'index))
    (t (%hierarchy-index hierarchy))))

(defun add-watch-token (token &key (test #'equal) key)
  (with-slots (watch-tokens) (current-hierarchy)
    (with-hierarchy-lock
      (unless (find token watch-tokens :test test :key key)
        (push token watch-tokens)))
    (start-hierarchy-watcher-thread)))

(defun remove-watch-token (token &key (test #'equal) key)
  (with-slots (watch-tokens) (current-hierarchy)
    (with-hierarchy-lock
      (setf watch-tokens (remove token watch-tokens :test test :key key)))))
