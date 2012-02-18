(in-package :log4cl)

#+sbcl
(declaim (sb-ext:always-bound
          *hierarchy* 
          *hierarchy-lock*
          *name-to-hierarchy*
          *hierarchy-max*))

(declaim (type fixnum  *hierarchy-max* *hierarchy*)
	 (type hash-table *name-to-hierarchy*)
         (inline hierarchy-index))

(defun %hierarchy-index (name)
  (when (stringp name)
    (setq name (intern name)))
  (let ((h (or (gethash name *name-to-hierarchy*)
               (with-recursive-lock-held (*hierarchy-lock*)
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



