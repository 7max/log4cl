(in-package #:log4cl-impl)

(defclass layout () ()
  (:documentation "Abstract layout class"))

(defgeneric layout-to-stream (layout stream logger level log-func)
  (:documentation
   "Prints the log message to the specified stream. log message can is
specified indirectly by LOG-FUNC argument, which is a callable object
that accepts a stream and writes log message to it"))

(defmethod property-alist ((instance layout))
  "Abstract layout has no properties"
  '())

