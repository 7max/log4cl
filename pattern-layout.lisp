(in-package #:log4cl)

(defclass pattern-layout (layout)
  ((pattern :initarg :pattern)
   (pattern-printer :initarg printer))
  (:documentation "Pattern layout uses a configurable conversion pattern:

Inside the pattern the following formatting sequences have special meaning:

%c is replaced with full category"))

(defmethod layout-to-stream ((layout pattern-layout)
			     stream
                             logger
			     level
                             log-func)
  "Format the user log statement with the pattern layout"
  (declare (type stream stream)
           (type fixnum level)
           (type function log-func))
  (with-slots (formatter)
      layout
    (funcall formatter stream logger level log-func))
  (values))


(defstruct format-info
  (minlen 0 :type fixnum)
  (maxlen nil :type (or null fixnum))
  (right-justify nil :type boolean))

(defun format-string (string stream format-info
                      &optional (start 0)
                                (end (length string)))
  "Write STRING to the STREAM, respecting flags in FORMAT-INFO"
  (declare (type string string)
           (type stream stream)
           (type fixnum start end))
  (let* ((min (format-info-minlen format-info))
         (len (- end start))
         (max (or (format-info-maxlen format-info) len)))
    (declare (type fixnum min max len))
    (when (< max len)
      (incf start (- len max))
      (decf len (- len max)))
    (if (< len min)
        (cond ((not (format-info-right-justify format-info))
               (write-string string stream :start start :end end)
               (loop repeat (- min len)
                     do (write-char #\Space stream)))
              (t (loop repeat (- min len)
                       do (write-char #\Space stream))
                 (write-string string stream :start start :end end)))
        (write-string string stream :start start :end end))))

;; (defun format-log-level (stream info level)
;;   (format-string stream string info))

;; (defun format-string (string stream start end minlen maxlen rightpad string)
;;   "Write string to the specified stream "
;;   (declare (type fixnum start end)
;;            (type (or null fixnum) minlen maxlen)))

;; (defun make-pattern-formatter (pattern)
;;   (let ((pos 0)
;;         (formatters '()))
;;     (labels ((make-print-string-formatter (string start end)
;;                (or end (setq end (length string)))
;;                (when (< start end)
;;                  (push (lambda (stream logger level log-func)
;;                          (declare (ignore logger level log-func))
;;                          (write-string string stream start end))
;;                        formatters)))
;;              (make-print-log-level-formatter ()
;;                (push (lambda (stream logger level log-func)
;;                        (declare (ignore logger log-func))
;;                        (write-log-level level stream))))
;;              (make-)))))
