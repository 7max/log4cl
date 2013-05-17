(defpackage :test.package.one
  (:use :cl)
  (:export :greetings :foobar :instantiating-a-logger-1 :instantiating-a-logger-2
           :from-a-label))

(in-package :test.package.one)

(defvar *greeting-text* "Hi There!")

(defun greetings ()
  (log:info "~A" *greeting-text*))

(greetings)

(defmethod foobar (a b (c number))
  (log:expr a b c))

(defmethod foobar ((a symbol) b (c (eql :ok)))
  (log:expr a b c))

(defmethod foobar (a b (c (eql :ok)))
  (log:expr a b c))

(defmethod foobar (a (b string) (c (eql :ok)))
  (log:expr a b c))

(defmethod foobar :after ((a string) b (c (eql :ok)))
  (log:expr a b c))

(defmethod foobar :after ((a string) (b string) (c (eql :ok)))
  (log:expr "two strings!" a b c))

(defmethod foobar :after (a b (c (eql 42)))
  (log:expr "Its forty two" a b c))

(defmethod foobar :around (a b c)
  (log:trace "Ever had a feeling someone is always watching you?")
  (log:trace a b c)
  (call-next-method))

(defun (setf greetings) (new-greeting)
  (log:info new-greeting "Its not nice to change someone's greeting but I'll do it just for you")
  (setq *greeting-text* new-greeting))

;; More esoteric stuff

(defun from-a-label (&optional runtime-logger-object)
  (labels ((i-am-a-local-function ()
             (log:debug "This is a local function")
             (when runtime-logger-object 
               (log:debug "we were passed" runtime-logger-object)
               (log:debug :logger runtime-logger-object "Lets see where this goes?"))))
    (log:info "this is main function")
    (i-am-a-local-function)))

(defun instantiate-a-logger-1 ()
  (let ((logger (log:category '(category via list))))
    (log:info "Going to call" #'from-a-label "and pass it a logger"
              "note the limitations of Log4SLime fontification")
    (from-a-label logger)))

(defun instantiate-a-logger-2 ()
  (let ((logger (log:category :category.via.keyword)))
    (log:info "Going to call" #'from-a-label "and pass it a logger")
    (from-a-label logger)))

