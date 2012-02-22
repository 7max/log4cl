(in-package :log4cl-impl)

(defvar *watcher-thread-bindings* `((*debug-io* . ,*debug-io*)))

(defun start-hierarchy-watcher-thread ()
  (unless *watcher-thread*
    (let ((logger (make-logger '(log4cl))))
      (bordeaux-threads:make-thread
       (lambda ()
         ;; prevent two watcher threads from being started due to race
         (when (with-hierarchies-lock
                 (cond (*watcher-thread*
                        (log-debug "Watcher thread already started")
                        nil)
                       (t (setq *watcher-thread* (bt:current-thread)))))
           (unwind-protect
                (handler-case 
                    (progn
                      (log-info logger "Hierarchy watcher started")
                      (loop
                        (let ((*watcher-event-time* (get-universal-time)))
                          (hierarchy-watcher-once))
                        (sleep *hierarchy-watcher-heartbeat*)))
                  (error (e)
                    (log-error logger "Error in hierarchy watcher thread:~%~A" e)))
             (with-hierarchies-lock
               (setf *watcher-thread* nil))
             (log-info logger "Hierarchy watcher thread ended"))))
       :name "Hierarchy Watcher"
       :initial-bindings
       `((*hierarchy* . 0)
         (*package* . (find-package :log4cl-impl))
         ,@*watcher-thread-bindings*)))))


(defun hierarchy-watcher-do-one-token (hier token)
  (with-slots (name) hier
    (with-log-hierarchy (hier)
      (handler-bind ((serious-condition
                       (lambda (c)
                         (remove-watch-token token :test #'eq)
                         (log-error
                          '(log4cl)
                          "WATCH-TOKEN-CHECK in ~S hierarchy signaled error for token ~S~%~A"
                          name token c)
                         (return-from hierarchy-watcher-do-one-token))))
        (watch-token-check token)))))

(defun hierarchy-watcher-once ()
  "Do one iteration of watcher loop."
  (map nil
       (lambda (hier)
         (dolist (token (slot-value hier 'watch-tokens))
           (hierarchy-watcher-do-one-token hier token)))
       *hierarchies*))

(defun stop-hierarchy-watcher-thread ()
  (when *watcher-thread*
    (bt::destroy-thread *watcher-thread*)))
