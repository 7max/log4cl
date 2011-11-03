(in-package :cl-log.test)

(in-root-suite)
(defsuite* speed)

;; Logging case
;;
;; (time (speed-test-to-file :iterations 10000000))
;;
;; Evaluation took:
;;   26.854 seconds of real time
;;   26.711938 seconds of total run time (26.596956 user, 0.114982 system)
;;   99.47% CPU
;;   64,327,060,507 processor cycles
;;   945,488 bytes consed
;;
;; java test 10000000  8.74s user 15.62s system 101% cpu 24.002 total
;;
;; No logging case (note 10x iterations the logging case)
;;
;; (time (speed-test-to-file :iterations 100000000 :root-logger-level :info))
;;
;; Evaluation took:
;;   1.187 seconds of real time
;;   1.178821 seconds of total run time (1.171822 user, 0.006999 system)
;;   99.33% CPU
;;   2,737,915,158 processor cycles
;;   32,736 bytes consed
;; 
;; java -Droot.level=INFO test 100000000  15.07s user 0.85s system 101% cpu 15.757 total
;;

(deftest speed-test-to-file (&key (filespec "/dev/null")
                                  (external-format :default)
                                  (layout (cl-log::make-simple-layout))
                                  (iterations 1000000)
                                  (root-logger-level :debug))
  (with-package-log-hierarchy
    (with-open-file (stream filespec :direction :output
                                     :if-exists :supersede
                                     :external-format external-format)
      (clear-logging-configuration)
      (add-appender *root-logger* (cl-log:make-stream-appender
                                   :layout layout
                                   :stream stream))
      (setf (logger-log-level *root-logger*) root-logger-level)
      (dotimes (cnt iterations)
        (log-debug :cl-log.test.category "iter=~d" cnt)))))
