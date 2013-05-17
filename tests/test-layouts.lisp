;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2012, Max Mikhanosha. All rights reserved.
;;;
;;; This file is licensed to You under the Apache License, Version 2.0
;;; (the "License"); you may not use this file except in compliance
;;; with the License.  You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(log4cl-test:defsubsuite :log4cl-test.layouts)
(in-package :log4cl-test.layouts)
(log4cl-test:subsuite-start)


(deftest test-pattern-parsing ()
  "Test parsing of pattern layout patters and that it signals
pattern-layout-error when parsing errors occur"
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%-"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "\\"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%-3"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%-3."))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%-3.p"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%.p"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%.p"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "%.10p"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%c{3"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "%c{3}"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "%c{3\\3}{}"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "%c{3\\3}{}{:invert}"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%c{3\\3}{}{:blah}"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%c{3\\}"))
  (signals pattern-layout-error
    (make-instance 'pattern-layout :conversion-pattern "%c{foo\\}bar}"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "lala%%lala"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "|%p %c %%|"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "\\\\"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "\\t"))
  (finishes 
    (make-instance 'pattern-layout :conversion-pattern "\\n")))

(deftest test-basic-patterns ()
  "Test basic pattern-layout patterns"
  (test-pattern-layout "%m" "message")
  (test-pattern-layout "|%m|" "|message|")
  (test-pattern-layout "%p - %m" "INFO - message")
  (test-pattern-layout "%-5p - %m"  " INFO - message")
  (test-pattern-layout "%5.2p - %m" "FO    - message")
  (test-pattern-layout "%.2p - %m" "FO - message"))

(deftest test-pattern-backslash ()
  "Test that backslash is handled correctly in the patterns"
  (test-pattern-layout "%p\\t%m" "INFO	message")
  (test-pattern-layout "%p\\n%m" "INFO
message")
  (test-pattern-layout "%p \\- %m" "INFO - message")
  (test-pattern-layout "%p \\\\-\\\\ %m" "INFO \\-\\ message"))

(deftest test-pattern-layout-optional ()
  (with-ndc () 
    (test-pattern-layout "%x" "")
    (test-pattern-layout "<%x>" "<>")
    (test-pattern-layout "%3x" "   ")
    (test-pattern-layout "<%3x>" "<   >")
    (test-pattern-layout "%:3x" "")
    (test-pattern-layout "%:-3x" "")
    (with-ndc (nil)
      (test-pattern-layout "%x" (format nil "~A" nil))
      (test-pattern-layout "%:x" (format nil "~A" nil))
      (test-pattern-layout "%:4x" (format nil "~A " nil))
      (test-pattern-layout "%:-4x" (format nil " ~A" nil)))
    (with-ndc ("A")
      (test-pattern-layout "%x" "A")
      (test-pattern-layout "%:x" "A")
      (test-pattern-layout "%:3x" "A  ")
      (test-pattern-layout "%:-3x" "  A"))
    (with-ndc () 
      (test-pattern-layout "%;<;;>;x" "<>")
      (test-pattern-layout "%;<;;>;3x" "<>   ") 
      (test-pattern-layout "%;<;;>;-3x" "   <>"))
    (with-ndc ("A")
      (test-pattern-layout "%;<;;>;x" "<A>")
      (test-pattern-layout "%;<;;>;3x" "<A>  ")
      (test-pattern-layout "%;<;;>;-3x" "  <A>")
      (test-pattern-layout "%;<;3x" "<A  ")
      (test-pattern-layout "%;;;>;-3x" "  A>"))
    (with-ndc ()
      (test-pattern-layout "%:;<;;>;x" "")
      (test-pattern-layout "%:;<;;>;3x" "")
      (test-pattern-layout "%:;<;;>;-3x" "")
      (test-pattern-layout "%:;<;3x" "")
      (test-pattern-layout "%:;;;>;-3x" ""))
    (with-ndc ("A")
      (test-pattern-layout "%:;-=<;;>;x" "-=<A>")
      (test-pattern-layout "%:;<;;>;3x" "<A>  ")
      (test-pattern-layout "%:;<;;>;-3x" "  <A>")
      (test-pattern-layout "%:;<;3x" "<A  ")
      (test-pattern-layout "%:;;;>;-3x" "  A>"))))

(deftest test-pattern-log-level ()
  "Test %p pattern"
  (loop for level from +log-level-fatal+ to +log-level-debu9+
        do (test-pattern-layout "%p"
                                (log-level-to-string level)
                                :level level)))

(defun make-expected  (list separator)
  (with-output-to-string (*standard-output*)
    (let (sep)
      (dolist (s list)
        (if sep (princ sep)
            (setq sep separator))
        (princ (if (stringp s) s (symbol-name s)))))))

(deftest test-make-expected ()
  "Test function for making expected results"
  ;; test the utility-function for making expected logger name
  (is (make-expected '(|ONE| |TWO| |THREE|) ":")
      "ONE:TWO:THREE")
  (is (make-expected '(|one| |two| |three|) ":")
      "one:two:three")
  (is (make-expected '(|One| |Two| |Three|) ":")
      "One:Two:Three"))

(deftest test-pattern-category-1 ()
  "Test %c pattern with regards to precision, category separator and
case conversion"
  (let ((categories '(one two three))
        (logger (make-logger '(one two three))))
    (labels
        ((test-category (pattern &key
                                 (separator ".")
                                 (categories categories)
                                 (logger logger))
           (let ((expected
                   (make-expected categories separator)))
             (test-pattern-layout pattern
                                  expected
                                  :logger logger))))
      ;; test using system native case
      (test-category "%c{4}")
      (test-category "%c{3}")
      (test-category "%c{2}" :separator "." :categories '(two three))
      (test-category "%c{1}" :separator "." :categories '(three))
      (test-category "%c{4}{_}" :separator "_")
      (test-category "%c{3}{_}" :separator "_")
      (test-category "%c{2}{_}" :separator "_" :categories '(two three))
      (test-category "%c{1}{_}" :separator "_" :categories '(three))
      ;; create loggers named with upper, lower and mixed cases
      ;; and test various combination of case conversion on them
      (let ((logger-mixed (make-logger '(|One| |Two| |Three|)))
            (cats-mixed '(|One| |Two| |Three|))
            (logger-upcase (make-logger '(|ONE| |TWO| |THREE|)))
            (cats-upcase '(|ONE| |TWO| |THREE|))
            (logger-downcase (make-logger '(|one| |two| |three|)))
            (cats-downcase '(|one| |two| |three|)))
        ;; test unchanged mixed case
        (test-category "%c{4}" :logger logger-mixed :categories cats-mixed)
        (test-category "%c{3}" :logger logger-mixed :categories cats-mixed)
        (test-category "%c{2}" :separator "." :logger logger-mixed
                               :categories (last cats-mixed 2))
        (test-category "%c{1}" :separator "." :logger logger-mixed
                               :categories (last cats-mixed 1))
        (test-category "%c{4}{_}" :separator "_"
                                  :logger logger-mixed :categories cats-mixed)
        (test-category "%c{}{_}" :separator "_"
                                  :logger logger-mixed :categories cats-mixed)
        (test-category "%c{2}{_}" :separator "_"
                                  :logger logger-mixed :categories
                                  (last cats-mixed 2))
        (test-category "%c{1}{_}" :separator "_"
                                  :logger logger-mixed :categories
                                  (last cats-mixed 1))
        ;; test invert does not change the case
        (test-category "%c{4}{}{:invert}" :logger logger-mixed :categories cats-mixed)
        (test-category "%c{3}{}{:invert}" :logger logger-mixed :categories cats-mixed)
        (test-category "%c{2}{}{:invert}" :separator "." :logger logger-mixed
                                          :categories (last cats-mixed 2))
        (test-category "%c{1}{}{:invert}" :separator "." :logger logger-mixed
                                          :categories (last cats-mixed 1))
        (test-category "%c{4}{_}{:invert}" :separator "_"
                                           :logger logger-mixed
                                           :categories cats-mixed)
        (test-category "%c{}{_}{:invert}" :separator "_"
                                          :logger logger-mixed
                                          :categories cats-mixed)
        (test-category "%c{2}{_}{:invert}" :separator "_"
                                           :logger logger-mixed :categories
                                           (last cats-mixed 2))
        (test-category "%c{1}{_}{:invert}" :separator "_"
                                           :logger logger-mixed :categories
                                           (last cats-mixed 1))
        ;; test forced upper case
        (test-category "%c{4}{.}{:upcase}"
                       :logger logger-mixed
                       :categories cats-upcase)
        (test-category "%c{}{}{:upcase}"
                       :logger logger-mixed
                       :categories cats-upcase)
        (test-category "%c{2}{}{:upcase}"
                       :separator "."
                       :logger logger-mixed
                       :categories (last cats-upcase 2))
        (test-category "%c{1}{}{:upcase}"
                       :separator "."
                       :logger logger-mixed
                       :categories (last cats-upcase 1))
        (test-category "%c{4}{_}{:upcase}"
                       :separator "_"
                       :logger logger-mixed
                       :categories cats-upcase)
        (test-category "%c{}{_}{:upcase}"
                       :separator "_"
                       :logger logger-mixed :categories cats-upcase)
        (test-category "%c{2}{_}{:upcase}"
                       :separator "_"
                       :logger logger-mixed :categories
                       (last cats-upcase 2))
        (test-category "%c{1}{_}{:upcase}"
                       :separator "_"
                       :logger logger-mixed :categories
                       (last cats-upcase 1))
        ;; test forced lower case
        (test-category "%c{4}{.}{:downcase}"
                       :logger logger-mixed
                       :categories cats-downcase)
        (test-category "%c{}{}{:downcase}"
                       :logger logger-mixed
                       :categories cats-downcase)
        (test-category "%c{2}{}{:downcase}"
                       :separator "."
                       :logger logger-mixed
                       :categories (last cats-downcase 2))
        (test-category "%c{1}{}{:downcase}"
                       :separator "."
                       :logger logger-mixed
                       :categories (last cats-downcase 1))
        (test-category "%c{4}{_}{:downcase}"
                       :separator "_"
                       :logger logger-mixed
                       :categories cats-downcase)
        (test-category "%c{}{_}{:downcase}"
                       :separator "_"
                       :logger logger-mixed :categories cats-downcase)
        (test-category "%c{2}{_}{:downcase}"
                       :separator "_"
                       :logger logger-mixed :categories
                       (last cats-downcase 2))
        (test-category "%c{1}{_}{:downcase}"
                       :separator "_"
                       :logger logger-mixed :categories
                       (last cats-downcase 1))
        ;; test that invert works on lower-case named logger
        (test-category "%c{4}{.}{:invert}"
                       :logger logger-downcase
                       :categories cats-upcase)
        (test-category "%c{}{}{:invert}"
                       :logger logger-downcase
                       :categories cats-upcase)
        (test-category "%c{2}{}{:invert}"
                       :separator "."
                       :logger logger-downcase
                       :categories (last cats-upcase 2))
        (test-category "%c{1}{}{:invert}"
                       :separator "."
                       :logger logger-downcase
                       :categories (last cats-upcase 1))
        (test-category "%c{4}{_}{:invert}"
                       :separator "_"
                       :logger logger-downcase
                       :categories cats-upcase)
        (test-category "%c{}{_}{:invert}"
                       :separator "_"
                       :logger logger-downcase :categories cats-upcase)
        (test-category "%c{2}{_}{:invert}"
                       :separator "_"
                       :logger logger-downcase :categories
                       (last cats-upcase 2))
        (test-category "%c{1}{_}{:invert}"
                       :separator "_"
                       :logger logger-downcase :categories
                       (last cats-upcase 1))
        ;; test that invert works on upper-case named logger
        (test-category "%c{4}{.}{:invert}"
                       :logger logger-upcase
                       :categories cats-downcase)
        (test-category "%c{}{}{:invert}"
                       :logger logger-upcase
                       :categories cats-downcase)
        (test-category "%c{2}{}{:invert}"
                       :separator "."
                       :logger logger-upcase
                       :categories (last cats-downcase 2))
        (test-category "%c{1}{}{:invert}"
                       :separator "."
                       :logger logger-upcase
                       :categories (last cats-downcase 1))
        (test-category "%c{4}{_}{:invert}"
                       :separator "_"
                       :logger logger-upcase
                       :categories cats-downcase)
        (test-category "%c{}{_}{:invert}"
                       :separator "_"
                       :logger logger-upcase :categories cats-downcase)
        (test-category "%c{2}{_}{:invert}"
                       :separator "_"
                       :logger logger-upcase :categories
                       (last cats-downcase 2))
        (test-category "%c{1}{_}{:invert}"
                       :separator "_"
                       :logger logger-upcase :categories
                       (last cats-downcase 1))))))


(deftest test-pattern-category-2 ()
  "Test alternate category separator, and that field min/max width
works correctly with it"
  (let ((logger (make-logger '(one two three))))
    (test-pattern-layout "%c{}{---}"
                         (make-expected '(one two three) "---")
                         :logger logger)
    (test-pattern-layout "%c{2}{---}"
                         (make-expected '(two three) "---")
                         :logger logger)
    (test-pattern-layout "%c{1}{---}"
                         (make-expected '(three) "---")
                         :logger logger)
    ;; 0134567890123456789
    ;; one---two---three
    (let ((expected (make-expected '(one two three) "---")))
      (loop for i from (length expected)
            downto 1
            do (test-pattern-layout
                (format nil "%.~dc{}{---}" i)
                (subseq expected
                        (- (length expected) i))
                :logger logger)
            do (test-pattern-layout
                (format nil "%~d.~dc{}{---}" (+ i 5) i)
                (concatenate 'string
                             (subseq expected
                                     (- (length expected) i))
                             "     ")
                :logger logger)
            do (test-pattern-layout
                (format nil "%-~d.~dc{}{---}" (+ i 5) i)
                (concatenate 'string
                             "     "
                             (subseq expected
                                     (- (length expected) i)))
                :logger logger)))))


(deftest test-pattern-category-3 ()
  (test-pattern-layout "%-5c{1}" (make-expected '(three) "."))
  (test-pattern-layout "%-5c{1}{:}" (make-expected '(three) ":")))

(deftest test-pattern-category-4 ()
  (test-pattern-layout "%-12c{2}" (concatenate 'string
                                               "   "
                                               (make-expected '(two three) ".")))
  (test-pattern-layout "%-12c{2}{:}" (concatenate 'string
                                                  "   "
                                                  (make-expected '(two three) ":")))
  (test-pattern-layout "%-12c{2}{--}" (concatenate 'string
                                                  "  "
                                                  (make-expected '(two three) "--"))))

(deftest test-pattern-category-extended-precision ()
  "Test the category precision in the form of %c{<from>,<count>}"
  (test-pattern-layout "%c{0,0}" (make-expected '(one two three) "."))
  (test-pattern-layout "%c{0,20}" (make-expected '(one two three) "."))
  (test-pattern-layout "%c{3,20}" "")
  (test-pattern-layout "%c{3,-1}" "")
  (test-pattern-layout "%c{2,1}" (make-expected '(three) "."))
  (test-pattern-layout "%c{1,1}" (make-expected '(two) "."))
  (test-pattern-layout "%c{0,2}" (make-expected '(one two) "."))
  (test-pattern-layout "%c{1,2}" (make-expected '(two three) "."))

  (test-pattern-layout "%c{0,0}{----}" (make-expected '(one two three) "----"))
  (test-pattern-layout "%c{0,20}{----}" (make-expected '(one two three) "----"))
  (test-pattern-layout "%c{3,20}{----}" "")
  (test-pattern-layout "%c{3,-1}{----}" "")
  (test-pattern-layout "%c{2,1}{----}" (make-expected '(three) "----"))
  (test-pattern-layout "%c{1,1}{----}" (make-expected '(two) "----"))
  (test-pattern-layout "%c{0,2}{----}" (make-expected '(one two) "----"))
  (test-pattern-layout "%c{1,2}{----}" (make-expected '(two three) "----"))
  ;; 0134567890123456789
  ;; one---two---three
  (let ((expected (make-expected '(two three four) "----"))
        (logger (make-logger '(one two three four five))))
    (loop for i from (length expected)
          downto 1
          do (test-pattern-layout
              (format nil "%.~dc{1,3}{----}" i)
              (subseq expected
                      (- (length expected) i))
              :logger logger)
          do (test-pattern-layout
              (format nil "%~d.~dc{1,3}{----}" (+ i 5) i)
              (concatenate 'string
                           (subseq expected
                                   (- (length expected) i))
                           "     ")
              :logger logger)
          do (test-pattern-layout
              (format nil "%-~d.~dc{1,3}{----}" (+ i 5) i)
              (concatenate 'string
                           "     "
                           (subseq expected
                                   (- (length expected) i)))
              :logger logger))))

(deftest test-pattern-date-1 ()
  "Test %d pattern. This test may fail if second changes doing the 1st
two asserts "
  (let* ((ut-utc (encode-universal-time 1 2 3 21 02 2012 0))
         (ut-loc (encode-universal-time 1 2 3 21 02 2012))
         (tz (nth-value 8 (decode-universal-time ut-loc))))
    ;; default one
    (test-pattern-layout
     (format nil "%d{}{~d}" ut-utc)
     "2012-02-21 03:02:01")
    (test-pattern-layout
     (format nil "-- %d{[%m/%d/%Y %H:%M:%S]}{~d} --" ut-utc)
     "-- [02/21/2012 03:02:01] --")
    (test-pattern-layout
     (format nil "-%%- %d{[%m/%d/%Y %H:%M:%S]}{~d} -%%-" ut-utc)
     "-%- [02/21/2012 03:02:01] -%-");;
    ;; Test extended formatting flags
    ;; 
    ;; _(underscore) pad with spaces
    ;; -(dash) don't pad
    ;; ^ convert to upper case
    ;;
    ;; followed by width, which pads field from the left if its
    ;; smaller
    ;; 
    (loop for d-str in '("d" "D")
          for ut in (list ut-utc ut-loc)
          do (progn
               (test-pattern-layout (format nil "%~A{%Y}{~d}" d-str ut) "2012")
               (test-pattern-layout (format nil "%~A{%y}{~d}" d-str ut) "12")
               (test-pattern-layout (format nil "%~A{%m}{~d}" d-str ut) "02")
               (test-pattern-layout (format nil "%~A{%_m}{~d}" d-str ut) " 2")
               (test-pattern-layout (format nil "%~A{%-m}{~d}" d-str ut) "2")
               (test-pattern-layout (format nil "%~A{%_d}{~d}" d-str ut) "21")
               (test-pattern-layout (format nil "%~A{%-d}{~d}" d-str ut) "21")
               (test-pattern-layout (format nil "%~A{%a}{~d}" d-str ut) "Tue")
               (test-pattern-layout (format nil "%~A{%^a}{~d}" d-str ut) "TUE")
               (test-pattern-layout (format nil "%~A{%A}{~d}" d-str ut) "Tuesday")
               (test-pattern-layout (format nil "%~A{%^A}{~d}" d-str ut) "TUESDAY")
               (test-pattern-layout (format nil "%~A{%b}{~d}" d-str ut) "Feb")
               (test-pattern-layout (format nil "%~A{%^b}{~d}" d-str ut) "FEB")
               (test-pattern-layout (format nil "%~A{%B}{~d}" d-str ut) "February")
               ;; Now test the width
               (test-pattern-layout (format nil "%~A{%4Y}{~d}" d-str ut) "2012")
               (test-pattern-layout (format nil "%~A{%6Y}{~d}" d-str ut) "  2012")
               (test-pattern-layout (format nil "%~A{%4y}{~d}" d-str ut) "  12")
               (test-pattern-layout (format nil "%~A{%6y}{~d}" d-str ut) "    12")
               (test-pattern-layout (format nil "%~A{%2m}{~d}" d-str ut) "02")
               (test-pattern-layout (format nil "%~A{%4m}{~d}" d-str ut) "  02")
               (test-pattern-layout (format nil "%~A{%_3m}{~d}" d-str ut) "  2")
               (test-pattern-layout (format nil "%~A{%-2m}{~d}" d-str ut) " 2")
               (test-pattern-layout (format nil "%~A{%-3m}{~d}" d-str ut) "  2")
               (test-pattern-layout (format nil "%~A{%_3d}{~d}" d-str ut) " 21")
               (test-pattern-layout (format nil "%~A{%5a}{~d}" d-str ut) "  Tue")
               (test-pattern-layout (format nil "%~A{%^5a}{~d}" d-str ut) "  TUE")
               (test-pattern-layout (format nil "|%~A{%10A}{~d}|" d-str ut) "|   Tuesday|")
               (test-pattern-layout (format nil "%~A{%^9A}{~d}" d-str ut) "  TUESDAY")))
    ;; test am-pm
    (test-pattern-layout (format nil "%d{%I %p}{~d}"
                                 (encode-universal-time 0 0 12 21 02 2012 0))
                         "12 PM")
    (test-pattern-layout (format nil "%d{%I %P}{~d}"
                                 (encode-universal-time 0 0 12 21 02 2012 0))
                         "12 pm")
    (test-pattern-layout (format nil "%d{%I %p}{~d}"
                                 (encode-universal-time 0 0 0 21 02 2012 0))
                         "12 AM")
    (test-pattern-layout (format nil "%d{%I:%M:%S %p}{~d}"
                                 (encode-universal-time 1 59 11 21 02 2012 0))
                         "11:59:01 AM")
    (test-pattern-layout (format nil "%d{%I:%M:%S %p}{~d}"
                                 (encode-universal-time 1 59 22 21 02 2012 0))
                         "10:59:01 PM")
    (test-pattern-layout
     (format nil "%d{%c}{~d}" ut-utc) "Tue 21 Feb 2012 03:02:01 +0000")
    ;; same in local
    (let ((expected-default-format-output
            ;; Below screwing around is needed because CL returns
            ;; timezone offset as rational number, so timezone offset
            ;; of +5:30 will be represented as 11/2 which we need to
            ;; format as +0530
            (multiple-value-bind (tz-hours tz-mins)
                (floor tz)
              (format nil "Tue 21 Feb 2012 03:02:01 ~:[+~;-~]~2,'0d~2,'0d"
                      (plusp tz-hours) (abs tz-hours) 
                      (round (* 60 (coerce tz-mins 'float)))))))
      (test-pattern-layout
       (format nil "%D{%c}{~d}" ut-loc)
       expected-default-format-output))))


(deftest test-pattern-hostname ()
  #+(and sbcl unix) (test-pattern-layout "%h" (sb-unix:unix-gethostname))
  #+(and sbcl win32) (test-pattern-layout "%h" (sb-win32::get-computer-name)))

(deftest test-pattern-newline ()
  (test-pattern-layout "%m%n" (format nil "message~%")))

(deftest test-pattern-thread-name ()
  (test-pattern-layout
   "thread is %t"
   (concatenate 'string "thread is "
                (bordeaux-threads:thread-name
                 (bordeaux-threads:current-thread)))))

(deftest test-pattern-ndc-context ()
  (test-pattern-layout "%x" "")
  (with-ndc ("blah")
    (test-pattern-layout "%x" "blah"))
  (with-ndc (:foo)
    (test-pattern-layout "[%-5x]" (format nil "[ ~s]" :foo))))

(deftest test-pattern-process-id ()
  (flet ((pid ()
           (or #+sbcl (sb-posix:getpid)
               #+clisp (system::process-id)
               0)))
    (test-pattern-layout "%i" (format nil "~d" (pid)))
    (test-pattern-layout "%20i" (format nil "~20<~d~;~>" (pid)))
    (test-pattern-layout "%-20i" (format nil "~20<~d~>" (pid)))))

(deftest test-pattern-log-indent ()
  (with-log-indent (0)
    (with-log-indent ()
      (test-pattern-layout "%I%p - %m" "  INFO - message")
      (with-log-indent ()
        (test-pattern-layout "%I{>}%p - %m" ">>INFO - message")))))

(deftest test-pattern-date-timezone ()
  "Test that date pattern %z timezone changes when DST is effect"
  (let* ((ut-winter (encode-universal-time 1 2 3 15 01 2012))
         (ut-summer (encode-universal-time 1 2 3 15 6 2012))
         (had-dst-in-summer-p (nth-value 7 (decode-universal-time ut-summer)))
         (winter-tz (nth-value 8 (decode-universal-time ut-winter)))
         (summer-tz (nth-value 8 (decode-universal-time ut-summer))))
    ;; CL decode-universal-time returns timezone without taking TZ
    ;; into consideration so just sanity check
    (is (equal winter-tz summer-tz))
    (if had-dst-in-summer-p
        ;; If dst is in effect in the summer, test that %z is
        ;; different from winter one
        (is (not (equal (with-output-to-string (s)
                            (log4cl::format-time s "%z" ut-winter nil))
                        (with-output-to-string (s)
                            (log4cl::format-time s "%z" ut-summer nil)))))
        ;; Otherwise should be same
        (is (equal (with-output-to-string (s)
                       (log4cl::format-time s "%z" ut-winter nil))
                   (with-output-to-string (s)
                       (log4cl::format-time s "%z" ut-summer nil)))))))

(deftest test-pattern-layout-ndc-nonstring ()
  "Unit test for bug where numbers were not printed correctly as NDC"
  (with-ndc () 
    (test-pattern-layout "%x" "")
    (test-pattern-layout "<%x>" "<>")
    (test-pattern-layout "%3x" "   ")
    (test-pattern-layout "<%3x>" "<   >")
    (test-pattern-layout "%:3x" "")
    (test-pattern-layout "%:-3x" "")
    (with-ndc (99)
      (test-pattern-layout "%x" "99")
      (test-pattern-layout "%:x" "99")
      (test-pattern-layout "%:3x" "99 ")
      (test-pattern-layout "%:-3x" " 99"))
    (with-ndc () 
      (test-pattern-layout "%;<;;>;x" "<>")
      (test-pattern-layout "%;<;;>;3x" "<>   ") 
      (test-pattern-layout "%;<;;>;-3x" "   <>"))
    (with-ndc (99)
      (test-pattern-layout "%;<;;>;x" "<99>")
      (test-pattern-layout "%;<;;>;3x" "<99> ")
      (test-pattern-layout "%;<;;>;-3x" " <99>")
      (test-pattern-layout "%;<;3x" "<99 ")
      (test-pattern-layout "%;;;>;-3x" " 99>"))
    (with-ndc ()
      (test-pattern-layout "%:;<;;>;x" "")
      (test-pattern-layout "%:;<;;>;3x" "")
      (test-pattern-layout "%:;<;;>;-3x" "")
      (test-pattern-layout "%:;<;3x" "")
      (test-pattern-layout "%:;;;>;-3x" ""))
    (with-ndc (99)
      (test-pattern-layout "%:;-=<;;>;x" "-=<99>")
      (test-pattern-layout "%:;<;;>;3x" "<99> ")
      (test-pattern-layout "%:;<;;>;-3x" " <99>")
      (test-pattern-layout "%:;<;3x" "<99 ")
      (test-pattern-layout "%:;;;>;-3x" " 99>"))))
