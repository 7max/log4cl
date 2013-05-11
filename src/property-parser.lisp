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

(in-package #:log4cl)

;; generic property file parser, can be reused for other stuff

(defclass property-parser ()
  ((name-token-separator :initform ":" :initarg :separator)
   (name-token-read-case :initform (readtable-case *readtable*)
                         :initarg :read-case)
   (%orig-initargs))
  (:documentation "Class for reading Java style property files."))


(defvar %parse-line nil
  "Current line being parsed")
(defvar %parse-line-num nil
  "Current line number in the input stream")

(defclass property-location ()
  ((line :initform %parse-line)
   (line-num :initform %parse-line-num))
  (:documentation "Remembered line and line number in the input
stream where object appeared, so we can signal errors with that
info"))

(defmacro with-property-location ((location) &body body)
  "Run BODY with %PARSE-LINE and %PARSE-LINE-NUM bound to the
remembered location, so that any errors signaled will have correct
location"
  `(with-slots (line-num line)
       ,location
     (let ((%parse-line line)
           (%parse-line-num line-num))
       ,@body)))

(define-condition property-parser-error (log4cl-error parse-error)
  ((condition :initarg :condition :accessor condition-of)
   (line-num :initarg :line-num :accessor line-num-of)
   (line :initarg :line :accessor line-of))
  (:report (lambda (c stream)
             (format stream "\"~A\"~%~
                             Error at line ~D:~%~
                             ~A~%"
                     (line-of c)
                     (line-num-of c)
                     (condition-of c)))))

(defmethod shared-initialize :after ((parser property-parser) slots &rest initargs &key &allow-other-keys)
  (declare (ignore slots))
  (with-slots (name-token-separator name-token-read-case
               line-num %orig-initargs) parser
    (unless (slot-boundp parser '%orig-initargs)
      (setf %orig-initargs initargs))
    (setf name-token-separator (getf %orig-initargs :separator ":")
          name-token-read-case (getf %orig-initargs :read-case
                                     (readtable-case *readtable*)))))

(defgeneric parse-property-stream (parser stream)
  (:documentation "Read stream and for each line that is not a
comment, call PARSE-LINE function."))

(defgeneric parse-property-line (parser name value)
  (:documentation "Called for each NAME=VALUE line in the properties
  stream. Both NAME and VALUE are strings"))

(defgeneric parse-property-tokens (parser tokens value)
  (:documentation "Called by default PARSE-PROPERTY-LINE
method. TOKENS will be the NAME part of the NAME=VALUE line, split
according to NAME-TOKEN-SEPARATOR and their case adjusted by
NAME-TOKEN-READ-CASE"))

(defgeneric parse-property-keyword (parser keyword more-tokens value)
  (:documentation "Called to handle properties that start with a
common prefix.  KEYWORD will be the 1st token of the property name,
interned as a keyword. MORE-TOKENS are the rest of the name tokens as strings

For example for a the properties stream line:
    \"log4cl:foo:bar=baz\"
this function will be called with the arguments
     (:LOG4CL '(\"FOO\" \"BAR\") \"BAZ\"value)
"))

(defmethod parse-property-stream ((parser property-parser) stream)
  ;; so that we can re-parse with the same parser
  (reinitialize-instance parser)
  (let (pos start name (%parse-line-num 0) (%parse-line nil))
    (flet ((space-or-equal-p (c)
             (or (char= c #\Space)
                 (char= c #\Tab)
                 (char= c #\=))))
      (tagbody  
         ;; bite my shiny metal ass Dijkstra
       :next-line 
         (unless (setq %parse-line (read-line stream nil)) (go :exit))
         (incf %parse-line-num)
         (setq start (position-if-not #'space-or-equal-p %parse-line))
         (when (or (not start) (char= (char %parse-line start) #\#))
           (go :next-line))
         (setq pos (position-if #'space-or-equal-p %parse-line :start start))
         (setq name (substr %parse-line start pos))
         (unless (plusp (length name))
           (log4cl-error "Property name can't be empty"))
         (unless (and pos (setq pos (position #\= %parse-line :start pos)))
           (log4cl-error "Expecting '=' after property '~a'" name))
         ;; note we don't strip whitespace from beginning of value, since
         ;; string properties like pattern layout's :conversion-pattern
         ;; may start with literal whitespace
         (parse-property-line parser name (substr %parse-line (1+ pos)))
         (go :next-line)
       :exit))))

(defun convert-read-case (string case)
  "Convert STRING according to CASE"
  (declare (type string string)
           (type (member nil :upcase :downcase :invert :preserve) case))
  (with-output-to-string (s)
    (if (not case)
        (princ string s)
        (write-string-modify-case (coerce string 'simple-string)
                                  s case))))


(defmethod parse-property-line ((parser property-parser) name value)
  "Handles two special cases of SEPARATOR=VALUE and
READ-CASE=VALUE (ignoring case differences), and updates the parser
accordingly, otherwise splits NAME with current separator, converts
tokens according to read case, and forwards to PARSE-PROPERTY-TOKENS"
  (with-slots (name-token-separator name-token-read-case) parser
    (log-sexp name value)
    (let ((name-tokens (split-string name name-token-separator)))
      (cond ((equalp "separator" (first name-tokens))
             (unless (null (cdr name-tokens))
               (log4cl-error "Separator can't have any sub-properties"))
             (setf name-token-separator (strip-whitespace value))
             (unless (plusp (length name-token-separator))
               (log4cl-error "Separator can't be empty"))
             (log-sexp "changed separator" name-token-separator))
            ((equalp "read-case" (first name-tokens))
             (unless (null (cdr name-tokens))
               (log4cl-error "Read-case can't have any sub-properties"))
             (setq name-token-read-case
                   (let ((read-case (strip-whitespace value)))
                     (cond ((equalp read-case ":upcase")
                            :upcase)
                           ((equalp read-case ":downcase")
                            :downcase)
                           ((equalp read-case ":invert")
                            :invert)
                           ((equalp read-case ":preserve")
                            :preserve)
                           ((or (equalp "" read-case)
                                (equalp "nil" read-case))
                            nil)
                           (t (log4cl-error "Invalid read case ~s" read-case)))))
             (log-sexp "changed read-case" name-token-read-case))
            (t
             (parse-property-tokens
              parser
              (mapcar (lambda (token)
                        (convert-read-case token name-token-read-case))
                      name-tokens)
              value))))))

(defmethod parse-property-tokens ((parser property-parser) tokens value)
  "Interns the first element of TOKENS as a keyword, and forwards to
  PARSE-PROPERTY-KEYWORD"
  (let ((keyword (intern (pop tokens) :keyword)))
    (log-sexp keyword tokens value)
    (parse-property-keyword parser keyword tokens value)))


(defmethod parse-property-stream :around ((parser property-parser) stream)
  "Wrap errors with line number"
  (declare (ignore stream))
  (handler-bind
      ((serious-condition (lambda (c)
                            (error 'property-parser-error
                                   :line %parse-line
                                   :line-num %parse-line-num
                                   :condition c))))
    (call-next-method)))

