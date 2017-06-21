;;
;;  re  -  Simple syntax for regular expressions in Common Lisp
;;
;;  Copyright 2014 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cl-user)

(defpackage #:re
  (:nicknames #:cl-re)
  (:use :cl)
  (:export
   #:disable-re-syntax
   #:do-re-groups
   #:do-re-matches
   #:enable-re-syntax
   #:in-re-readtable
   #:re
   #:re-bind
   #:re-match
   #:re-matches
   #:re-readtable
   #:re-split
   #:re-subst
   #:re-subst*
   #:re-quote))

(in-package #:re)

;;  CL-PPCRE sugar. All exported symbols are prefixed with RE-
;;  so (USE-PACKAGE #:RE) makes sense.

(defun re (re &key
                case-insensitive-mode
                multi-line-mode
                single-line-mode
                extended-mode
                destructive)
  (cl-ppcre:create-scanner re
                           :case-insensitive-mode case-insensitive-mode
                           :multi-line-mode multi-line-mode
                           :single-line-mode single-line-mode
                           :extended-mode extended-mode
                           :destructive destructive))

(defun re-match (re string &key
                             (start 0)
                             (end (length string))
                             real-start-pos)
  (cl-ppcre:scan re string
                 :start start
                 :end end
                 :real-start-pos real-start-pos))

(defun re-matches (re string &key
                             (start 0)
                             (end (length string))
                             sharedp)
  (cl-ppcre:all-matches-as-strings re string
                 :start start
                 :end end
                 :sharedp sharedp))

(defmacro do-re-matches (re (var) string &body body)
  (let ((n (gensym "N-")))
    `(let ((,n 0))
       (cl-ppcre:do-matches-as-strings (,var ,re ,string (when (< 0 ,n)
                                                           ,n))
         (incf ,n)
         ,@body))))

(defmacro re-bind (re var-list string &body body)
  `(cl-ppcre:register-groups-bind ,var-list (,re ,string)
     ,@(or body `((values ,@var-list)))))

(defmacro do-re-groups (re var-list string &body body)
  `(cl-ppcre:do-register-groups ,var-list (,re ,string)
     ,@body))

(defun re-split (re string &key
                             (start 0)
                             (end (length string))
                             limit
                             registers
                             unmatched
                             shared)
  (cl-ppcre:split re string
                  :start start
                  :end end
                  :limit limit
                  :with-registers-p registers
                  :omit-unmatched-p (not unmatched)
                  :sharedp shared))

(defun re-subst (re subst string &key
                                   (start 0)
                                   (end (length string))
                                   preserve-case
                                   (element-type 'character))
  (cl-ppcre:regex-replace-all re string subst
                              :start start
                              :end end
                              :preserve-case preserve-case
                              :element-type element-type))

(defun re-subst* (string &rest re-subst*)
  (if (endp re-subst*)
      string
      (destructuring-bind (re subst &rest rest) re-subst*
        (apply #'re-subst* (re-subst re subst string) rest))))

;;  Quoting

(defun re-quote (string &key (start 0) (end (length string)))
  (cl-ppcre:quote-meta-chars string :start start :end end))

;;  Reader syntax

(defun re-mode-match (re)
  `(lambda (str)
     (re-match ,re str)))

(defun re-mode-subst (re subst)
  `(lambda (str)
     (re-subst ,re ,subst str)))

(defvar *re-modes* '((nil identity 1)
                     (#\m re-mode-match 1)
                     (#\s re-mode-subst 2)))

(defun read-delimited-parts (count delimiter stream)
  "Reads COUNT strings separated by a DELIMITER character, from STREAM.
The delimiter and '\' can be escaped with '\'.
Other characters will not be escaped and '\' will be part of the string."
  (loop repeat count
        collect (with-output-to-string (out)
                  (loop for c = (read-char stream)
                        until (char= c delimiter)
                     do (write-char (if (and (char= #\\ c)
                                             (let ((p (peek-char nil stream)))
                                               (or (char= delimiter p)
                                                   (char= #\\ p))))
                                        (read-char stream)
                                        c)
                                    out)))))

(defun read-re (stream &optional char n)
  (declare (ignore char n))
  (let* ((char (read-char stream))
         (mode (assoc char *re-modes*))
         (delimiter (if mode (read-char stream) char)))
    (destructuring-bind (fun count) (cdr (or mode (assoc nil *re-modes*)))
      (apply fun (read-delimited-parts count delimiter stream)))))

(defvar *previous-readtables* nil)

(defun re-readtable (&key merge (char #\#) (subchar #\~))
  (let ((rt (copy-readtable merge)))
    (if subchar
        (set-dispatch-macro-character char subchar 'read-re rt)
        (set-macro-character char 'read-re nil rt))
    rt))

(defmacro in-re-readtable (&key merge (char #\#) (subchar #\~))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* (re-readtable :merge ,merge
                                     :char ,char
                                     :subchar ,subchar))))

(defmacro enable-re-syntax (&optional (char #\#) (subchar #\~))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (re-readtable :char ,char :subchar ,subchar))))

(defmacro disable-re-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (when *previous-readtables*
      (setq *readtable* (pop *previous-readtables*)))))
