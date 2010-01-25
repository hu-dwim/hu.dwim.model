;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (entity e) persistent-log-entry ()
  ((logged-at (transaction-timestamp)
    :type timestamp
    :reference #t
    :primary #t)
   (category
    :type (text 32)
    :reference #t
    :primary #t)
   (level
    :type (member +dribble+ +debug+ +info+ +warn+ +error+ +fatal+)
    :primary #t)
   (content
    :type text
    :primary #t)))

(def association
  ((:slot authenticated-session :type (or null authenticated-session) :primary #t
    :initform (when (has-authenticated-session)
                (current-authenticated-session)))
   (:slot persistent-log-entries :type (set persistent-log-entry))))

;;;;;;
;;; Functional

(def (class e) persistent-appender (appender)
  ())

(def method append-message (logger (appender persistent-appender) level message-control message-arguments)
  (assert (symbolp level))
  ;; TODO there should be some assert that we are not in a read-only/rollback-only transaction
  (make-instance 'persistent-log-entry
                 :category (string-downcase (hu.dwim.logger::name-of logger))
                 :level level
                 :content (if message-arguments
                              (apply 'format-persistent-log-message message-control message-arguments)
                              message-control)))

(def function format-persistent-log-message (message &rest args)
  (cl-l10n:with-locale "en"
    (bind ((processed-args (mapcar (lambda (arg)
                                     (typecase arg
                                       (persistent-object
                                        (bind ((instance arg)
                                               (oid (oid-of instance)))
                                          (assert oid)
                                          (format nil "`p(~A,~A)" oid (localized-instance-name instance))))
                                       (t arg)))
                                   args)))
      (apply #'format nil message processed-args))))

(def (function e) split-persistent-log-message (message)
  (declare (type string message))
  (bind ((last-end 0)
         (result (list)))
    (cl-ppcre:do-scans (start end reg-starts reg-ends "(.*?)`(\\w)\\((\\d+),(.*?)\\)" message)
      (flet ((register (index)
               (subseq message (elt reg-starts index) (elt reg-ends index))))
        (bind ((free-text (register 0))
               (type (bind ((start (elt reg-starts 1))
                            (end (elt reg-ends 1)))
                       (assert (= (1+ start) end))
                       (elt message start)))
               (oid (register 2))
               (textual-representation (register 3)))
          (setf last-end end)
          (eswitch (type :test #'char=)
            (#\p
             (push free-text result)
             (push (cons (parse-integer oid) textual-representation) result))))))
    (push (subseq message last-end) result)
    (nreverse result)))

;; TODO maybe this should be project specific...
(bind ((persistent-appender (make-instance 'persistent-appender)))
  (bind ((audit-logger (find-logger 'audit)))
    (setf (hu.dwim.logger::appenders-of audit-logger) (list (make-instance 'persistent-appender)))
    (setf (log-level audit-logger) +dribble+)))
