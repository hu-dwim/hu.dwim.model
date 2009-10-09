;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def entity persistent-log-entry ()
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
  ((:type (or null authenticated-session)
    :initform (when (has-authenticated-session)
                (current-authenticated-session))
    :primary #t)
   (:type (set persistent-log-entry))))

;;;;;;
;;; Localization

(def localization en
  (class-name.persistent-log-entry "persistent log entry")

  (slot-name.logged-at "logged at")
  (slot-name.category "category")
  (slot-name.level "level")

  (level.+dribble+ "dribble")
  (level.+debug+ "debug")
  (level.+info+ "information")
  (level.+warn+ "warning")
  (level.+error+ "error")
  (level.+fatal+ "fatal error"))

(def localization hu
  (class-name.persistent-log-entry "napló bejegyzés")

  (slot-name.logged-at "bejegyezés időpontja")
  (slot-name.category "categória")
  (slot-name.level "szint")

  (level.+dribble+ "sustorgás")
  (level.+debug+ "részletes")
  (level.+info+ "információ")
  (level.+warn+ "figyelmeztetés")
  (level.+error+ "hiba")
  (level.+fatal+ "fatális hiba"))

;;;;;;
;;; Functional

(def (class e) persistent-appender (appender)
  ())

(def method append-message (category (appender persistent-appender) message level)
  (assert (symbolp level))
  ;; TODO there should be some assert that we are not in a read-only/rollback-only transaction
  (make-instance 'persistent-log-entry
                 :category (string-downcase (hu.dwim.logger::name-of category))
                 :level level
                 :content (if (consp message)
                              (apply 'format-persistent-log-message message)
                              message)))

(def function format-persistent-log-message (message &rest args)
  (cl-l10n:with-locale "en"
    (bind ((processed-args (mapcar (lambda (arg)
                                     (typecase arg
                                       (persistent-object
                                        (bind ((instance arg)
                                               (oid (oid-of instance)))
                                          (assert oid)
                                          ;; FIXME KLUDGE find-symbol due to load-order issues
                                          ;; create a dwim-model asd and it should depend on dwim-presentation
                                          (format nil "`p(~A,~A)" oid (funcall (find-symbol "LOCALIZED-INSTANCE-REFERENCE-STRING" :hu.dwim.wui)
                                                                               instance))))
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

(bind ((persistent-appender (make-instance 'persistent-appender)))
  (flet ((setup (logger-name)
           (bind ((logger (find-logger logger-name)))
             (push persistent-appender (hu.dwim.logger::appenders-of logger))
             (setf (log-level logger) +dribble+))))
    (setup 'audit)
    (setup 'login)))
