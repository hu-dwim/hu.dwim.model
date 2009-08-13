;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (persistent-type e) mime-type ()
  `(text 100))

(def entity attachment (audited-object)
  ((name
    :type (or null standard-text)
    :reference #t
    :primary #t)
   (content
    :type serialized
    :prefetch #f)
   (mime-type
    :type mime-type
    :primary #t)))

(def entity object-with-attachments ()
  ()
  (:abstract #t))

(def association
  ((:type object-with-attachments)
   (:type (set attachment))))

;;;;;;
;;; Localization

(def localization en
  (class-name.attachment "attachment")
  (class-name.object-with-attachments "object with attachments")

  (slot-name.name "name")
  (slot-name.mime-type "MIME type")
  (slot-name.content "content"))

(def localization hu
  (class-name.attachment "csatolás")
  (class-name.object-with-attachments "csatolásokkal renderlező objektum")

  (slot-name.name "név")
  (slot-name.mime-type "MIME típus")
  (slot-name.content "tartalom"))
