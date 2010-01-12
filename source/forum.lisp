;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (entity e) topic (audited-object)
  ((title
    :type standard-text
    :reference #t
    :primary #t)))

(def association
  ((:slot parent-topic :type (or null topic))
   (:slot children-topics :type (set topic))))

(def (entity e) topic-post (audited-object)
  ((subject
    :type standard-text
    :reference #t
    :primary #t)
   (content
    :type html-text)
   (publish-at
    (now)
    :type timestamp)))

(def association
  ((:slot topic :type topic)
   (:slot topic-posts :type (set topic-post))))

(def (singleton-persistent-instance e) *root-topic*
  (select-instance (instance topic)
    (where (null (parent-topic-of instance)))))

;;;;;;
;;; Localization

(def localization en
  (class-name.topic "topic")
  (class-name.topic-post "post")

  (slot-name.title "title")
  (slot-name.subject "subject")
  (slot-name.content "content")
  (slot-name.publish-at "publish at")
  (slot-name.children-topics "children topics"))

(def localization hu
  (class-name.topic "téma")
  (class-name.topic-post "bejegyzés")

  (slot-name.title "cím")
  (topic-post.subject "tárgy")
  (slot-name.content "tartalom")
  (slot-name.publish-at "megjelenés ideje")
  (slot-name.children-topics "altémák"))
