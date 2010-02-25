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

(def (persistent-singleton e) *root-topic*
  (select-instance (instance topic)
    (where (null (parent-topic-of instance)))))

;;;;;;
;;; Functional

(def (function e) select-last-topic-posts (topic-post)
  (select (instance)
    (from (instance topic-post))
    (where (and (eq (topic-of instance) topic-post)
                (timestamp< (publish-at-of instance) (transaction-timestamp))))
    (order-by :descending (publish-at-of instance))
    (limit 10)))
