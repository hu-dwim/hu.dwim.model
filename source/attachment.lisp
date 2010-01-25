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

(def (entity e) attachment (audited-object)
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

(def (entity e) object-with-attachments ()
  ()
  (:abstract #t))

(def association
  ((:slot object-with-attachments :type object-with-attachments)
   (:slot attachments :type (set attachment))))
