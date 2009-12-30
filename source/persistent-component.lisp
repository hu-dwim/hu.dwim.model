;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Persistent component

(def (entity e) persistent-component ()
  ((description
    :type (or null html-text)
    :reference #t
    :primary #t)
   (content
    :type serialized)))

;;;;;;
;;; Localization

(def localization en
  (class-name.persistent-component "component")

  (slot-name.description "description")
  (slot-name.content "content"))

(def localization hu
  (class-name.persistent-component "komponens")

  (slot-name.description "leírás")
  (slot-name.content "tartalom"))
