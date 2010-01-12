;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Persistent function

(def (entity e) persistent-function ()
  ((name
    :type standard-text
    :primary #t
    :reference #t)
   (description
    :type (or null html-text)
    :primary #t)
   (body
    :type form)))

(def (entity e) persistent-argument ()
  ((name
    :type standard-text
    :primary #t
    :reference #t)
   (description
    :type (or null html-text)
    :primary #t)
   (the-type
    :type form)))

(def association
  ((:slot persistent-function :type persistent-function :primary #t)
   (:slot persistent-arguments :type (set persistent-argument))))

;;;;;;
;;; Localization

(def localization en
  (class-name.persistent-function "function")
  (class-name.persistent-argument "argument")

  (slot-name.name "name")
  (slot-name.description "description")
  (slot-name.body "body")
  (slot-name.the-type "type"))

(def localization hu
  (class-name.persistent-function "függvény")
  (class-name.persistent-argument "paraméter")

  (slot-name.name "név")
  (slot-name.description "leírás")
  (slot-name.body "törzs")
  (slot-name.the-type "típus"))
