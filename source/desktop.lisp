;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Desktop

(def (entity e) desktop ()
  ((name
    :type standard-text
    :reference #t
    :primary #t)))

(def association
  ((:type (set persistent-component))
   (:type desktop)))

;;;;;;
;;; Localization

(def localization en
  (class-name.desktop "desktop"))

(def localization hu
  (class-name.desktop "asztal"))
