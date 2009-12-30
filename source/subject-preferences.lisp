;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (entity e) subject-preferences ()
  ((display-command-labels :type (or unbound boolean) :accessor display-command-labels?)
   (maximum-drill-down-depth :type (or null integer-8))
   (timezone :type (or null (text 64)))))

(def association
  ((:type subject)
   (:type (or null subject-preferences))))

(def (function e) current-effective-subject-preferences ()
  (subject-preferences-of (current-effective-subject)))

;;;;;;
;;; Localization

(def localization hu
  (class-name.subject-preferences "beállítások")
  (slot-name.display-command-labels "parancsikonok szövegének megjelenítése")
  (slot-name.maximum-drill-down-depth "maximális lefúrási mélység")
  (slot-name.timezone "időzóna"))
