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
  ((:slot desktop :type (or null desktop))
   (:slot persistent-components :type (set persistent-component))))
