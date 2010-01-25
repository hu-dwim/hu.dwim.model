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
