;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (entity e) public-place ()
  ((name :type standard-text :primary #t :reference #t)
   (long-name :type standard-text :compute-as (string+)))) ;; TODO:

(def association
  ((:slot public-places :type (set public-place))
   (:slot public-place-type :type public-place-type)))

(def association
  ((:slot public-places :type (set public-place))
   (:slot settlement :type settlement :reference #t)))

(def (entity e) address ()
  ((house-number :type integer-16 :primary #t)
   (floor-number :type (or null integer-16) :primary #t)
   (door :type (or null integer-16) :primary #t)
   (short-address :type standard-text :reference #t :compute-as (short-address -self-))
   (full-address :type standard-text :compute-as (full-address -self-))
   (latitude :type (or null float-32))
   (longitude :type (or null float-32)))
  (:statistics :min 10))

(def association
  ((:slot addresses :type (set address))
   (:slot public-place :type public-place :primary #t)))
