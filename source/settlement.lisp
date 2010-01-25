;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (entity e) settlement ()
  ((name :type standard-text :primary #t :reference #t)
   (sub-name :type (or null standard-text) :primary #t)
   (code :type integer-32)
   (zip-code :type integer-32 :primary #t :reference #t)
   (latitude :type (or null float-32))
   (longitude :type (or null float-32))
   (dialing-code :type integer-16 :primary #t)))

#+nil
(def association
  ((:slot parent-settlement :type (or null settlement))
   (:class settlement :slot settlement-part :type (set settlement))))

(def association
  ((:slot settlements :type (set settlement))
   (:slot county :type county)))

;;;;;;
;;; Data

(def function create-settlements (country-code settlement-data)
  (bind ((country (select-country :code country-code))
         (counties (counties-of country)))
    ;; (name code zip-code county-index location-x location-y dialing-code { recurse }*)
    (dolist (data settlement-data)
      (make-settlement
       :name (pop data)
       :sub-name (pop data)
       :code (pop data)
       :zip-code (pop data)
       :county (elt counties (1- (pop data)))
       :latitude (pop data)
       :longitude (pop data)
       :dialing-code (pop data)))))

(def special-variable *hu-settlement-data*)

(def function create-hu-settlements ()
  (create-settlements 'hu (mapcar #'cdr *hu-settlement-data*)))
