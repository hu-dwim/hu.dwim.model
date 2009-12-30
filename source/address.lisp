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
  ((:class public-place-type :slot public-places :type (set public-place))
   (:class public-place :slot public-place-type :type public-place-type)))

(def association
  ((:class settlement :slot public-places :type (set public-place))
   (:class public-place :slot settlement :type settlement :reference #t)))

(def localization en
  (class-name.public-place "public place")
  (class-name.public-place-type "public place type"))

(def localization hu
  (class-name.public-place "közterület")
  (class-name.public-place-type "közterület típus"))

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
  ((:class public-place :slot addresses :type (set address))
   (:class address :slot public-place :type public-place :primary #t)))

;;;;;;
;;; Localization

(def localization en
  (class-name.address "address")

  (slot-name.house-number "house number")
  (slot-name.floor-number "floor number")
  (slot-name.door "door")
  (slot-name.short-address "short address")
  (slot-name.full-address "full address")

  ;; TODO: fix english address
  (short-address (address)
    ;; TODO these slobops shouldn't be needed, fix slots instead
    (bind ((public-place (when (slot-boundp address 'public-place) (public-place-of address)))
           (settlement (when (slot-boundp public-place 'settlement) (settlement-of public-place))))
      (string+ (when public-place
                            (name-of public-place))
                          " "
                          (when public-place
                            (name-of (public-place-type-of public-place)))
                          " "
                          (when address
                            (princ-to-string (house-number-of address)))
                          ", "
                          (name-of settlement))))
  
  ;; TODO: fix english address
  (full-address (address)
    (bind ((public-place (public-place-of address))
           (settlement (settlement-of public-place)))
      (string+ (name-of public-place)
                          " "
                          (name-of (public-place-type-of public-place))
                          " "
                          (princ-to-string (house-number-of address))
                          " "
                          (princ-to-string (floor-number-of address))
                          "/"
                          (princ-to-string (door-of address))
                          " "
                          (name-of settlement)
                          ", "
                          (name-of (country-of (county-of settlement)))
                          " "
                          (princ-to-string (zip-code-of settlement))))))

(def localization hu
  (class-name.address "cím")

  (slot-name.house-number "házszám")
  (slot-name.floor-number "emelet")
  (slot-name.door "ajtó")
  (slot-name.short-address "rövid cím")
  (slot-name.full-address "teljes cím")

  (short-address (address)
    ;; TODO these slobops shouldn't be needed, fix slots instead
    (bind ((public-place (when (slot-boundp address 'public-place) (public-place-of address)))
           (settlement (when (slot-boundp public-place 'settlement) (settlement-of public-place))))
      (string+ (name-of settlement)
                          ", "
                          (when public-place
                            (name-of public-place))
                          " "
                          (when public-place
                            (name-of (public-place-type-of public-place)))
                          " "
                          (when address
                            (princ-to-string (house-number-of address))))))

  (full-address (address)
    (bind ((public-place (public-place-of address))
           (settlement (settlement-of public-place)))
      (string+ (name-of (country-of (county-of settlement)))
                          " "
                          (princ-to-string (zip-code-of settlement))
                          " "
                          (name-of settlement)
                          ", "
                          (name-of public-place)
                          " "
                          (name-of (public-place-type-of public-place))
                          " "
                          (princ-to-string (house-number-of address))
                          " "
                          (princ-to-string (floor-number-of address))
                          "/"
                          (princ-to-string (door-of address))))))
