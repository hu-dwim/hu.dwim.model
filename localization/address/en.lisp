;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

(def localization en
  (class-name.address "address")
  (class-name.country "country")
  (class-name.county "county")
  (class-name.public-place "public place")
  (class-name.public-place-type "public place type")
  (class-name.settlement "settlement")
  (slot-name.dialing-code "dialing code")
  (slot-name.door "door")
  (slot-name.floor-number "floor number")
  (slot-name.full-address "full address")
  (slot-name.house-number "house number")
  (slot-name.latitude "latitude")
  (slot-name.longitude "longitude")
  (slot-name.short-address "short address")
  (short-address (address) ; TODO: fix english address
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
  (full-address (address) ; TODO: fix english address
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
