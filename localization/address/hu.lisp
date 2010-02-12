;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

(def localization hu
  (class-name.address "cím")
  (class-name.country "ország")
  (class-name.county "megye")
  (class-name.public-place "közterület")
  (class-name.public-place-type "közterület típus")
  (class-name.settlement "település")
  (slot-name.dialing-code "előhívó szám")
  (slot-name.door "ajtó")
  (slot-name.floor-number "emelet")
  (slot-name.full-address "teljes cím")
  (slot-name.house-number "házszám")
  (slot-name.latitude "szélességi fok")
  (slot-name.longitude "hosszúsági fok")
  (slot-name.short-address "rövid cím")
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
