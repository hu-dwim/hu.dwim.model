;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (entity e) county ()
  ((name :type standard-text :primary #t :reference #t)))

(def association
  ((:class country :slot counties :type (set county))
   (:class county :slot country :type country :primary #t)))

;;;;;;
;;; Localization

(def localization en
  (class-name.county "county"))

(def localization hu
  (class-name.county "megye"))

;;;;;;
;;; Data

(def generic create-counties (country &optional counties)
  (:method ((country-code symbol) &optional counties)
    (create-counties (select-country :code country-code) counties))

  (:method ((country-code string) &optional counties)
    (create-counties (find-symbol (string-upcase country-code) :hu.dwim.model) counties))

  (:method ((country persistent-object) &optional counties)
    (bind ((variable-name (format-symbol :hu.dwim.model "*~A-COUNTY-DATA*" (code-of country))))
      (dolist (data (or counties (symbol-value variable-name)))
        (make-county :name (pop data) :country country)))))

(def function create-hu-counties ()
  (create-counties 'hu))

(def special-variable *hu-county-data*
  '(("Bács-Kiskun")
    ("Baranya")
    ("Békés")
    ("Borsod-Abaúj-Zemplén")
    ("Budapest")
    ("Csongrád")
    ("Fejér")
    ("Győr-Moson-Sopron")
    ("Hajdú-Bihar")
    ("Heves")
    ("Jász-Nagykun-Szolnok")
    ("Komárom-Esztergom")
    ("Nógrád")
    ("Pest")
    ("Somogy")
    ("Szabolcs-Szatmár-Bereg")
    ("Tolna")
    ("Vas")
    ("Veszprém")
    ("Zala")))
