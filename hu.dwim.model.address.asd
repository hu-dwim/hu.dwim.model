;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.model.address
  :class hu.dwim.system
  :description "Address"
  :depends-on (:hu.dwim.common
               :hu.dwim.def+cl-l10n
               :hu.dwim.defclass-star
               :hu.dwim.logger
               :hu.dwim.meta-model
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.util.all
               :hu.dwim.wui+hu.dwim.perec)
  :components ((:module "source"
                :components ((:file "address" :depends-on ("settlement" "public-place-type"))
                             (:file "country" :depends-on ("logger"))
                             (:file "county" :depends-on ("country"))
                             (:file "hu-settlement" :depends-on ("settlement"))
                             (:file "public-place-type" :depends-on ("logger"))
                             (:file "settlement" :depends-on ("county"))))))
