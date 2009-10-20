;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.model
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various model fragments"
  :depends-on (:cl-ppcre
               :cl-base64
               :hu.dwim.common
               :hu.dwim.def+cl-l10n
               :hu.dwim.defclass-star
               :hu.dwim.logger
               :hu.dwim.meta-model
               :hu.dwim.perec
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.util
               :local-time)
  :components ((:module "source"
                :components ((:file "address" :depends-on ("settlement" "public-place-type"))
                             (:file "attachment" :depends-on ("audited-object"))
                             (:file "audited-object" :depends-on ("configuration"))
                             (:file "authentication" :depends-on ("cluster"))
                             (:file "authorization" :depends-on ("configuration"))
                             (:file "cluster" :depends-on ("audited-object"))
                             (:file "configuration" :depends-on ("logger"))
                             (:file "country" :depends-on ("configuration"))
                             (:file "county" :depends-on ("country"))
                             (:file "desktop" :depends-on ("persistent-component"))
                             (:file "forum" :depends-on ("audited-object"))
                             (:file "hu-settlement" :depends-on ("settlement"))
                             (:file "logger" :depends-on ("package"))
                             (:file "package")
                             (:file "persistent-component" :depends-on ("configuration"))
                             (:file "persistent-function" :depends-on ("configuration"))
                             (:file "persistent-log" :depends-on ("configuration"))
                             (:file "public-place-type" :depends-on ("configuration"))
                             (:file "settlement" :depends-on ("county"))
                             (:file "subject-preferences" :depends-on ("configuration"))))))
