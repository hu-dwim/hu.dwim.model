;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.model
  :class hu.dwim.system
  :description "Various reusable model fragments."
  :depends-on (:babel
               :cl-ppcre
               :cl-base64
               :hu.dwim.common
               :hu.dwim.def+cl-l10n
               :hu.dwim.defclass-star
               :hu.dwim.logger
               :hu.dwim.meta-model
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.util
               :hu.dwim.presentation+hu.dwim.perec
               :local-time)
  :components ((:module "source"
                :components ((:file "attachment" :depends-on ("audited-object"))
                             (:file "audited-object" :depends-on ("logger"))
                             (:file "authentication" :depends-on ("cluster"))
                             (:file "cluster" :depends-on ("audited-object"))
                             (:file "desktop" :depends-on ("persistent-component"))
                             (:file "forum" :depends-on ("audited-object"))
                             (:file "logger" :depends-on ("package"))
                             (:file "localization" :depends-on ("package"))
                             (:file "package")
                             (:file "persistent-component" :depends-on ("logger"))
                             (:file "persistent-function" :depends-on ("logger"))
                             (:file "persistent-log" :depends-on ("logger"))
                             (:file "subject-preferences" :depends-on ("logger"))))))
