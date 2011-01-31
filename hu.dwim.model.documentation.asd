;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.model.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.model.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "model" :depends-on ("package"))
                             (:file "package")))))
