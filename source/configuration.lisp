;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; These definitions need to be available by the time we are reading other files, therefore they are in a standalone file.

(def function setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-lambda-with-bang-args-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '(:hu.dwim.model :hu.dwim.model.test) 'setup-readtable)
