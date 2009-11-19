;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.model.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.model
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.model)))
