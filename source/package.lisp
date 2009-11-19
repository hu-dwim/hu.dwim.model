;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.model
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.meta-model
        :hu.dwim.perec
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :local-time)
  (:readtable-setup
   (enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))
