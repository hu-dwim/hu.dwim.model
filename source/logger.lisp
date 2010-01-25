;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

(def logger model ())

(def logger cluster (model))

;; TODO: move?
(def localization-loader-callback model-localization-loader :hu.dwim.model "localization")
