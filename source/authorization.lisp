;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Generic operations

(def operation standard-operation ()
  ()
  "Base operation for all operations.")

;;;;;;
;;; Model operations

(def operation standard-model-operation (standard-operation)
  ()
  "Base operation for all model related operations.")

(def operation write-model-operation (standard-model-operation)
  ()
  "Base operation for all model related operations that have side-effects.")

(def operation read-model-operation  (standard-model-operation)
  ()
  "Base operation for all model related operations that read data from the model.")

;;;;;;
;;; Entity operations

(def operation standard-entity-operation (standard-operation)
  (-entity-)
  "Base operation for all entity related operations.")

(def operation create-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits creating instances.")

(def operation read-write-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits reading and writing instances.")

(def operation read-entity-operation (read-write-entity-operation)
  (-entity-)
  "Permits reading instances.")

(def operation write-entity-operation (read-write-entity-operation)
  (-entity-)
  "Permits writing instances.")

(def operation filter-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits filtering for instances.")

(def operation delete-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits deleting instances.")

(def operation document-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits documenting entities.")

;;;;;;
;;; Entity property operations

(def operation standard-entity-property-operation (standard-entity-operation)
  (-entity- -property-)
  "Base operation for all entity property related operations.")

(def operation create-entity-property-operation (standard-entity-operation)
  (-entity- -property-)
  "Permits creating instances with the propery.")

(def operation read-write-entity-property-operation (standard-entity-property-operation)
  (-entity- -property-)
  "Permits filtering for instances based on entity properties.")

(def operation filter-entity-property-operation (standard-entity-property-operation)
  (-entity- -property-)
  "Permits filtering for instances based on entity properties.")

(def operation read-entity-property-operation (read-write-entity-property-operation)
  (-entity- -property-))

(def operation write-entity-property-operation (read-write-entity-property-operation)
  (-entity- -property-))

;;;;;;
;;; Instance operations

(def operation standard-instance-operation (standard-entity-operation)
  (-entity- -instance-)
  "Base operation for all instance related operations.")

(def operation read-write-instance-operation (standard-instance-operation)
  (-entity- -instance-)
  "Base operation that includes both read and write operations.")

(def operation create-instance-operation (standard-instance-operation)
  (-entity- -instance-)
  "Permits creating new instances.")

(def operation read-instance-operation (read-write-instance-operation)
  (-entity- -instance-)
  "Permits reading some or all properties of an instance.")

(def operation write-instance-operation (read-write-instance-operation)
  (-entity- -instance-)
  "Permits writing some or all properties of an instance.")

(def operation delete-instance-operation (standard-instance-operation)
  (-entity- -instance-)
  "Permits permanently deleting an instance.")

;;;;;;
;;; Persistent Process operations

(def operation standard-persistent-process-operation (standard-entity-operation)
  (-entity-)
  "Base operation for all persistent-process related operations.")

(def operation start-persistent-process-operation (standard-persistent-process-operation)
  (-entity-)
  "Permits starting persistent processes.")

(def operation standard-persistent-process-instance-operation (standard-instance-operation standard-persistent-process-operation)
  (-entity- -instance-)
  "Base operation for all persistent-process instance related operations.")

(def operation continue-persistent-process-operation (standard-persistent-process-instance-operation)
  (-entity- -instance-)
  "Permits continuing a persistent processe instance.")

;;;;;;
;;; Instance property operations

(def operation standard-instance-property-operation (standard-instance-operation)
  (-entity- -instance- -property-))

(def operation read-write-instance-property-operation (standard-instance-property-operation)
  (-entity- -instance- -property-))

(def operation read-instance-property-operation (read-write-instance-property-operation)
  (-entity- -instance- -property-))

(def operation write-instance-property-operation (read-write-instance-property-operation)
  (-entity- -instance- -property-))

;;;;;;
;;; Subject related operations

(def operation impersonalize-operation (standard-entity-operation)
  (-entity-))

(def operation generate-new-password-operation (standard-entity-operation)
  (-entity-))

;;;;;;
;;; General rules

#|
;; add somethig similar to your application:

(def authorization default-authorization () ()
  #t)

(def authorization top-level-authorization () ()
  default-authorization)
|#
