;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Generic operations

(def (operation e) standard-operation ()
  ()
  "Base operation for all operations.")

;;;;;;
;;; Model operations

(def (operation e) standard-model-operation (standard-operation)
  ()
  "Base operation for all model related operations.")

(def (operation e) write-model-operation (standard-model-operation)
  ()
  "Base operation for all model related operations that have side-effects.")

(def (operation e) read-model-operation  (standard-model-operation)
  ()
  "Base operation for all model related operations that read data from the model.")

;;;;;;
;;; Entity operations

(def (operation e) standard-entity-operation (standard-operation)
  (-entity-)
  "Base operation for all entity related operations.")

(def (operation e) create-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits creating instances.")

(def (operation e) read-write-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits reading and writing instances.")

(def (operation e) read-entity-operation (read-write-entity-operation)
  (-entity-)
  "Permits reading instances.")

(def (operation e) write-entity-operation (read-write-entity-operation)
  (-entity-)
  "Permits writing instances.")

(def (operation e) filter-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits filtering for instances.")

(def (operation e) delete-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits deleting instances.")

(def (operation e) document-entity-operation (standard-entity-operation)
  (-entity-)
  "Permits documenting entities.")

;;;;;;
;;; Entity property operations

(def (operation e) standard-entity-property-operation (standard-entity-operation)
  (-entity- -property-)
  "Base operation for all entity property related operations.")

(def (operation e) create-entity-property-operation (standard-entity-operation)
  (-entity- -property-)
  "Permits creating instances with the propery.")

(def (operation e) read-write-entity-property-operation (standard-entity-property-operation)
  (-entity- -property-)
  "Permits filtering for instances based on entity properties.")

(def (operation e) filter-entity-property-operation (standard-entity-property-operation)
  (-entity- -property-)
  "Permits filtering for instances based on entity properties.")

(def (operation e) read-entity-property-operation (read-write-entity-property-operation)
  (-entity- -property-))

(def (operation e) write-entity-property-operation (read-write-entity-property-operation)
  (-entity- -property-))

;;;;;;
;;; Instance operations

(def (operation e) standard-instance-operation (standard-entity-operation)
  (-entity- -instance-)
  "Base operation for all instance related operations.")

(def (operation e) read-write-instance-operation (standard-instance-operation)
  (-entity- -instance-)
  "Base operation that includes both read and write operations.")

(def (operation e) create-instance-operation (standard-instance-operation)
  (-entity- -instance-)
  "Permits creating new instances.")

(def (operation e) read-instance-operation (read-write-instance-operation)
  (-entity- -instance-)
  "Permits reading some or all properties of an instance.")

(def (operation e) write-instance-operation (read-write-instance-operation)
  (-entity- -instance-)
  "Permits writing some or all properties of an instance.")

(def (operation e) delete-instance-operation (standard-instance-operation)
  (-entity- -instance-)
  "Permits permanently deleting an instance.")

;;;;;;
;;; Persistent Process operations

(def (operation e) standard-persistent-process-operation (standard-entity-operation)
  (-entity-)
  "Base operation for all persistent-process related operations.")

(def (operation e) start-persistent-process-operation (standard-persistent-process-operation)
  (-entity-)
  "Permits starting persistent processes.")

(def (operation e) standard-persistent-process-instance-operation (standard-instance-operation standard-persistent-process-operation)
  (-entity- -instance-)
  "Base operation for all persistent-process instance related operations.")

(def (operation e) continue-persistent-process-operation (standard-persistent-process-instance-operation)
  (-entity- -instance-)
  "Permits continuing a persistent processe instance.")

;;;;;;
;;; Instance property operations

(def (operation e) standard-instance-property-operation (standard-instance-operation)
  (-entity- -instance- -property-))

(def (operation e) read-write-instance-property-operation (standard-instance-property-operation)
  (-entity- -instance- -property-))

(def (operation e) read-instance-property-operation (read-write-instance-property-operation)
  (-entity- -instance- -property-))

(def (operation e) write-instance-property-operation (read-write-instance-property-operation)
  (-entity- -instance- -property-))

;;;;;;
;;; Subject related operations

(def (operation e) impersonalize-operation (standard-entity-operation)
  (-entity-))

(def (operation e) generate-new-password-operation (standard-entity-operation)
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


;;;;;;
;;; KLUDGE

(def (operation e) export-instance-operation (standard-instance-operation)
  (-entity- -instance- -format-))

(def (operation e) expand-instance-operation (standard-instance-operation)
  (-entity- -instance-))
