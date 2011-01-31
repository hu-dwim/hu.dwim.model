;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (special-variable e) *audit-changes* #t)

;;; unbound is used for the various slot/association types because
;;; these slots are unbound until the end of the transaction the given
;;; instance is created.

(def (entity e) audited-object ()
  ((created-at
    :type (or unbound timestamp)
    :definitive #t
    :editable #f)
   (last-modified-at
    :type (or unbound timestamp)
    :editable #f))
  (:abstract #t)
  (:direct-store :push-down)
  (:documentation "Maintains various audit information such as created by, created at, last modified by and last modified at. These are automatically updated within the transaction. Authentication must be used in order to get the current session otherwise an error will be thrown."))

(def method make-persistent-using-class :after (class (instance audited-object))
  (when (and *audit-changes*
             (created-p instance))
    (touch-instance instance #t)))

(def association
  ((:slot created-instances :type (set audited-object))
   (:slot created-in :type (or unbound authenticated-session) :definitive #t :editable #f)))

(def association
  ((:slot last-modified-instances :type (set audited-object))
   (:slot last-modified-in :type (or unbound authenticated-session) :editable #f)))

;;;;;;
;;; Functional

(def method (setf slot-value-using-class) :before (new-value
                                                  (class persistent-class)
                                                  (instance audited-object)
                                                  (slot persistent-effective-slot-definition))
  (let ((slot-name (slot-definition-name slot)))
    (when (and *audit-changes*
               (persistent-p instance)
               (not (member slot-name
                            '(created-at created-in
                              last-modified-at last-modified-in)
                            :test #'eq))
               (not (modified-p instance))
               (not (created-p instance)))
      (touch-instance instance #f))))

(def (function e) last-modified-at-of/if-actually-modified (instance)
  (bind ((last-modified-at (last-modified-at-of instance)))
    (unless (timestamp= last-modified-at (created-at-of instance))
      last-modified-at)))

(def (function e) last-modified-in-of/if-actually-modified (instance)
  (bind ((last-modified-in (last-modified-in-of instance)))
    (unless (eq last-modified-in (created-in-of instance))
      last-modified-in)))

(def (function e) touch-instance (instance &optional (created nil created-p))
  (bind ((now (transaction-timestamp))
         (authenticated-session (current-authenticated-session)))
    (when (or (and created-p
                   created)
              (and (not created-p)
                   (created-p instance)))
      (setf (created-in-of instance) authenticated-session)
      (setf (created-at-of instance) now))
    (setf (last-modified-in-of instance) authenticated-session)
    (setf (last-modified-at-of instance) now)))
