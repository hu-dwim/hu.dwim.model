;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

(def (entity e) cluster (audited-object)
  ((name
    :type standard-text
    :primary #t
    :reference #t)
   (requested-operation-mode
    :type (member :normal :maintenance :shutdown :mixed)
    :primary #t
    :reference #t
    :compute-as (bind ((requested-operation-modes (mapcar 'requested-operation-mode-of (cluster-nodes-of -self-))))
                  (if (all-the-same? requested-operation-modes)
                      (first requested-operation-modes)
                      :mixed))
    :documentation "The mixture of the requested operation modes of the cluster's nodes.")
   (mail-relay-host-name
    :type (or null standard-text)))
  (:documentation "A cluster több számítógépet magába foglaló fürtözött, elosztott rendszer. A terhelés elosztása a skálázhatóság miatt fontos."))

(def (entity e) cluster-node (audited-object)
  ((host-name
    (machine-instance)
    :type standard-text
    :reference #t
    :primary #t)
   (requested-operation-mode
    :normal
    :type (member :normal :maintenance :shutdown)
    :primary #f
    :documentation "The requested operation mode may be different from the actual. For example when the node is shutdown the operation mode may still be normal indicating what should be done when the node will be started again.")
   (persistent-process-scheduler-poll-time
    nil ;; no persistent process scheduler for now
    :type (or null integer-16))
   (persistent-process-worker-count
    0 ;; no worker for now
    :type (or null integer-16))
   (web-worker-count
    1
    :type (or null integer-16))
   ;; TODO: use to-be-flattened in cluster-node-session
   (startup-at
    :type (or null timestamp)
    :primary #t
    :compute-as (awhen (last-started-session-of -self-)
                  (startup-at-of it)))
   ;; TODO: use to-be-flattened in cluster-node-session
   (last-activity-at
    :type (or null timestamp)
    :primary #t
    :compute-as (awhen (last-started-session-of -self-)
                  (last-activity-at-of it)))
   ;; TODO: use to-be-flattened in cluster-node-session
   (load-average
    :type (or null standard-text)
    :primary #t
    :compute-as (awhen (last-started-session-of -self-)
                  (load-average-of it)))
   ;; TODO: use to-be-flattened in cluster-node-session
   (dynamic-space-usage
    :type (or null standard-text)
    :primary #t
    :compute-as (when-bind session (last-started-session-of -self-)
                  (awhen (dynamic-space-usage-of session)
                    (cl-l10n:format-number/decimal nil it))))
   (web-session-count
    :type (or null integer-16)
    :primary #t
    :compute-as (awhen (last-started-session-of -self-)
                  (web-session-count-of it)))
   (last-started-session
    :type cluster-node-session
    :flattened #t
    :compute-as (select-instance (instance cluster-node-session)
                  (where (and (eq -self- (cluster-node-of instance))
                              (eq (status-of instance) :alive)))
                  (order-by :desc (startup-at-of instance)))))
  (:documentation "Egy a clusterben részt vevö számítógép."))

(def association
  ((:slot cluster :type cluster)
   (:slot cluster-nodes :type (set cluster-node))))

(def (entity e) cluster-node-session ()
  ((status
    :alive
    :type (member :alive :shutdown :crashed))
   (startup-at
    (transaction-timestamp)
    :type timestamp
    :primary #t)
   (shutdown-at
    :type (or null timestamp)
    :primary #t)
   (last-activity-at
    :type (or null timestamp)
    :primary #t
    :reference #t)
   (load-average
    :type (or null standard-text)
    :primary #t)
   (dynamic-space-usage
    :type (or null integer-64)
    :primary #t)
   (web-session-count
    :type (or null integer-16)
    :primary #t))
  (:documentation "A clusterben részt vevő számítógépek minden egyes elindítása és a működése közben felvett statisztikák egy sessionben vannak tárolva."))

(def association
  ((:slot cluster-node :type cluster-node :primary #t :reference #t)
   (:slot cluster-node-sessions :type (set cluster-node-session))))

(def simple-entity-relationship-diagram cluster-diagram
  (cluster cluster-node cluster-node-session authenticated-session)
  :documentation "A cluster több számítógép egyetlen elosztott alkalmazásban való együttes felhasználása.")

(def special-variable *cluster-name* "test")

(def (special-variable e :documentation "The currently running cluster node session or nil") *cluster-node-session* nil)

(def (special-variable e :documentation "The universal time when node activity was last updated in the database") *cluster-node-last-activity* nil)

(def (special-variable e :documentation "The current operation mode which may be different from the requested in the database.") *cluster-node-current-operation-mode*)

(def (special-variable e :documentation "The web server associated with this node.") *cluster-node-web-server* nil)

;;;;;;
;;; Functional

(def print-object cluster-node ()
  (print-persistent-instance -self-)
  (write-string " ")
  (princ (best-effort-slot-value -self- 'host-name)))

(def (function e) is-cluster-node-running? ()
  (not (null *cluster-node-session*)))

(def function assert-cluser-node-is-running ()
  (assert (is-cluster-node-running?) nil "Cluster node is not running"))

(def (function e) select-cluster-by-name (name)
  (aprog1
      (select-instance (instance cluster)
        (where (and (equal (name-of instance) name))))
    (unless it
      (error "Cluster ~S was not found." name))))

(def (function e) select-machine-cluster-node (cluster)
  (bind ((host-name (machine-instance)))
    (select-instance (instance cluster-node)
      (where (and (equal (host-name-of instance) host-name)
                  (eq (cluster-of instance) cluster))))))

;; TODO this is bitrotten
#+nil
(def (function e) startup-cluster-node (cluster-name &optional web-server)
  (assert (not (is-cluster-node-running?)) nil "Cluster node is already running")
  (cluster.info "Starting up cluster node in cluster ~S" cluster-name)
  (setf *cluster-name* cluster-name)
  (setf *cluster-node-web-server* web-server)
  (with-model-database
    (export-persistent-classes-to-database-schema)
    (with-transaction
      (bind ((cluster (select-cluster-by-name cluster-name))
             (cluster-node (select-machine-cluster-node cluster)))
        (assert cluster)
        (assert cluster-node)
        (dolist (crashed-cluster-node-session (select-instances (instance cluster-node-session)
                                                (where (and (eq cluster-node (cluster-node-of instance))
                                                            (eq (status-of instance) :alive)))))
          (cluster.warn "Oops, setting the status of the crashed ~A to :crashed" crashed-cluster-node-session)
          (setf (status-of crashed-cluster-node-session) :crashed))
        (setf *cluster-node-session* (make-instance 'cluster-node-session))
        (setf *cluster-node-current-operation-mode* (requested-operation-mode-of cluster-node))
        (setf (cluster-node-of *cluster-node-session*) cluster-node)
        (when web-server
          (bind ((worker-count (web-worker-count-of cluster-node)))
            (when worker-count
              (cluster.info "Starting up the web server for node ~A with ~S workers" cluster-node worker-count)
              (hu.dwim.web-server:startup-server web-server :initial-worker-count worker-count))))
        (awhen (persistent-process-scheduler-poll-time-of cluster-node)
          (cluster.info "Starting process scheduler for node ~A" cluster-node)
          (start-persistent-process-scheduler it)
          (iter (repeat (persistent-process-worker-count-of cluster-node))
                (start-persistent-process-worker)))
        (dolist (authenticated-session
                  (select (instance)
                    (from (instance authenticated-session))
                    (where (and (eq (cluster-node-of (cluster-node-session-of instance)) cluster-node)
                                (null (logout-at-of instance))))))
          (with-authenticated-session authenticated-session
            (logout/authenticated-session :status :crashed)))
        cluster-node))))

#+nil
(def (function e) shutdown-cluster-node ()
  (cluster.info "Shutting down cluster node")
  (assert-cluser-node-is-running)
  (with-model-database
    (prog1
        (with-transaction
          (with-reloaded-instance *cluster-node-session*
            (bind ((cluster-node (cluster-node-of *cluster-node-session*)))
              (setf (shutdown-at-of *cluster-node-session*) (transaction-timestamp))
              (setf (status-of *cluster-node-session*) :shutdown)
              ;; TODO expire web sessions
              (awhen *cluster-node-web-server*
                (hu.dwim.web-server:shutdown-server it))
              (when (persistent-process-scheduler-poll-time-of cluster-node)
                (stop-persistent-process-scheduler)
                (stop-all-persistent-process-workers))
              cluster-node)))
      (setf *cluster-node-session* nil)
      (setf *cluster-node-web-server* nil)
      (setf *cluster-node-current-operation-mode* nil))))

;; TODO: this needs to be called from wui server when activity happens in an authenticated session
(def (function e) notify-cluster-node-activity ()
  (cluster.debug "Registering cluster node activity")
  (assert-cluser-node-is-running)
  (bind ((now (isys:get-monotonic-time)))
    (when (or (not *cluster-node-last-activity*)
              (> (- now *cluster-node-last-activity*) 60))
      (setf *cluster-node-last-activity* now)
      (with-transaction
        (with-reloaded-instance *cluster-node-session*
          (setf (last-activity-at-of *cluster-node-session*) (transaction-timestamp))
          (setf (load-average-of *cluster-node-session*) (alexandria:read-file-into-string "/proc/loadavg"))
          (setf (dynamic-space-usage-of *cluster-node-session*) (sb-kernel::dynamic-usage))
          (setf (web-session-count-of *cluster-node-session*) (awhen *cluster-node-web-server*
                                                                (hu.dwim.web-server::total-web-session-count it))))))))

#+nil ; TODO: unfinished and since then also bitrotten
(def (function e) synchronize-cluster-node ()
  ;; TODO: what about changing the number of workers and other parameters?
  (cluster.info "Synchornizing cluster node")
  (assert-cluser-node-is-running)
  (with-model-database
    (with-transaction
      (bind ((cluster-node (cluster-node-of *cluster-node-session*))
             (cluster-name (name-of (cluster-of cluster-node)))
             (requested-operation-mode (requested-operation-mode-of cluster-node)))
        (when (is-cluster-node-running?)
          (assert (eq cluster-node
                      (with-reloaded-instance *cluster-node-session*
                        (cluster-node-of *cluster-node-session*)))))
        (ecase requested-operation-mode
          (:normal
           (unless (is-cluster-node-running?)
             (startup-cluster-node cluster-name)))
          (:maintenance
           (unless (is-cluster-node-running?)
             (startup-cluster-node cluster-name)))
          (:shutdown
           (when (is-cluster-node-running?)
             (shutdown-cluster-node))))
        (setf *cluster-node-current-operation-mode* requested-operation-mode)
        cluster-node))))

(def (function e) machine-short-textual-information ()
  (concatenate 'string (machine-instance) " " (alexandria:read-file-into-string "/proc/loadavg")))

(def (function e) machine-load-in-percent ()
  (bind ((load (parse-number:parse-number (second (cl-ppcre:split " " (alexandria:read-file-into-string "/proc/loadavg"))))))
    (round (* load 100))))

;;;;;;
;;; cluster/detail/inspector

(def (component e) cluster/detail/inspector (t/detail/inspector)
  ((cluster :type component)
   (cluster-nodes :type components)))

(def refresh-component cluster/detail/inspector
  (bind (((:slots cluster cluster-nodes component-value) -self-))
    (setf cluster (make-value-viewer component-value)
          cluster-nodes (make-value-viewer (cluster-nodes-of component-value)))))

(def render-xhtml cluster/detail/inspector ()
  (bind (((:slots cluster cluster-nodes) -self-))
    <div ,(render-component cluster)
         ,(render-component cluster-nodes)>))

(def layered-function make-cluster-commands (component class prototype instance)
  (:method ((component t/inspector) (class entity) (prototype cluster) (instance cluster))
    ;; TODO:
    ))

(def layered-method make-command-bar-commands ((component t/inspector) (class entity) (prototype cluster) (instance cluster))
  (append (call-next-layered-method) (make-cluster-commands component class prototype instance)))

(def layered-method make-command-bar-commands ((component t/inspector) (class entity) (prototype cluster-node) (instance cluster-node))
  (append (call-next-layered-method) (make-cluster-node-commands component class prototype instance)))

(def layered-method make-context-menu-items ((component t/row/inspector) (class entity) (prototype cluster-node) (instance cluster-node))
  (list* (make-menu-item "Cluster node" (make-cluster-node-commands component class instance prototype)) (call-next-layered-method)))

(def layered-method make-command-bar-commands ((component t/row/inspector) (class entity) (prototype cluster-node) (instance cluster-node))
  (list* (make-menu-item "Cluster node" (make-cluster-node-commands component class instance prototype)) (call-next-layered-method)))

(def layered-function make-cluster-node-commands (component class prototype instance)
  (:method ((component t/inspector) (class entity) (prototype cluster-node) (instance cluster-node))
    (optional-list (make-restart-cluster-node-command component class prototype instance)
                   (make-startup-cluster-node-command component class prototype instance)
                   (make-shutdown-cluster-node-command component class prototype instance))))

(def icon restart-cluster-node)

(def layered-function make-restart-cluster-node-command (component class prototype instance)
  (:method ((component t/inspector) (class entity) (prototype cluster-node) (instance cluster-node))
    (command/widget ()
      (icon/widget restart-cluster-node)
      (make-action
        (not-yet-implemented)))))

(def icon startup-cluster-node)

(def layered-function make-startup-cluster-node-command (component class prototype instance)
  (:method ((component t/inspector) (class entity) (prototype cluster-node) (instance cluster-node))
    (command/widget ()
      (icon/widget startup-cluster-node)
      (make-action
        (not-yet-implemented)))))

(def icon shutdown-cluster-node)

(def layered-function make-shutdown-cluster-node-command (component class prototype instance)
  (:method ((component t/inspector) (class entity) (prototype cluster-node) (instance cluster-node))
    (command/widget ()
      (icon/widget shutdown-cluster-node)
      (make-action
        (not-yet-implemented)))))
