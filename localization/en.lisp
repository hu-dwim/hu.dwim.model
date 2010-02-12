;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

(def localization en
  (class-name.attachment "attachment")
  (class-name.audited-object "audited object")
  (class-name.cluster "cluster")
  (class-name.cluster-node "cluster node")
  (class-name.cluster-node-session "cluster node session")
  (class-name.desktop "desktop")
  (class-name.encrypted-password-authentication-instrument "encrypted password authentication instrument")
  (class-name.object-with-attachments "object with attachments")
  (class-name.persistent-argument "argument")
  (class-name.persistent-component "component")
  (class-name.persistent-function "function")
  (class-name.persistent-log-entry "persistent log entry")
  (class-name.subject-preferences "subject preferences")
  (class-name.topic "topic")
  (class-name.topic-post "topic post")
  (diagram-name.authentication-diagram "authentication diagram")
  (diagram-name.cluster-diagram "cluster diagram")
  (level.+debug+ "debug")
  (level.+dribble+ "dribble")
  (level.+error+ "error")
  (level.+fatal+ "fatal error")
  (level.+info+ "information")
  (level.+warn+ "warning")
  (requested-operation-mode.maintenance "maintenance")
  (requested-operation-mode.mixed "mixed")
  (requested-operation-mode.normal "normal")
  (requested-operation-mode.shutdown "shutdown")
  (slot-name.body "body")
  (slot-name.category "category")
  (slot-name.children-topics "children topics")
  (slot-name.code "code")
  (slot-name.content "content")
  (slot-name.created-at "created at")
  (slot-name.created-in "created in")
  (slot-name.created-instances "created instances")
  (slot-name.description "description")
  (slot-name.display-command-labels "display command labels")
  (slot-name.dynamic-space-usage "dynamic space usage")
  (slot-name.host-name "host name")
  (slot-name.last-activity-at "last activity at")
  (slot-name.last-modified-at "last modified at")
  (slot-name.last-modified-in "last modified in")
  (slot-name.last-modified-instances "last modified instances")
  (slot-name.last-started-session "last started session")
  (slot-name.level "level")
  (slot-name.load-average "load average")
  (slot-name.logged-at "logged at")
  (slot-name.mail-relay-host-name "mail relay host name")
  (slot-name.maximum-drill-down-depth "maximum drill down depth")
  (slot-name.mime-type "MIME type")
  (slot-name.name "name")
  (slot-name.password "password")
  (slot-name.password-expires-at "password expires at")
  (slot-name.persistent-process-scheduler-poll-time "persistent process scheduler poll time")
  (slot-name.persistent-process-worker-count "persistent process worker count")
  (slot-name.publish-at "publish at")
  (slot-name.requested-operation-mode "operation mode")
  (slot-name.salt "password salt")
  (slot-name.shutdown-at "shutdown at")
  (slot-name.startup-at "startup at")
  (slot-name.subject "subject")
  (slot-name.the-type "type")
  (slot-name.timezone "timezone")
  (slot-name.title "title")
  (slot-name.web-session-count "web session count")
  (slot-name.web-worker-count "web worker count")
  (slot-name.zip-code "zip code"))
