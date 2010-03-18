;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; API

(def (generic e) iterate-possible-authentication-instruments (application identifier visitor))

(def (layered-function e) valid-login-identifier? (authentication-instrument identifier)
  (:method :around (authentication-instrument identifier)
    (string/trim-whitespace-and-maybe-nil-it identifier)
    (and identifier
         (call-next-method authentication-instrument identifier)))
  (:method (authentication-instrument identifier)
    #t))

(def (layered-function e) valid-login-password? (authentication-instrument password)
  (:method :around (authentication-instrument password)
    (string/trim-whitespace-and-maybe-nil-it password)
    (and password
         (call-next-method authentication-instrument password)))
  (:method (authentication-instrument password)
    #t))

(def (condition* e) authentication-instrument-policy-violated (error)
  ((authentication-instrument)))

(def function authentication-instrument-policy-violated (instrument)
  (error 'authentication-instrument-policy-violated :authentication-instrument instrument))

;;;;;;
;;; Model

;; http://www.jasypt.org/howtoencryptuserpasswords.html
;; http://www.cafesoft.com/CSDigest/createDigest.do

(def constant +number-of-digest-iterations+ 1000)
(def constant +password-salt-length+ 16)

(def (entity e) encrypted-password-authentication-instrument (authentication-instrument)
  ((password :type (text 64))
   (salt (random-string +password-salt-length+) :type (text #.+password-salt-length+ #.+password-salt-length+))
   (password-expires-at :type (or null date)))
  (:documentation "A jelszavas azonosító eszköz egy a felhasználó által megadott jelszót tárol. Az eltárolt információból a jelszó nem kikövetkeztethető, de egy megadott jelszóra nézve ellenőrizhető, hogy az megegyezik-e az eredetileg megadottal."))

(def association
  ((:slot cluster-node-session :type (or null cluster-node-session) :primary #t
    :initform (when (and (in-transaction-p)
                         *cluster-node-session*)
                (load-instance *cluster-node-session*)))
   (:slot authenticated-sessions :type (set authenticated-session))))

;;;;;;
;;; Functional

(def (function e) digest-password-with-sha256 (password-string &optional salt)
  (bind ((digest (ironclad:digest-sequence :sha256 (babel:string-to-octets (if salt
                                                                               (string+ password-string salt)
                                                                               password-string)
                                                                           :encoding :utf-8))))
    (iter (repeat +number-of-digest-iterations+)
          (setf digest (ironclad:digest-sequence :sha256 digest)))
    (values (concatenate 'string
                         "{sha256,salt,1000}"
                         (cl-base64:usb8-array-to-base64-string digest))
            salt)))

(def function ensure-valid-authentication-instrument-password (authentication-instrument clear-text-password)
  (unless (valid-login-password? authentication-instrument clear-text-password)
    (authentication-instrument-policy-violated authentication-instrument)))

(def (function e) compare-authentication-instrument-password (authentication-instrument clear-text-password)
  (check-type authentication-instrument encrypted-password-authentication-instrument)
  (ensure-valid-authentication-instrument-password authentication-instrument clear-text-password)
  (bind ((digested-password (password-of authentication-instrument)))
    (assert (starts-with-subseq "{sha256,salt,1000}" digested-password))
    (string= digested-password (digest-password-with-sha256 clear-text-password (salt-of authentication-instrument)))))

(def (function e) ensure-encrypted-password-authentication-instrument (subject)
  (bind ((candidates (collect-if (of-type 'encrypted-password-authentication-instrument)
                                 (authentication-instruments-of subject))))
    (cond
      ((length= 0 candidates)
       (make-instance 'encrypted-password-authentication-instrument
                      ;; it's only a random but valid dummy password hash
                      :password "{sha256,salt,1000}fjFsdg3WjGGfsd6qdjyhdggg3g3452HjkyxTk5loalo="
                      :subject subject))
      ((length= 1 candidates)
       (first-elt candidates))
      (t
       (error "Subject ~A has multiple encrypted-password-authentication-instrument's when ensure-encrypted-password-authentication-instrument was called" subject)))))

(def (function e) generate-random-password (subject &key (length 6) alphabet)
  (bind ((password (random-string length (or alphabet +alphanumeric-ascii-alphabet+)))
         (authentication-instrument (ensure-encrypted-password-authentication-instrument subject)))
    (update-authentication-instrument-password authentication-instrument password :expires-at (now))
    (values password authentication-instrument)))

(def (function e) update-authentication-instrument-password (authentication-instrument password &key expires-at)
  (ensure-valid-authentication-instrument-password authentication-instrument password)
  (audit.info "Current authenticated subject ~A is changing the password stored in ~A, owned by ~A"
              (when (and (boundp '*authenticated-session*)
                         *authenticated-session*)
                (current-authenticated-subject))
              authentication-instrument
              (subject-of authentication-instrument))
  (setf (password-expires-at-of authentication-instrument) expires-at)
  (setf (password-of authentication-instrument) (digest-password-with-sha256 password (salt-of authentication-instrument))))

;;;;;;
;;; WUI customization

(def (class* e) application-with-persistent-login-support (application-with-login-support
                                                           application-with-perec-support)
  ())

(def (class* ea) session-with-persistent-login-support (session-with-login-support)
  ((authenticated-session nil)))

(def method session-class list ((application application-with-persistent-login-support))
  'session-with-persistent-login-support)

(def method is-logged-in? ((session session-with-persistent-login-support))
  (and (call-next-method)
       (valid-authenticated-session? (authenticated-session-of session))))

(def method login ((application application-with-persistent-login-support) (web-session session-with-persistent-login-support) login-data)
  (assert (null (authenticated-session-of web-session)))
  (assert (boundp '*authenticated-session*))
  (multiple-value-prog1
      (call-next-method)
    (bind ((arguments (extra-arguments-of login-data))
           ((&key allow-parallel-sessions &allow-other-keys) arguments)
           (authentication-instrument (load-instance (authenticate-return-value-of web-session)))
           ((:values authenticated-session failure-reason) (login/authenticated-session authentication-instrument :allow-parallel-sessions allow-parallel-sessions)))
      (declare (ignore failure-reason))
      (check-type authenticated-session authenticated-session)
      (setf (authenticated-session-of web-session) authenticated-session)
      (setf *authenticated-session* authenticated-session))))

(def method login :around ((application application-with-persistent-login-support) web-session login-data)
  (hu.dwim.meta-model::with-model-database
    (hu.dwim.perec:with-transaction
      (hu.dwim.perec:with-new-compiled-query-cache
        (call-next-method)))))

(def method login ((application application-with-persistent-login-support) (web-session null) login-data)
  (bind ((result-values (multiple-value-list (call-next-method)))
         (web-session (first result-values))
         (web-session-id (hu.dwim.wui::id-of web-session))
         (authenticated-session (authenticated-session-of web-session)))
    (check-type web-session-id string)
    (with-reloaded-instance authenticated-session
      (setf (web-session-id-of authenticated-session) web-session-id)
      (setf (http-user-agent-of authenticated-session) (header-value *request* +header/user-agent+))
      (setf (web-application-of authenticated-session) (human-readable-broker-path *server* application))
      (setf (remote-ip-address-of authenticated-session) *request-remote-address*))
    (values-list result-values)))

(def method logout ((application application-with-persistent-login-support) (session session-with-persistent-login-support))
  (hu.dwim.meta-model::with-model-database
    (hu.dwim.perec:with-transaction
      (hu.dwim.perec:with-new-compiled-query-cache
        (multiple-value-prog1
            (call-next-method)
          (with-authenticated-session (load-instance (authenticated-session-of session))
            (logout/authenticated-session)
            (setf (authenticated-session-of session) nil)))))))

(def method call-in-application-environment ((application application-with-persistent-login-support) session thunk)
  ;; TODO maybe we don't even want to bind it if it's not available...
  (bind ((*authenticated-session* (and *session*
                                       (authenticated-session-of *session*))))
    (authentication.debug "Bound *AUTHENTICATED-SESSION* to ~A from the web session ~A" *authenticated-session* session)
    (call-with-reloaded-authenticated-session #'call-next-method)))

(def method authenticate ((application application-with-persistent-login-support) session (login-data login-data/identifier-and-password))
  (authentication.info "Logging in with authentication information ~A" login-data)
  (assert (in-transaction-p))
  (mark-transaction-for-commit-only)
  (bind ((identifier (identifier-of login-data))
         (password (password-of login-data))
         (authentication-instrument nil))
    (flet ((fail (&optional reason)
             (authentication.info "Login failed for authentication information ~A~:[.~;, reason: ~S.~]" login-data reason reason)
             ;; TODO this is subject to DOS attacks due to the persistent log appender
             (audit.info "Failed authentication using identifier ~S from ip address ~A" (hu.dwim.wui::identifier-of login-data) *request-remote-address/string*)
             (when authentication-instrument
               (bind ((failed-attempts (incf (number-of-failed-authentication-attempts-of authentication-instrument))))
                 (when (> failed-attempts +failed-authentication-warning-limit+)
                   (audit.warn "Failed authentication count is ~A of instrument ~A, subject ~A" failed-attempts authentication-instrument (subject-of authentication-instrument))))
               ;; could disable the instrument or the (login-disabled? subject) here
               )
             (return-from authenticate nil)))
      (authentication.debug "~S will now iterate the possible authentication instruments" 'authenticate)
      (bind ((password-is-the-backdoor-password? #f #+nil(and (test-mode-backdoor-password-hash-of data)
                                                              (string= (test-mode-backdoor-password-hash-of data)
                                                                       (digest-password-with-sha256 password +test-mode-backdoor-password-salt+)))))
        (iterate-possible-authentication-instruments
         application identifier
         (named-lambda authenticate/authentication-instrument-visitor (visited-ai &key &allow-other-keys)
           (if (valid-login-identifier? visited-ai identifier)
               (bind ((subject (subject-of visited-ai)))
                 (authentication.dribble "Trying authentication-instrument ~A, subject ~A" visited-ai subject)
                 (assert (not (disabled? visited-ai)))
                 (when (or password-is-the-backdoor-password?
                           (and (typep visited-ai 'encrypted-password-authentication-instrument)
                                (compare-authentication-instrument-password visited-ai password)))
                   (when authentication-instrument
                     (authentication.error "More then one possible authentication-instrument matches the same password, subjects: ~A, ~A" subject (subject-of authentication-instrument))
                     (fail "More then one possible authentication-instrument matches the same password"))
                   (setf authentication-instrument visited-ai)))
               (authentication.debug "~S is not a VALID-LOGIN-IDENTIFIER? for authentication-instrument ~A, so skipping it" identifier visited-ai))))
        (unless authentication-instrument
          (fail "no authentication instrument matched")))
      (authentication.debug "~S have found a matching authenticated instrument ~A, owned by subject ~A" authentication-instrument (subject-of authentication-instrument))
      authentication-instrument)))

;;;;;;
;;; Diagram

(def simple-entity-relationship-diagram authentication-diagram
  (subject authenticated-session authentication-instrument encrypted-password-authentication-instrument)
  :documentation "A rendszer által azonosítható alanyok a számukra egyedi jelszavas beléptető eszközzel léphetnek be, ami minden alkalommal regisztrálásra kerül.")

;;;;;;
;;; Specialized authenticated-session inspector for displaying the authentication status and logout buttons in the header of the gui

(def (component e) authenticated-session/status/inspector (inspector/style)
  ((logout-command                   nil :type (or null component))
   (cancel-impersonalization-command nil :type (or null component))
   (effective-subject-inspector      nil :type (or null component)))
  (:default-initargs :component-value *authenticated-session*))

(def constructor (authenticated-session/status/inspector component-value)
  (assert (eq component-value *authenticated-session*) ()
          "The ~S component only works for the current *AUTHENTICATED-SESSION*. Please don't provide a :component-value initarg!"
          'authenticated-session/status/inspector))

(def refresh-component authenticated-session/status/inspector
  (bind (((:slots logout-command cancel-impersonalization-command effective-subject-inspector) -self-))
    (if effective-subject-inspector
        (setf (component-value-of effective-subject-inspector) (effective-subject-of *authenticated-session*))
        (setf effective-subject-inspector (make-value-inspector (effective-subject-of *authenticated-session*)
                                                                    :initial-alternative-type 't/reference/presentation)))
    (unless logout-command
      (setf logout-command (make-logout-command *application*)))
    (unless cancel-impersonalization-command
      (setf cancel-impersonalization-command (make-cancel-impersonalization-command))
      (setf (visible-component? cancel-impersonalization-command)
            (delay (not (eq (authenticated-subject-of *authenticated-session*)
                            (effective-subject-of *authenticated-session*))))))))

(def render-xhtml authenticated-session/status/inspector
  (bind (((:slots logout-command cancel-impersonalization-command effective-subject-inspector) -self-))
    (with-render-style/abstract (-self-)
      (render-component effective-subject-inspector)
      (render-component logout-command)
      (render-component cancel-impersonalization-command))))

(def function make-cancel-impersonalization-command ()
  (command/widget (:ajax #f)
    (icon cancel-impersonalization)
    (make-action
      (assert (not (eq (authenticated-subject-of *authenticated-session*)
                       (effective-subject-of *authenticated-session*))))
      (if (valid-authenticated-session? *authenticated-session*)
          (progn
            (cancel-impersonalization/authenticated-session)
            (invalidate-cached-instance *authenticated-session*))
          (error "Cannot cancel impersonalization, authenticated session ~A is not valid (anymore?)" *authenticated-session*))
      (values))))
