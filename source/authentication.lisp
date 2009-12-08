;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.model)

;;;;;;
;;; Model

;; http://www.jasypt.org/howtoencryptuserpasswords.html
;; http://www.cafesoft.com/CSDigest/createDigest.do

(def constant +number-of-digest-iterations+ 1000)

(def (constant e) +password-salt-length+ 16)

(def entity encrypted-password-authentication-instrument (authentication-instrument)
  ((password :type (text 64))
   (salt (random-string +password-salt-length+) :type (text #.+password-salt-length+ #.+password-salt-length+))
   (password-expires-at :type (or null date)))
  (:documentation "A jelszavas azonosító eszköz egy a felhasználó által megadott jelszót tárol. Az eltárolt információból a jelszó nem kikövetkeztethető, de egy megadott jelszóra nézve ellenőrizhető, hogy az megegyezik-e az eredetileg megadottal."))

(def association
  ((:type (or null cluster-node-session)
    :initform (when (and (in-transaction-p)
                         *cluster-node-session*)
                (load-instance *cluster-node-session*))
    :primary #t)
   (:type (set authenticated-session))))

;;;;;;
;;; WUI customization

(def (class* e) application-with-authentication-support (application-with-login-support
                                                         application-with-perec-support)
  ())

;; KLUDGE?
(def layered-method call-in-login-entry-point-environment :around ((application application-with-authentication-support) login-encapsulation thunk)
  (hu.dwim.meta-model::with-model-database
    (hu.dwim.perec:with-transaction
      (hu.dwim.perec:with-new-compiled-query-cache
        (call-next-method)))))

(def (layered-function e) iterate-possible-authentication-instruments (application identifier visitor))

(def layered-method authenticate ((application application-with-authentication-support) unregistered-new-web-session (login-encapsulation identifier-and-password-login-encapsulation))
  (authentication.info "Logging in with authentication information ~A" login-encapsulation)
  (assert (in-transaction-p))
  (mark-transaction-for-commit-only)
  (bind ((arguments (extra-arguments-of login-encapsulation))
         ((&key allow-parallel-sessions &allow-other-keys) arguments)
         (identifier (identifier-of login-encapsulation))
         (password (password-of login-encapsulation))
         (authentication-instrument nil))
    (flet ((fail (&optional reason)
             (authentication.info "Login failed for authentication information ~A~:[.~;, reason: ~S.~]" login-encapsulation reason reason)
             (when authentication-instrument
               (bind ((failed-attempts (incf (number-of-failed-authentication-attempts-of authentication-instrument))))
                 (when (> failed-attempts +failed-authentication-warning-limit+)
                   (audit.warn "Failed authentication count is ~A of instrument ~A, subject ~A" failed-attempts authentication-instrument (subject-of authentication-instrument))))
               ;; could disable the instrument or the (login-disabled-p subject) here
               )
             (return-from authenticate nil)))
      (authentication.debug "~S will now iterate the possible authentication instruments" 'authenticate)
      (bind ((failure-reason nil))
        (setf (values authentication-instrument failure-reason) (%invoke-iterate-possible-authentication-instruments application identifier password))
        (when failure-reason
          (fail failure-reason)))
      (bind ((subject (subject-of authentication-instrument)))
        (authentication.debug "~S have a matching authenticated instrument ~A, owned by subject ~A" authentication-instrument subject)
        (when (disabled? authentication-instrument)
          (fail "authenticated instrument is disabled"))
        (setf (number-of-failed-authentication-attempts-of authentication-instrument) 0)
        (when (login-disabled-p subject)
          (fail "subject's login is disabled"))
        (bind ((authenticated-session (make-instance 'authenticated-session
                                                     :effective-subject subject
                                                     :authenticated-subject subject
                                                     :authentication-instrument authentication-instrument
                                                     :login-at (transaction-timestamp)
                                                     :http-user-agent (header-value *request* +header/user-agent+)
                                                     :web-application (human-readable-broker-path *server* application)
                                                     ;; TODO not available here yet :web-session-id
                                                     :remote-ip-address *request-remote-host*)))
          (login.info "Successful login by subject ~A using authentication-instument ~A into session ~A" subject authentication-instrument authenticated-session)
          (unless allow-parallel-sessions
            (bind ((parallel-sessions (select (session)
                                        (from (session authenticated-session))
                                        (where (and (eq (authenticated-subject-of session) subject)
                                                    (null (logout-at-of session)))))))
              ;; TODO invalidate web sessions of the parallel authenticated sessions?
              (iter (for parallel-session :in-sequence parallel-sessions)
                    (unless (eq parallel-session authenticated-session)
                      (authentication.info "Forcing logout of parallel session ~A, authenticated subject ~A" parallel-session (authenticated-subject-of parallel-session))
                      (setf (logout-at-of parallel-session) (transaction-timestamp))))))
          (if authenticated-session
              (progn
                ;; TODO?
                ;; (setf (authenticated-session-of unregistered-new-web-session) authenticated-session)
                #t)
              (progn
                ;; TODO this is subject to DOS attacks
                (audit.info "Failed authentication using identifier ~S from ip address ~A" (hu.dwim.wui::identifier-of login-encapsulation) (iolib:address-to-string *request-remote-host*))
                #f)))))))

(def function %invoke-iterate-possible-authentication-instruments (application identifier password)
  (bind ((password-is-the-backdoor-password? #f #+nil(and (test-mode-backdoor-password-hash-of data)
                                                          (string= (test-mode-backdoor-password-hash-of data)
                                                                   (digest-password-with-sha256 password +test-mode-backdoor-password-salt+))))
         (candidate-instrument nil))
    (flet ((fail (reason)
             (return-from %invoke-iterate-possible-authentication-instruments (values nil reason))))
      (iterate-possible-authentication-instruments
       application identifier
       (named-lambda authenticate/authentication-instrument-visitor (authentication-instrument &key &allow-other-keys)
         (bind ((subject (subject-of authentication-instrument)))
           (authentication.dribble "Trying authentication-instrument ~A, subject ~A at login entry point" authentication-instrument subject)
           (assert (not (disabled? authentication-instrument)))
           (when (or password-is-the-backdoor-password?
                     (and (typep authentication-instrument 'encrypted-password-authentication-instrument)
                          (compare-authentication-instrument-password authentication-instrument password)))
             (when candidate-instrument
               (authentication.error "More then one possible authentication-instrument matches the same password, subjects: ~A, ~A" subject (subject-of candidate-instrument))
               (fail "More then one possible authentication-instrument matches the same password"))
             (setf candidate-instrument authentication-instrument)))))
      (unless candidate-instrument
        (fail "no authentication instrument matched")))
    (values candidate-instrument nil)))

;;;;;;
;;; Diagram

(def simple-entity-relationship-diagram authentication-diagram
  (subject authenticated-session authentication-instrument encrypted-password-authentication-instrument)
  :documentation "A rendszer által azonosítható alanyok a számukra egyedi jelszavas beléptető eszközzel léphetnek be, ami minden alkalommal regisztrálásra kerül.")

;;;;;;
;;; Localization

(def localization en
  (class-name.encrypted-password-authentication-instrument "encrypted password authentication instrument")

  (diagram-name.authentication-diagram "authentication diagram")

  (slot-name.password "password")
  (slot-name.salt "password salf")
  (slot-name.password-expires-at "password expires at"))

(def localization hu
  (class-name.encrypted-password-authentication-instrument "jelszavas beléptető eszköz")

  (diagram-name.authentication-diagram "beléptető rendszer")

  (slot-name.password "jelszó")
  (slot-name.salt "jelszó só")
  (slot-name.password-expires-at "jelszó lejárati időpontja"))

;;;;;;
;;; Functional

(def (function e) digest-password-with-sha256 (password-string &optional salt)
  (bind ((digest (ironclad:digest-sequence :sha256 (babel:string-to-octets (if salt
                                                                               (concatenate 'string password-string salt)
                                                                               password-string)
                                                                           :encoding :utf-8))))
    (iter (repeat +number-of-digest-iterations+)
          (setf digest (ironclad:digest-sequence :sha256 digest)))
    (values (concatenate 'string
                         "{sha256,salt,1000}"
                         (cl-base64:usb8-array-to-base64-string digest))
            salt)))

(def (function e) compare-authentication-instrument-password (authentication-instrument clear-text-password)
  (check-type authentication-instrument encrypted-password-authentication-instrument)
  (bind ((digested-password (password-of authentication-instrument)))
    (assert (starts-with-subseq "{sha256,salt,1000}" digested-password))
    (string= digested-password (digest-password-with-sha256 clear-text-password (salt-of authentication-instrument)))))

(def (function e) ensure-encrypted-password-authentication-instrument (subject)
  (bind ((candidates (collect-if (of-type 'encrypted-password-authentication-instrument)
                                 (authentication-instruments-of subject))))
    (cond
      ((length= 0 candidates)
       (make-instance 'encrypted-password-authentication-instrument
                      ;; it's only a valid random dummy password hash
                      :password "{sha256,salt,1000}fjFsdg3WjGGfsd6qdjyhdggg3g3452HjkyxTk5loalo="
                      :subject subject))
      ((length= 1 candidates)
       (first-elt candidates))
      (t
       (error "Subject ~A has multiple encrypted-password-authentication-instrument's when ensure-encrypted-password-authentication-instrument was called" subject)))))

(def (function e) has-encrypted-password-authentication-instrument? (subject)
  (check-type subject subject)
  (some (of-type 'encrypted-password-authentication-instrument)
        (authentication-instruments-of subject)))

(def function random-password (&key (length 6) alphabet)
  (unless alphabet
    (setf alphabet "abcdefghijklmnopqrstuvwxyz"))
  (loop
     :with result = (make-string length)
     :with alphabet-length = (length alphabet)
     :for i :below length
     :do (setf (aref result i) (aref alphabet (random alphabet-length)))
     :finally (return result)))

(def (function e) generate-random-password (subject &key (length 6) alphabet)
  (bind ((password (random-password :length length :alphabet alphabet))
         (authentication-instrument (ensure-encrypted-password-authentication-instrument subject)))
    (update-authentication-instrument-password authentication-instrument password :expires-at (now))
    (values password authentication-instrument)))

(def (function e) update-authentication-instrument-password (authentication-instrument password &key expires-at)
  (audit.info "Current authenticated subject ~A is changing the password stored in ~A, owned by ~A"
              (when (has-authenticated-session)
                (current-authenticated-subject))
              authentication-instrument
              (subject-of authentication-instrument))
  (setf (password-expires-at-of authentication-instrument) expires-at)
  (setf (password-of authentication-instrument) (digest-password-with-sha256 password (salt-of authentication-instrument))))

#+nil ;; TODO
(
(def (function e) select-count-of-authenticated-sessions (start-timestamp end-timestamp granularity)
  (check-type granularity (member :sec :minute :hour :day :month :year))
  (bind ((truncated-start (timestamp-truncate start-timestamp :unit granularity))
         (truncated-end (timestamp-truncate end-timestamp :unit granularity))
         (count-of-units (1+ (timestamp-difference truncated-end truncated-start :unit granularity)))
         (sessions (select ((login-at-of session) (logout-at-of session))
                     (from (session authenticated-session))
                     (where (and (timestamp>= end-timestamp (login-at-of session))
                                 (timestamp>= (logout-at-of session) start-timestamp)))
                     (order-by :ascending (login-at-of session))))
         (counts (make-array count-of-units :element-type 'integer :initial-element 0)))
    (iter (for i index-of-vector counts)
          (for timestamp =
               (bind (((:values nsec sec day timezone)
                       (local-time::%offset-timestamp-part truncated-start granularity i)))
                 (make-timestamp :day day :sec sec :nsec nsec :timezone timezone)))
          (setf (aref counts i)
                (vector timestamp 0)))
    (iter (for session in sessions)
          (for start-index = (max 0 (timestamp-difference (first session) truncated-start
                                                           :unit granularity)))
          (for end-index = (min (1- count-of-units) (timestamp-difference (second session) truncated-start
                                                                           :unit granularity)))
          (iter (for i from start-index to end-index)
                (incf (aref (aref counts i) 1))))
    counts))

;;; TODO: move these to local-time, check all call sites, too!
(def function timestamp-difference (time-a time-b &key (unit :year))
  "Returns the difference between the two timestamp in the given UNITS. The result is always an integer."
  (declare (type timestamp time-b time-a))
  (let ((time-b (local-time::adjust-to-timezone time-b (timezone-of time-a) (make-timestamp))))
    (ecase unit
      ((:year :month)
       (multiple-value-bind (year-a month-a) (local-time::timestamp-decode-date time-a)
         (multiple-value-bind (year-b month-b) (local-time::timestamp-decode-date time-b)
           (let ((year (- year-a year-b))
                 (month (- month-a month-b)))
             (case unit
               (:year year)
               (:month (+ (* 12 year) month)))))))
      ((:day :hour :minute :sec)
       (let ((second (- (sec-of time-a) (sec-of time-b)))
             (day (- (day-of time-a) (day-of time-b))))
         (case unit
           (:week (truncate day +days-per-week+))
           (:day day)
           (:hour (+ (truncate second +seconds-per-hour+) (* day +hours-per-day+)))
           (:minute (+ (truncate second +seconds-per-minute+) (* day +minutes-per-day+)))
           (:sec (+ second (* day +seconds-per-day+)))))))))

(def function timestamp-truncate (time &key (unit :year))
  "Truncates TIME to the specified UNIT by clearing its smaller fields."
  (declare (type timestamp time))
  (multiple-value-bind (nsec sec minute hour day month year day-of-week)
      (decode-timestamp time)
    (declare (ignore nsec day-of-week))
    (ecase unit
      (:year (encode-timestamp 0 0 0 0 1 1 year :offset 0))
      (:month (encode-timestamp 0 0 0 0 1 month year :offset 0))
      (:day (encode-timestamp 0 0 0 0 day month year :offset 0))
      (:hour (encode-timestamp 0 0 0 hour day month year :offset 0))
      (:minute (encode-timestamp 0 0 minute hour day month year :offset 0))
      (:sec (encode-timestamp 0 sec minute hour day month year :offset 0)))))
)
