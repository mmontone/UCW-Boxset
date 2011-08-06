;; -*- lisp -*-

(in-package :it.bese.ucw)

(defmethod (setf application.dispatchers) :around (dispatchers (app basic-application))
  (call-next-method (sort (copy-list dispatchers)
                          #'> :key #'priority)
                    app))

(defun register-dispatcher (application dispatcher)
  (setf (application.dispatchers application)
        (append (ensure-list dispatcher) (application.dispatchers application))))

(defmethod debug-on-error :around ((app basic-application))
  (if (slot-boundp app 'debug-on-error)
      (call-next-method)
      (debug-on-error nil)))

(defmethod request-context-class list ((app minimal-application))
  'standard-request-context)

(defmethod shared-initialize :after ((app minimal-application) slot-names &key &allow-other-keys)
  (setf (request-context-class-of app)
        (make-instance 'standard-class
                       :direct-superclasses (mapcar #'find-class (request-context-class app))
                       :name (intern-concat (list (class-name-of app) "-REQUEST-CONTEXT")))))

(defmethod shared-initialize :after ((app basic-application) slot-names &key &allow-other-keys)
  ;; trigger the accessor to ensure it's sorted
  (setf (application.dispatchers app) (application.dispatchers app)))

(defmethod shared-initialize :after ((app application-with-session-handling-mixin) slot-names &key &allow-other-keys)
  (setf (session-class-of app)
        (make-instance 'standard-class
                       :direct-superclasses (mapcar #'find-class (session-class app))
                       :name (intern-concat (list (class-name-of app) "-SESSION")))))

(defmethod make-request-context ((app minimal-application)
                                 (request request)
                                 (response response))
  (make-instance (request-context-class-of app)
                 :request request
                 :response response
                 :application app))

(defmethod find-session ((app application-with-session-handling-mixin) (context request-context))
  "Returns the session with ID (find-session-id CONTEXT) in APP,
NIL if there is no session with that id."
  (when-bind session-id (find-session-id context)
    (when-bind session (gethash session-id (application.session-table app))
      (if (expiredp session)
          (delete-session app session)
          (return-from find-session session))))
  nil)

(defmethod session-class list ((app basic-application))
  'basic-session)

(defmethod make-new-session ((app application-with-session-handling-mixin))
  "Returns a new session object.

The slot SESSION-TYPE controls the class of session created."
  (ucw.rerl.application.dribble "Trying to make a new session.")
  (let ((session-table (application.session-table app)))
    (when (> (hash-table-count session-table)
             *maximum-number-of-sessions*)
      (error 'too-many-sessions))
    (let ((new-session (make-instance (session-class-of app))))
      (setf (session.id new-session)
            (insert-with-new-key session-table
                                 +session-id-length+
                                 new-session))
      (ucw.rerl.application.dribble "New Session id ~S." (session.id new-session))
      new-session)))

(defmethod remove-expired-sessions ((app application-with-session-handling-mixin))
  "Loops over all the sessions in APP, calls EXPIRE-SESSION on
those for which EXPIREDP returns T. Then drops them from the
APP's session-table hash."
  (setf (last-session-purge-time-of app) (get-universal-time))
  (iter (for (session-id session) :in-hashtable (application.session-table app))
        (when (expiredp session)
          (ucw.rerl.application.dribble "Removing expired session ~S." session)
          (expire-session session)
          (remhash session-id (application.session-table app)))))

(defmethod delete-session ((app application-with-session-handling-mixin) (session session) &optional (expire t))
  "Remove SESSION from the set of known sessions. When EXPIRE is
  true the expire-session method will be called on SESSION
  before removing it."
  (with-lock-held-on-session session
    (when expire
      (expire-session session))
    (remhash (session.id session) (application.session-table app)))
  session)

(defmethod ensure-session ((app application-with-session-handling-mixin)
                           (context standard-request-context)
                           &optional (session (or (find-session app context)
                                                  (make-new-session app))))
  "If CONTEXT's request specifies a session then put it in the
  context, otherwise create a new context.

Updates the session's last-access time."
  (unless (eq (context.session context) session)
    (setf (context.session context) session))
  (setf (session.last-access (context.session context)) (get-universal-time))
  session)

(defun iterate-sessions-with-lock-held (app visitor)
  ;; one way to make it safe from deadlocks is to lock the app first and keep it
  ;; locked until all sessions are visited
  (prog2
      (ucw.rerl.ajax.dribble "Entering iterate-sessions-with-lock-held for app ~S in thread ~S" app (current-thread))
      (with-recursive-lock-held ((lock-of app))
        (iter (for (nil session) in-hashtable (application.session-table app))
              (with-recursive-lock-held ((lock-of session))
                (funcall visitor session))))
    (ucw.rerl.ajax.dribble "Leaving iterate-sessions-with-lock-held for app ~S in thread ~S" app (current-thread))))

(defmethod service :before ((app application-with-session-handling-mixin) context)
  (unless (> (- (get-universal-time)
                (last-session-purge-time-of app))
             (session-purge-period-of app))
    (remove-expired-sessions app)))

(defmethod service ((app basic-application) (context request-context))
  "Service a request for this application.

The CONTEXT is inspected and session is either created (if an
existing one can not be found), or retrieved form the
application's session table. The session is created by
make-new-session and is passed to SERVICE. CONTEXT is updated to
contain the session object."
  (let ((response (context.response context)))
    (call-as-response-handler
     response
     (lambda ()
       (dolist (dispatcher (application.dispatchers app) nil)
         (when (dispatch dispatcher app context)
           (ucw.rerl.dispatcher.dribble "~S handled the request, sending response and returning t from app service" dispatcher)
           (send-response response)
           (return-from service t))))
     :send-response nil)))

(defprint-object (app application :identity nil)
  (format *standard-output* "~A ~S"
          (application.url-prefix app)
          (hash-table-count (application.session-table app))))

(defmethod startup-application ((app application))
  ;; nop
  )

(defmethod startup-application :before ((app application-with-session-handling-mixin))
  "Simply clears out the app's session-table."
  ;; make sure, just in case...
  (clrhash (application.session-table app)))

(defmethod shutdown-application ((app application))
  ;; nop
  )

(defmethod shutdown-application :before ((app application-with-session-handling-mixin))
  (loop
      for session being the hash-values of (application.session-table app)
      do (delete-session app session t)))

(defmethod restart-application ((app basic-application))
  "Calls shutdown-application and then startup-application on
APP."
  (shutdown-application app)
  (startup-application app))

(defmethod compute-url ((action action) (app basic-application))
  "Creates the default url for APP which, when requested, will
cause the action to be called.

The generated URL's path will be the app's url-prefix."
  (let ((query (list (cons +action-parameter-name+
                           (action-id action))))
        (path (application.url-prefix app)))
    (make-uri :path path :query query)))

(defmethod compute-url :around (action (app application-with-session-handling-mixin))
  "Appends the session and frame parameters UCW needs to
find the session and the frame."
  (let ((uri (call-next-method)))
    (add-query-parameter-to-uri uri
                                +session-parameter-name+
                                (session.id (context.session *context*)))
    (add-query-parameter-to-uri uri
                                +frame-parameter-name+
                                (frame.id (context.current-frame *context*)))
    uri))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
