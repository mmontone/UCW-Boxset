;; See the file LICENCE for licence information.
(in-package :ucw)

(enable-sharpquote<>-syntax)
(enable-bracket-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dirty-component-tracking

(defclass dirty-component-tracking-application-mixin ()
  ()
  (:documentation "Application mixin that enables the tracking of dirty components."))

(defclass dirty-component-tracking-session (standard-session)
  ((dirty-components :initform (make-hash-table :weakness :key :test #'eq)
                     :accessor dirty-components-of)))

(defmethod session-class list ((app dirty-component-tracking-application-mixin))
  'dirty-component-tracking-session)

(defmethod has-events-for-the-client ((session dirty-component-tracking-session))
  (or (call-next-method)
      (iterate-visible-dirty-components
       (lambda (component)
         (declare (ignore component))
         (return-from has-events-for-the-client t)))))

;;;;;;;;;;;;;;;;;;;
;; ajax-application

(defclass ajax-application-mixin (dirty-component-tracking-application-mixin)
  ())

(defclass ajax-session (dirty-component-tracking-session standard-session)
  ((event-condition-variable :accessor event-condition-variable-of
                             :initform (when *supports-threads-p*
                                         (make-condition-variable)))
   (latest-polling-thread :accessor latest-polling-thread-of :initform nil)))

(defmethod session-class list ((app ajax-application-mixin))
  'ajax-session)

(defgeneric notify-session-event (session)
  (:method ((session ajax-session))
    (when *supports-threads-p*
      (ucw.rerl.ajax.debug "notify-session-event for session ~S in thread ~S" session (thread-name (current-thread)))
      (condition-notify (event-condition-variable-of session)))))

(defgeneric wait-for-session-event (session)
  (:method ((session standard-session))
    (when *supports-threads-p*
      (ucw.rerl.ajax.debug "wait-for-session-event for session ~S, in thread ~S" session (thread-name (current-thread)))
      (condition-wait (event-condition-variable-of session) (lock-of session)))))

(defmethod send-events-to-the-client ((session ajax-session))
  (ajax-render-dirty-components))


;;;
;;; Dirty stuff
;;;
(defvar %disable-dirtyness-tracking%)

(defun register-dirty-component (component)
  (unless (boundp '%disable-dirtyness-tracking%)
    (let ((session (session-of component)))
      (when (typep session 'dirty-component-tracking-session)
        (let ((table (dirty-components-of session)))
          ;; TODO portable assert that we have the lock on the session
          #+sbcl (assert (eq (sb-thread::mutex-value (lock-of session)) (current-thread)) ()
                         "You must have a lock on the session while registering dirty components in it")
          (when table
            (ucw.rerl.ajax.debug "About to register dirty component ~S, the session has ~S dirty components currently"
                                 component (hash-table-count table))
            (setf (gethash component table) t)
            (notify-session-event session)))))))

(defun unregister-dirty-component (component)
  (let ((session (session-of component)))
    (when (typep session 'dirty-component-tracking-session)
      (let ((table (dirty-components-of session)))
        ;; TODO portable assert that we have the lock on the session
        #+sbcl (assert (eq (sb-thread::mutex-value (lock-of session)) (current-thread)) ()
                       "You must have a lock on the session while unregistering dirty components from it")
        (when table
          (remhash component table)
          (ucw.rerl.ajax.debug "Component ~S is not dirty anymore, the session has ~S dirty components currently"
                               component (hash-table-count table)))))))

(defmacro without-dirtyness-tracking (&body body)
  "Disable dirtyness tracking. IOW, register-dirty-component will have no effects
while in the dynamic scope of without-dirtyness-tracking."
  `(let ((%disable-dirtyness-tracking% t))
    ,@body))

(defun mark-dirty (component)
  "It's a (setf (dirtyp component) t) inside a with-lock-held-on-session for convenience."
  (with-lock-held-on-session (session-of component)
    (setf (dirtyp component) t)))

(defun iterate-visible-dirty-components (visitor)
  (ucw.rerl.ajax.dribble "iterate-visible-dirty-components entered with visitor ~S" visitor)
  (when-bind table (dirty-components-of (context.session *context*))
    (let ((components (hash-table-keys table)))
      (ucw.rerl.ajax.dribble "List of dirty components before collecting ~S" components)
      (setf components (iter (for component in components)
                             (ucw.rerl.ajax.dribble "Checking component ~S" component)
                             (unless (dirtyp component)
                               (ucw.rerl.ajax.dribble "Component ~S is not dirty anymore, unregistering" component)
                               (unregister-dirty-component component)
                               (next-iteration))
                             (when (and (typep component 'ajax-component-mixin)
                                        (not (has-ever-been-rendered-p component)))
                               (ucw.rerl.ajax.dribble "The ajax component ~S has not been rendered, skipping it" component)
                               (next-iteration))
                             (for (values visiblep distance) = (visiblep component))
                             (if visiblep
                                 (collect (cons component distance))
                                 (ucw.rerl.ajax.dribble "Component ~S is not visible, dropping from the list" component))))
      (ucw.rerl.ajax.dribble "List of dirty components before sorting ~S" components)
      (setf components (sort components #'< :key #'cdr))
      (ucw.rerl.ajax.dribble "List of dirty components after sorting ~S" components)
      (iter (for (component . nil) in components)
            (ucw.rerl.ajax.debug "iterate-visible-dirty-components visiting component ~S, dirtyp? ~S"
                                 component (dirtyp component))
            ;; need to check for dirtyness again, it might have been rendered meanwhile
            (when (dirtyp component)
              (ucw.rerl.ajax.debug "iterate-visible-dirty-components calling visitor with component ~S" component)
              (funcall visitor component))))))

(defmethod expire-session :after ((session ajax-session))
  ;; abort the client poller, if there's any
  (setf (latest-polling-thread-of session) nil)
  (notify-session-event session))

(defvar *default-polling-delay* 3000
  "The default delay in ms to wait before the client connects the server again for new events.")
(defvar *max-number-of-live-polling-connections* 30
  "While there are less then this many polling connections, they are blocked on the server and woke up when an event is available.")
(defparameter *current-number-of-live-polling-connections* 0)

(defun calculate-client-polling-delay ()
  (if (and *supports-threads-p*
           (< *current-number-of-live-polling-connections*
              *max-number-of-live-polling-connections*))
      0
      *default-polling-delay*))

(defgeneric handle-polling-of-session (application session frame)
  (:documentation "Called by the polling-dispatcher. SESSION and FRAME may be nil
when a polling request was received in an unknown session.")
  (:method ((application ajax-application-mixin) session frame)
    (if (and session frame)
        (with-lock-held-on-session session
          (ucw.rerl.ajax.debug "handle-polling-of-session entered while there are ~S alive pollers"
                               *current-number-of-live-polling-connections*)
          (incf *current-number-of-live-polling-connections*)
          (unwind-protect
               (progn
                 (let ((current-thread (current-thread)))
                   (setf (latest-polling-thread-of session) current-thread)
                   (notify-session-event session) ; wake up any previous pollers to make them quit
                   (when (and *supports-threads-p*
                              (not (has-events-for-the-client session)))
                     (ucw.rerl.ajax.debug "client-polling-handler got nil from has-events-for-the-client, falling asleep")
                     (loop named waiting do
                           (wait-for-session-event session) ; we release the session lock and wait for a notification
                           (unless (eq (latest-polling-thread-of session)
                                       current-thread)
                             (ucw.rerl.ajax.debug "client-poller aborting because there's a newer polling thread")
                             (notify-session-event session) ; wake up any other (possible poller) threads waiting
                             (return-from handle-polling-of-session))
                           (when (has-events-for-the-client session)
                             (return-from waiting))))
                   (ucw.rerl.ajax.debug "client-poller woke up, sending back the ajax answer")
                   ;; go through the public protocol, so transactions and stuff is alive while serving polling requests
                   (handle-action (make-action
                                   (lambda ()
                                     (<ucw:script :toplevelp t
                                                  `(ucw.io.polling.set-delay ,(calculate-client-polling-delay))))
                                   :class 'ajax-action)
                                  application session frame)))
            (decf *current-number-of-live-polling-connections*)))
        (handle-action (make-action
                        (lambda ()
                          (<ucw:script :toplevelp t
                                       `(progn
                                         (ucw.io.polling.stop)
                                         (ucw.load-relative-url ,(application.url-prefix application)))))
                        :class 'ajax-action)
                                  application session frame))))

(defmacro js-to-lisp-rpc ((&rest options &key
                           (invocation-isolated nil)
                           (sync 'true)
                           handler
                           &allow-other-keys)
                          args
                          values
                          &body body)
  "This macro generates the necessary js to call unnamed server Remote Procedure Calls in parenscript bodies.
WARNING: Due to a current limitation in dojo, the return value is not propagated back to the js side."
  (assert (not (find '&optional args)) () "Only mandatory and keyword arguments are supported in JS-TO-LISP-RPC")
  (let ((action-args nil))
    (push* action-args invocation-isolated :invocation-isolated)
    (remf-keywords options :invocation-isolated :sync :handler)
    ``((lambda ()
         (let ((evaluated-js-values (array ,@',(mapcar (lambda (arg)
                                                           (if (keywordp arg)
                                                               (string-downcase arg)
                                                               arg))
                                                         values))))
            (log.debug "Triggering server RPC with values " evaluated-js-values)
            (return
              (ucw.io.execute-ajax-action
               (create
                :url ,(action-href
                       (register-ajax-action (,@action-args)
                         ,(with-unique-names (lisp-result compiled-js)
                            `(let* ((,lisp-result
                                     ,(if args
                                          `(with-request-params (values) *request*
                                            (ucw.rerl.ajax.debug "Calling unnamed server RPC with js values: ~S" values)
                                            (destructuring-bind ,args (ensure-list values)
                                              ,@body))
                                          `(progn
                                            (ucw.rerl.ajax.debug "Calling unnamed, arg-less server RPC")
                                            ,@body)))
                                    (,compiled-js (js:js-to-statement-strings
                                                   (js:js-compile ,lisp-result)
                                                   0)))
                              (assert (= (length ,compiled-js) 1) () "A JS-TO-LISP-RPC body returned with something that compiled into multiple js statements")
                              {with-xml-syntax
                                <return-value
                                  (<:as-html (first ,compiled-js))>}))))
                :sync ,',sync
                :handler ,,(if handler
                               handler
                               ``(lambda (type data event)
                                  (ucw.io.process-ajax-answer type data event)
                                  (let ((return-value-node (aref (data.get-elements-by-tag-name "return-value") 0))
                                        (return-value-string return-value-node.first-child))
                                    (log.debug "Return value (as string) is " return-value-string ", as node " return-value-node)
                                    (return (eval return-value-string)))))
                :content (create :values evaluated-js-values)
                ,,@options))))))))

(defmacro js-to-lisp-rpc* (options args &body body)
  "Just like js-to-lisp-rpc but the arguments are transferred to lisp as-is. IOW, you can't give a js expression
that evaluates to the argument, but rather the named js variable will be visible on the lisp side."
  `(js-to-lisp-rpc ,options ,args ,args ,@body))
