(in-package :ucw)

(defcomponent user-login (html-form)
  ((username :accessor username
             :initarg :username
             :initform (make-instance 'string-field
                                      :input-size 10
                                      :validators (list (make-instance 'not-empty-validator)))
             :documentation "User login name.")
   (password :accessor password
             :initarg :password
             :initform (make-instance 'password-field
                                      :input-size 10
                                      :validators (list (make-instance 'not-empty-validator)))
             :documentation "User password."))
  (:documentation "User login component."))

(defcomponent user-login-window (standard-window-component
				 status-bar-mixin)
  ((user-login :initarg :user-login
			:accessor user-login
			:component user-login))
  (:documentation "A container window componenet for the user-login.")
  (:default-initargs :title "User Login" :stylesheet "/ucw/ucw.css"))

(defmethod render ((self user-login))
  (<:table
   (<:tr
    (<:td "Username")
    (<:td (render (username self))))
   (<:tr
    (<:td "Password")
    (<:td (render (password self))))
   (<:tr
    (<:td :colspan 2
          (<ucw:input :type "submit" :action-body (submit self) :value "Ok")
          (<ucw:input :type "submit" :action-body (cancel self) :value "Cancel")))))

(defmethod render-html-body ((self user-login-window))
  (render (status-bar self))
  (render (user-login self)))

(defmethod render :before ((self user-login-window))
  (show-message "Please enter login and password."))

(defmethod report-error ((self user-login) slot-name condition)
  (show-message (format nil "~a - ~a~%"
                            (label (slot-value self slot-name)) condition)
                    :severity :error))

(defmethod/cc cancel ((self user-login))
  (answer-component (parent self) nil))

(defmethod/cc submit ((self user-login))
  (when (validp self)
      (aif (check-credentials self)
           (answer-component (parent self) it)
           (show-message "Bad login or password." :severity :error))))

(defmethod check-credentials ((self user-login))
  (let* ((username (value (username self)))
         (password (value (password self)))
         (user (find-application-user username)))
    (when (and user (check-user-password user password))
      user)))
