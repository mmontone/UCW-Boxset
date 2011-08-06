;;;; -*- lisp -*-

(in-package :it.bese.ucw-user)

;;;; the file uploading example and test

(defclass file-upload-example (widget-component template-component)
  ((file :accessor file-upload-example.file :initform ""))
  (:metaclass standard-component-class)
  (:default-initargs :template-name "upload.tal")
  (:documentation "Form for uploading a file."))

(defmethod/cc upload ((self file-upload-example))
  (call 'file-upload-viewer :file-data (file-upload-example.file self)))

(defclass file-upload-viewer (widget-component)
  ((file-data :initarg :file-data))
  (:metaclass standard-component-class)
  (:documentation "View a file uplodaed by the file-upload-example component."))

(defmethod render ((viewer file-upload-viewer))
  (<:table
   (<:tr (<:th "Header") (<:th "Value"))
   (dolist* ((name . value) (mime-part-headers (slot-value viewer 'file-data)))
     (<:tr
      (<:td (<:as-html name)) (<:td (<:as-html value)))))
  (<:p "Body:")
  (<:pre (<:as-html (mime-part-body (slot-value viewer 'file-data))))
  (<ucw:a :action-body (ok viewer) "Ok."))

;;;; web form example

(defcomponent example-form (form-component-mixin)
  ((abandon-handler-select-field
    :accessor abandon-handler-select-field
    :initform (make-instance 'alist-select-field
                             :data-set '((nil . "Do nothing")
                                         (:ask-user . "Ask user")
                                         (:auto-submit . "Autosubmit"))))
   (string-input :accessor string-input
                 :initform (make-instance 'string-field
                                          :input-size 18
                                          :validators (list
                                                       (make-instance 'length-validator
                                                                      :min-length 4
                                                                      :max-length 18))))
   (password-input :accessor password-input
                   :initform (make-instance 'password-field :input-size 12))
   (number-input :accessor number-input
                 :initform (make-instance 'number-field :input-size 4))
   (limited-number-input :accessor limited-number-input
                         :initform (make-instance 'number-field :input-size 4
                                                  :validators (list (make-instance 'number-range-validator :min-value 13/3 :max-value 7.92))))
   
   (integer-input :accessor integer-input
                  :initform (make-instance 'integer-field
                                           :input-size 4
                                           :validators (list (make-instance 'not-empty-validator))))
   (textarea-input :accessor textarea-input
                   :initform (make-instance 'textarea-field :rows 2))
   (date-input :accessor date-input
               :initform (make-instance 'dmy-date-field))
   (dojo-date-input :accessor dojo-date-input-of
                    :initform (make-instance 'dojo-dropdown-date-picker :value (now)
                                             :validators (list (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p timezone)
                                                                   (decode-local-time (now))
                                                                 (declare (ignore usec sec min hour day-of-week daylight-saving-time-p))
                                                                 (make-instance 'time-range-validator
                                                                                :min-value (encode-local-time 0 0 0 0 (- day 10) month year :timezone timezone)
                                                                                :max-value (encode-local-time 0 59 59 23 (+ day 10) month year :timezone timezone))))))
   (dojo-time-input :accessor dojo-time-input-of
                    :initform (make-instance 'dojo-dropdown-time-picker :value (now)))
   (dojo-timestamp-input :accessor dojo-timestamp-input-of
                         :initform (make-instance 'dojo-timestamp-picker :value (now)))
   (submit-button :accessor submit-button
                  :component (submit-button :label "Submit"))
   (messages :accessor messages :initform '())
   (inner-form :accessor inner-form-of :component (inner-example-form))
   (inline-edit-box
    :initform (make-instance 'dojo-inline-edit-box
                             :value "Click to edit. When saved, the value is posted to the server using JS-TO-LISP-RPC independently from the surrounding form if there's any at all.")
    :accessor inline-edit-box-of))
  (:default-initargs :default-action (make-action-body (:with-call/cc t)
                                       (refresh-component self))))

(defcomponent inner-example-form (form-component-mixin)
  ((string-input :accessor string-input
                 :initform (make-instance 'string-field
                                          :value "initial value"))
   (submit-button :accessor submit-button
                  :component (submit-button :label "Submit"))))

(defmethod render ((self inner-example-form))
  (<:p (<:as-html "An inner form, current value is: \"" (value (string-input self)) "\""))
  (<:p (render (string-input self))
       (render (submit-button self))
       (<ucw:button :action (register-ajax-action ()
                              (with-call/cc
                                (refresh-component self))
                              (mark-dirty self))
                    :default t
                    :progress-label "Submitting the inner form"
                    "Submit inner with AJAX")))

(defmethod shared-initialize :after ((form example-form) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (setf (value (abandon-handler-select-field form)) (ucw::abandon-handler-of form))
  ;; install an onchange handler factory that simply validates the form and rerenders it
  ;; (and therefore applies the freshly selected abandon handler)
  (setf (ucw::on-change-action-factory-of (abandon-handler-select-field form))
        (lambda ()
          (values (register-ajax-action ()
                    (with-call/cc
                      (refresh-component form))
                    (mark-dirty form))
                  #.(with-yaclml-output-to-string
                      (<:as-html "Submitting the new " (<:b "abandon policy"))))))
  (push (make-instance 'string=-validator :other-field (string-input form))
        (validators (password-input form))))

(defmethod/cc refresh-component :after ((form example-form))
  (sleep 1) ; so that we can demo the progress labels
  (setf (messages form) '())
  (flet ((push-message-unless-valid (field name)
           (multiple-value-bind (validp failed-validators)
               (validp field)
             (unless validp
               (push (format nil "~A failed~{ ~A~} check~P."
                             name (mapcar (compose #'class-name #'class-of)
                                          failed-validators)
                             (length failed-validators))
                     (messages form))))))
    (setf (ucw::abandon-handler-of form) (value (abandon-handler-select-field form)))
    (push-message-unless-valid (string-input form) "First string field")
    (push-message-unless-valid (password-input form) "Password field")
    (push-message-unless-valid (number-input form) "First number field")
    (push-message-unless-valid (limited-number-input form) "Second number field (range)")
    (push-message-unless-valid (integer-input form) "First integer field")
    (push-message-unless-valid (textarea-input form) "Second string field")
    (push-message-unless-valid (dojo-date-input-of form) "Dojo date field")
    (push-message-unless-valid (dojo-time-input-of form) "Dojo time field")
    (push-message-unless-valid (dojo-timestamp-input-of form) "Dojo timestamp field")
    (setf (messages form) (nreverse (messages form)))))

(defmethod render ((form example-form))
  (<:p :style "font-size: smaller"
       "This example demonstrates UCW's form features. The outer form has client side validators on the fields, and
it can bring up a warning or automatically post the form data based on the
value of the 'Abandon policy' field. When this field changes the form is ajax-submitted, validated on
the server side and rerendered with AJAX (in the process the selected new abandon handler is set).")

  (<:p :style "font-size: smaller"
       "When ajax-render'ing or rendering a nested form, UCW automatically renders the form(s) using DIV's and
keeps track of the form fields using js. This is needed because nesting <:form tags is forbidden.")

  (when (messages form)
    (<:ul
     (dolist (message (messages form))
       (<:li (<:b (<:as-html message))))))
  (<:table
   (<:tr
    (<:th "Label")
    (<:th "")
    (<:th "Constraints"))
   (<:tr
    (<:td "Abandon policy")
    (<:td (render (abandon-handler-select-field form)))
    (<:td "Selects what to do when a changed form is being abandoned."))
   (<:tr
    (<:td "String")
    (<:td (render (string-input form)))
    (<:td "Must be between 4 and 18 characters."))
   (<:tr
    (<:td "Password")
    (<:td (render (password-input form)))
    (<:td "Must be equal to the string field above."))
   (<:tr
    (<:td "Number")
    (<:td (render (number-input form)))
    (<:td "Any number (try fractions!)"))
   (<:tr
    (<:td "Number in a range")
    (<:td (render (limited-number-input form)))
    (<:td "Must be between 13/3 and 7.92."))
   (<:tr
    (<:td "Integer")
    (<:td (render (integer-input form)))
    (<:td "Required field."))
   (<:tr
    (<:td "Another string")
    (<:td (render (textarea-input form)))
    (<:td "No constraints."))
   (<:tr
    (<:td "A date")
    (<:td (render (date-input form)) "(dd/mm/yyyy)")
    (<:td "No constraints."))
   (<:tr
    (<:td "A dojo date")
    (<:td (render (dojo-date-input-of form)))
    (<:td "Must be between today and +/- 10 days"))
   (<:tr
    (<:td "A dojo time")
    (<:td (render (dojo-time-input-of form)))
    (<:td "No constraints."))
   (<:tr
    (<:td "A dojo timestamp")
    (<:td (render (dojo-timestamp-input-of form)))
    (<:td "No constraints."))
   (<:tr
    (<:td :style "border: 1px solid #000066;" :colspan 3
          (render (inner-form-of form))))
   (<:tr
    (<:td :colspan 3 :align "center"
          (render (submit-button form))
          (<ucw:button :action (register-ajax-action ()
                                 (with-call/cc
                                   (refresh-component form))
                                 (mark-dirty form))
                       ;; demonstrate custom progress labels, even a styled kind
                       :progress-label #.(with-yaclml-output-to-string
                                           (<:as-html "Submitting the "
                                                      (<:i "form")
                                                      " example"))
                       "Submit outer with AJAX")
          (<ucw:button :action (register-ajax-action ()
                                 (with-call/cc
                                   (refresh-component form))
                                 (mark-dirty form))
                       :forms-to-submit (list (dom-id form)
                                              (dom-id (inner-form-of form)))
                       ;; please note that :default t hijacks the simple
                       ;; submit button due to the default onsubmit handler
                       ;; of the form
                       :default t
                       ;; demonstrate custom progress labels, even a styled kind
                       :progress-label "Submitting both forms"
                       "Submit both with AJAX"))))
  (render (inline-edit-box-of form)))

;;;; using the web form example to create a dynamic form

(defcomponent dynamic-form (form-component-mixin)
  ((select-field :accessor select-field
                 :initform (make-instance 'select-field
                                          :data-set (list 'string-field 'textarea-field 'integer-field)))
   (fields :accessor fields :initform '())))

(defmethod/cc add-field ((f dynamic-form))
  (when (value (select-field f))
    (push (make-instance (value (select-field f))) (fields f))))

(defmethod/cc delete-field ((f dynamic-form) field)
  (setf (fields f) (delete field (fields f))))

(defmethod render ((form dynamic-form))
  (<:table
   (<:tr
    (<:td "Add new field of type")
    (<:td :colspan 2 (render (select-field form)))
    (<:td (<ucw:submit :action-body (add-field form)
                       :value "Add")))
   (dolist* (field (fields form))
     (<:tr
      (<:td (<:as-html (class-name (class-of field))) ":")
      (<:td (render field))
      (<:td (inspect-anchor form (value field)))
      (<:td (<ucw:input :type "submit"
                        :action-body (delete-field form field)
                        :value "Delete"))))
   (<:tr
    (<:td :colspan 4 (<ucw:submit :action-body (refresh-component form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
