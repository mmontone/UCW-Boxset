;; -*- lisp -*-

(in-package :it.bese.ucw)

(enable-bracket-syntax)

(declaim (inline action-ajax-p))
(defun action-ajax-p (action)
  (typep action 'ajax-action))

(defmethod compute-url ((action ajax-action) (app application))
  (let ((uri (call-next-method)))
    (setf (uri.path uri) (strcat (uri.path uri) +ajax-action-dispatcher-url+))
    uri))

(defmethod handle-toplevel-condition ((application application) (error serious-condition) (action ajax-action))
  (abort-action "Internal server error"))

(defmacro handle-ajax-request ((&key (succesful-when-finishes t) &allow-other-keys) &body body)
  (with-unique-names (yaclml-body)
    `(progn
      (setf (get-header *response* "Status") +http-ok+
            (get-header *response* "Content-Type") "text/xml")
      (<:as-is #.(format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%"))
      {with-xml-syntax
        <answer
          (let ((-successp- nil) ; we leave these variables visible, which is hackish, but this is an internal macro anyway
                (-message- nil)
                (,yaclml-body))
            (unwind-protect
                 (setf ,yaclml-body
                       (with-yaclml-output-to-string
                         ,@body
                         ,(when succesful-when-finishes
                            `(setf -successp- t))))
              (if -successp-
                  (progn
                    (ucw.rerl.ajax.debug "Successfully rendered ajax answer, body length is ~S" (length ,yaclml-body))
                    (ucw.rerl.ajax.dribble "The following ajax answer body will be sent:~%~A" ,yaclml-body)
                    (<:as-is ,yaclml-body)
                    <result "success">)
                  (progn
                    (ucw.rerl.ajax.debug "Failed to render ajax answer, error message is ~S" -message-)
                    <result "failure">
                    (when -message-
                      <error-message (<:as-is :quotedp t -message-)>)))))>})))

(defmethod call-action ((action ajax-action) application session frame)
  "Wrap the ajax action's output in an XML document. The action is free to render
any valid XML body that can be processed on the client side."
  ;; TODO, attila: the encoding in the default xml header should be taken from (encoding (context.response *context*))
  ;; is there a function that converts to the appropiate format?
  (handle-ajax-request (:succesful-when-finishes nil)
    (restart-case
         (let ((swank::*sldb-quit-restart* 'abort-action))
           (call-next-method)
           (ucw.rerl.actions.debug "The body of CALL-ACTION for AJAX-ACTION was successful, calling SEND-EVENTS-TO-THE-CLIENT")
           ;; make sure we don't send partial content in case of an error
           (<:as-is
            ;; TODO there could be a more efficient construct in yaclml for this based on (setf (fill-pointer ...) ...)
            (with-yaclml-output-to-string
              (when (has-events-for-the-client session)
                (send-events-to-the-client session))))
           (setf -successp- t))
      (abort-action (&optional (failure-message "Internal server error"))
        :report "Abort processing this ajax action"
        (ucw.rerl.actions.debug "Ajax ABORT-ACTION restart invoked with FAILURE-MESSAGE ~S" failure-message)
        (if failure-message
            (setf -message- failure-message)
            (setf -successp- t))))))

(defmethod call-action :around ((action ajax-action) application session frame)
  (let ((form (creation-time-current-form-of action)))
    (if (and form
             (parent form))
        (let ((*current-form* form))
          (ucw.rerl.actions.dribble "Restored *CURRENT-FORM* to ~A from CALL-ACTION of AJAX-ACTION" *current-form*)
          (call-next-method))
        (call-next-method))))

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
