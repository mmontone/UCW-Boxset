;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** A Trivial HTTP Server

(defclass httpd-backend (basic-backend)
  ())

;; so we may get called with :host nil :port nil still meaning the default
(defmethod initialize-instance :after ((self httpd-backend) &key &allow-other-keys)
  (unless (host self)
    (setf (host self) "0.0.0.0"))
  (unless (port self)
    (setf (port self) 8080)))

(defclass httpd-message (basic-message)
  ())

(defmethod network-stream ((message httpd-message))
  (usocket:socket-stream (socket message)))

(defmethod remote-address ((message httpd-message))
  (usocket:get-peer-address (socket message)))

(defclass httpd-request (httpd-message basic-request)
  ())

(defclass httpd-response (httpd-message basic-response)
  ())

;;;; Backend methods
(defmethod startup-backend :before ((backend httpd-backend) &key &allow-other-keys)
  (loop
    (with-simple-restart (retry "Try opening the socket again on host ~S port ~S"
                                (host backend) (port backend))
      (setf (socket backend)
            (usocket:socket-listen (host backend) (port backend)
                                   :reuse-address t))
      (return))))

;;;; The single threaded server

(defmethod startup-backend ((backend httpd-backend) &rest init-args)
  "Start the RERL."
  (declare (ignore init-args))
  (let (stream
        stream-socket
        *request*
        *response*)
    (flet ((serve-one-request ()
             (unwind-protect
                  (progn
                    (setf stream-socket (usocket:socket-accept (socket backend)
                                                               :element-type '(unsigned-byte 8))
                          stream (usocket:socket-stream stream-socket)
                          *request* (read-request backend stream-socket)
                          *response* (make-response *request*))
                    (handle-request backend *request* *response*)
                    (close-request *request*))
               (ignore-errors
                 (usocket:socket-close stream-socket))))
           (handle-request-error (condition)
             (unless (typep condition 'no-handler-for-request)
               (ucw.backend.error "Error while handling a httpd backend request on socket ~S:~%~A" stream-socket condition))
             (handle-toplevel-condition nil condition nil)
             (abort-backend-request condition)))
      (unwind-protect
           (loop (call-as-backend-request-handler #'serve-one-request
                                                  :ignore-stream-errors-on stream
                                                  :error-handler #'handle-request-error))
        (ignore-errors
          (usocket:socket-close (socket backend)))
        (setf (socket backend) nil))))
  (values))

(defmethod shutdown-backend ((backend httpd-backend) &rest init-args)
  "This would stop the single therad httpd backend if that made any sense.

Stopping the single therad backend requires nothing more than
getting STARTUP-BACKEND to return (either normally or by chosing
you implementation's abort restart after a break)."
  (declare (ignore init-args))
  backend)

(defmethod read-request ((backend httpd-backend) stream-socket)
  "Reads an HTTP request message from STREAM. Returns a httpd-request object."
  (read-basic-request (make-instance 'httpd-request :socket stream-socket)))

(defmethod make-response ((request httpd-request))
  (make-instance 'httpd-response
                 :request request
                 :socket (socket request)))

;; Copyright (c) 2005-2006 Edward Marco Baringer
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
