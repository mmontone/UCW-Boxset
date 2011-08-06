;;; -*- lisp -*-


(in-package :it.bese.ucw)

(eval-when (:compile-toplevel :load-toplevel)
  (defun threaded-lisp-p ()
    #+(and sbcl sb-thread) t
    #+(and cmu mp) t
    #+openmcl t
    #+lispworks mp::*multiprocessing*
    #+(and allegro multiprocessing) t
    ))

(defun start-swank (&optional (port *ucw-swank-port*))
  #.(if (threaded-lisp-p)
        '(if (swank::default-connection)
          (warn "Swank is already running")
          (swank:create-server :dont-close t
           :port port
           :coding-system (external-format-for :slime)))
        '(warn "Can't start slime server due to lack of threads.")))

(defun ensure-system (system-name)
  (unless (asdf:find-system system-name nil)
    (error "Unable to find the system for
~S. asdf:*central-registry* is ~S.  Are the symlinks and
asdf:*central-registry* properly setup?"
           system-name
           asdf:*central-registry*)))

(defun create-server (&key
                      (backend `(,*ucw-backend-type* :host ,*ucw-backend-host*
                                 :port ,*ucw-backend-port*))
                      (applications *ucw-applications*)
                      (start-p t)
                      (server-class *ucw-server-class*)
                      log-level
                      (log-to-console-p t)
                      (log-root-directory (truename *ucw-log-root-directory*)))
  "Creates and returns a UCW server according to SERVER-CLASS, HOST and
PORT. Affects *DEFAULT-SERVER*.

BACKEND is a list of (BACKEND-TYPE &rest INITARGS). BACKEND-TYPE
may be :HTTPD, :MOD-LISP, :ASERVE, :ARANEIDA, an existing
backend, an existing UCW server backend or :DEFAULT in which case
it attempts to return a sane default from the UCW backends loaded
and available, or any other value for which a valid MAKE-BACKEND
method has been defined. INITARGS will be passed, unmodified, to
MAKE-BACKEND.

APPLICATIONS is a list of defined applications to be loaded into the
server.

Logs are generated in verbosity defined by LOG-LEVEL and directed to
LOG-ROOT-DIRECTORY if defined."

  (let ((root-logger (get-logger 'ucw))
        (ucw.backend (get-logger 'ucw.backend))
        (console-appender (make-instance 'brief-stream-log-appender
                                         :stream *debug-io*)))
    (when log-root-directory
      (setf (appenders root-logger)
            (list* (make-file-log-appender (merge-pathnames "ucw.log"
                                                            log-root-directory))
                   (when log-to-console-p
                     (list console-appender))))
      (setf (appenders ucw.backend)
            (list* (make-file-log-appender (merge-pathnames "ucw-backend.log"
                                                            log-root-directory))
                   (when log-to-console-p
                     (list console-appender))))))
  (awhen log-level
    (setf (ucw.log-level) it))
  (let ((server nil))
    (restart-case
        (when *default-server*
          (error "*DEFAULT-SERVER* already defined as ~A. Create another server anyway?" *default-server*))
      (replace ()
        :report "Replace *DEFAULT-SERVER* with a new server instance"
        (shutdown-server *default-server*)
        (setf *default-server* nil))
      (continue ()
        :report "Create an additional server"))
    (setf server (make-instance server-class))
    (unless *default-server*
      (setf *default-server* server))
    (setf (server.backend server) (apply #'make-backend backend))
    (dolist (app-form applications)
      (let ((app (eval app-form)))
        ;;since the current launch framework is aggressively searching for applications to load
        ;;check to make sure everything it found is registerable.
        (cond
          ((and app (subtypep (class-of app) 'application))
           (register-application server app)
           (ucw.backend.info "Registered application ~A created by ~S" app app-form))
          ((null app)
           (ucw.backend.warn "Ignored registering application ~S" app-form))
          (t (ucw.backend.error "Ignored registering application ~S" app-form)))))
    (when start-p
      (startup-server server))
    server))

(defgeneric make-backend (backend-spec &key &allow-other-keys)
  (:documentation "Returns a UCW server backend as requested by
the functional arguments.  BACKEND-SPEC may be :HTTPD, :MOD-LISP,
:ASERVE, :ARANEIDA, an existing backend, an existing UCW server
backend or :DEFAULT in which case it attempts to return a sane
default from the UCW backends loaded and available.")
  (:method ((backend-spec t) &rest args)
    (declare (ignore args))
    (error "~A does not specify a backend." backend-spec)))

(defmethod make-backend ((backend-spec (eql :httpd))
                         &key host port (use-threads (threaded-lisp-p)))
  (ensure-system :puri)
  (ensure-system :rfc2388-binary)
  (asdf:oos 'asdf:load-op :ucw.httpd)
  (make-instance (if use-threads
                     'ucw:multithread-httpd-backend
                     'ucw:httpd-backend)
                 :host host
                 :port port))

(defmethod make-backend ((backend-spec (eql :mod-lisp)) &key host port)
  (ensure-system :puri)
  (ensure-system :rfc2388-binary)
  (asdf:oos 'asdf:load-op :ucw.mod-lisp)
  (make-instance '#.(if (threaded-lisp-p)
                        'ucw:multithread-mod-lisp-backend
                        'ucw:mod-lisp-backend)
                 :host host
                 :port port))

(defmethod make-backend ((backend-spec (eql :iolib)) &key host port)
  (ensure-system :puri)
  (ensure-system :rfc2388-binary)
  (asdf:oos 'asdf:load-op :ucw.iolib)
  (make-instance 'iolib-backend
                 :host host
                 :port port))

(defmethod make-backend ((backend-spec (eql :single-threaded-mod-lisp)) &key host port)
  (ensure-system :puri)
  (ensure-system :rfc2388-binary)
  (asdf:oos 'asdf:load-op :ucw.mod-lisp)
  (make-instance 'ucw:mod-lisp-backend
                 :host host
                 :port port))

#+nil
(defmethod make-backend ((backend-spec (eql :aserve)) &key host port &allow-other-keys)
  (ensure-system :aserve)
  (asdf:oos 'asdf:load-op :ucw.aserve)
  (make-instance 'ucw:aserve-backend :host host :port port))

#+nil
(defmethod make-backend ((backend-spec (eql :araneida)) &key host port)
  (ensure-system :araneida)
  (asdf:oos 'asdf:load-op :ucw.araneida)
  (let ((araneida-server (make-instance 'ucw:araneida-backend
                                        :host host
                                        :port port)))
    (warn "Starting araneida with a default url of ~S.
You MUST use this hostname to access ucw."
          (default-url araneida-server ))
    araneida-server))

(defmethod make-backend ((backend-spec (eql :default)) &key host port)
  (cond
    ((find-class 'mod-lisp-backend nil)
     (make-backend :mod-lisp :host host :port port))
    ((find-class 'httpd-backend nil)
     (make-backend :httpd :host host :port port))
    #+nil((find-class 'aserve-backend nil)
     (make-backend :aserve :port port))
    #+nil((find-class 'araneida-backend nil)
     (make-backend :araneida :host host :port port))
    (t (error "No backends loaded and ready for use"))))

(defmethod make-backend ((backend-spec backend) &key host port)
  (declare (ignore host port))
  backend-spec)


;; Copyright (c) 2005 Robert Marlow
;; Copyright (c) 2005-2006 Edward Marco baringer
;; Copyright (c) 2006 Luca Capello http://luca.pca.it <luca@pca.it>
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
;;  - Neither the name of Robert Marlow, Luca Capello, Edward Marco
;;    Baringer, nor BESE, nor the names of its contributors may be
;;    used to endorse or promote products derived from this software
;;    without specific prior written permission.
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
