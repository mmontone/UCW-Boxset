;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * Global Configuration Variables

(defvar *inspect-components* nil
  "When rendering, should we put links next to components that will bring up the inspector?")

(defconstant +default-encoding-for-uri+ :utf-8
  "UTF-8 is the semi-standard encoding for URL:s
 RFC 2396 does not specify how multi-byte characters are encoded, but mentions UTF-8 as an example.
 RFC 2718 Strongly recommends UTF-8 for new URI-schemes.
 RFC 3987 The IRI (International Resource Identifier) proposed standard specifies UTF-8.
 The Javascript ECMA standard specifies that the conversion functions (EncodeURI et al.) use UTF-8,
 http://www.ecma-international.org/publications/files/ecma-st/ECMA-262.pdf
")

(defvar *external-formats* `(:url ,+default-encoding-for-uri+
                             :slime :iso-latin-1-unix
                             :http-emacsen :iso-latin-1-unix
                             :http-lispish :latin-1)

  "The external formats used for url-unescaping, slime and http(via swank) intercommunication")

(defun external-format-for (what)
  (getf ucw::*external-formats* what))

(defun (setf external-format-for) (value what)
  (setf (getf ucw::*external-formats* what) value))

(defvar *debug-on-error* (debug-only t)
  "The default, system wide, value for debug-on-error. Applications may override this.")

(defgeneric debug-on-error (application)
  (:method (app)
           "Method used when there is no current application or its debug-on-error slot is unbound."
           *debug-on-error*)
  (:documentation "Returns T if APPLICATION should attempt to debug errors (instead of just returning an error page)."))

(defgeneric (setf debug-on-error) (value app)
  (:method (value (app null))
           (setf *debug-on-error* value)))



;;;; * Default UCW configuration

;;;; This file defines a default configuration for UCW, based on
;;;; settings which should be as general as possible and at the same
;;;; time the most common.

;;;; * The default settings

;;;; General variables
(defvar *ucw-config-file* (arnesi:getenv "CONFIGFILE"))
(defvar *ucw-swank-port* 4005)

;;;; `ucw:create-server' variables
(defvar *ucw-backend-type* :httpd)
(defvar *ucw-backend-host* "0.0.0.0")
(defvar *ucw-backend-port* 8080)
(defvar *ucw-server-class* 'standard-server)

(defvar *ucw-applications-directory* (cl-fad:directory-exists-p "/etc/ucw/applications.d/"))
(defvar *ucw-systems* nil
  "The asdf systems listed here will be loaded as part of the configuration process.")
(defvar *ucw-applications* nil
  "Each element of this list is evaluated and should return an application instance.
\(A CLOS object instance evaluates to itself, so you are free to provide either app
instances or expressions evaluating to app instances.\)")

(defvar *ucw-log-root-directory* (or (arnesi:getenv "LOGROOT")
                                     (merge-pathnames #P"logs/"
                                                      (asdf:component-pathname
                                                       (asdf:find-system :ucw)))))
(defvar *ucw-log-level* +info+
  "This is the default runtime log level for the UCW loggers.")

(defvar *ucw-compile-time-log-level* #+debug +dribble+ #-debug +info+

  "UCW logger messages below this level will not be compiled into the code, so they will have no performance penalty.")

#+(or (and sbcl sb-unicode)
      (and allegro ics)
      (and clisp unicode))
(dolist (cell '((:slime        . "utf-8-unix")
                (:url          . :utf-8)
                (:http         . :utf-8)
                (:http-emacsen . :utf-8-unix)
                (:http-lispish . :utf-8)))
  (setf (external-format-for (car cell)) (cdr cell)))

;;;
;;; l10n
;;;

(defconstant +accept-language-cache-purge-size+ 1000
  "The maximum size of the cache of Accept-Language header over which the hashtable is cleared.")
(defconstant +maximum-accept-language-value-length+ 100
  "The maximum size of the Accept-Language header value that is accepted.")

(defparameter +missing-resource-css-class+ "missing-resource"
  "CSS class to use when an i18n resource could not be found. Mostly used at compile-time by UCW.")

(defparameter *js-resource-registry* (make-hash-table)
  "Contains the resource names that should be sent to the client side js")

(defparameter *js-resource-registry-last-modified* 0
  "A timestamp updated when the registry was last modified.")

(defun register-js-resource (name)
  (declare (type symbol name))
  (unless (gethash name *js-resource-registry*)
    (setf (gethash name *js-resource-registry*) t)
    (setf *js-resource-registry-last-modified* (get-universal-time))))

(defun register-js-resources (&rest names)
  (dolist (name names)
    (register-js-resource name)))

(defun unregister-js-resource (name)
  (declare (type symbol name))
  (remhash name *js-resource-registry*))

(defvar *dispatcher-registers*)

(defparameter +xhtml-namespace-uri+ "http://www.w3.org/1999/xhtml")
(defparameter +dojo-namespace-uri+ "http://www.dojotoolkit.org/2004/dojoml")

;; TODO use it from alexandria
(eval-always
  (defmacro defconstant-eqx (symbol expr eqx &optional doc)
    `(defconstant ,symbol
      (%defconstant-eqx-value ',symbol ,expr ,eqx)
      ,@(when doc (list doc))))

  (defun %defconstant-eqx-value (symbol expr eqx)
    (declare (type function eqx))
    (flet ((bummer (explanation)
             (error "~@<bad DEFCONSTANT-EQX ~S ~2I~_~S: ~2I~_~A ~S~:>"
                    symbol
                    expr
                    explanation
                    (symbol-value symbol))))
      (cond ((not (boundp symbol))
             expr)
            ((not (constantp symbol))
             (bummer "already bound as a non-constant"))
            ((not (funcall eqx (symbol-value symbol) expr))
             (with-simple-restart
                 (continue "Go ahead and change the value.")
               (bummer "already bound as a different constant value"))
             expr)
            (t
             (symbol-value symbol))))))

(macrolet ((def (&body defs)
               `(progn
                 ,@(iter (for (name value reason-phrase) :on defs :by #'cdddr)
                         ;; this is not yet needed
                         #+nil(collect `(defconstant ,(intern-concat (list (subseq (string name) 0
                                                                              (1- (length (string name))))
                                                                      "-CODE+")) ,value))
                         (collect `(defconstant-eqx ,name
                                    ,(format nil "~A ~A" value reason-phrase)
                                    #'equal))))))
  (def
    +http-continue+                        100 "Continue"
    +http-switching-protocols+             101 "Switching Protocols"
    +http-ok+                              200 "OK"
    +http-created+                         201 "Created"
    +http-accepted+                        202 "Accepted"
    +http-non-authoritative-information+   203 "Non-Authoritative Information"
    +http-no-content+                      204 "No Content"
    +http-reset-content+                   205 "Reset Content"
    +http-partial-content+                 206 "Partial Content"
    +http-multi-status+                    207 "Multi-Status"
    +http-multiple-choices+                300 "Multiple Choices"
    +http-moved-permanently+               301 "Moved Permanently"
    +http-moved-temporarily+               302 "Moved Temporarily"
    +http-see-other+                       303 "See Other"
    +http-not-modified+                    304 "Not Modified"
    +http-use-proxy+                       305 "Use Proxy"
    +http-temporary-redirect+              307 "Temporary Redirect"
    +http-bad-request+                     400 "Bad Request"
    +http-authorization-required+          401 "Authorization Required"
    +http-payment-required+                402  "Payment Required"
    +http-forbidden+                       403 "Forbidden"
    +http-not-found+                       404 "Not Found"
    +http-method-not-allowed+              405 "Method Not Allowed"
    +http-not-acceptable+                  406 "Not Acceptable"
    +http-proxy-authentication-required+   407 "Proxy Authentication Required"
    +http-request-time-out+                408 "Request Time-out"
    +http-conflict+                        409 "Conflict"
    +http-gone+                            410 "Gone"
    +http-length-required+                 411 "Length Required"
    +http-precondition-failed+             412 "Precondition Failed"
    +http-request-entity-too-large+        413 "Request Entity Too Large"
    +http-request-uri-too-large+           414 "Request-URI Too Large"
    +http-unsupported-media-type+          415 "Unsupported Media Type"
    +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable"
    +http-expectation-failed+              417 "Expectation Failed"
    +http-failed-dependency+               424 "Failed Dependency"
    +http-internal-server-error+           500 "Internal Server Error"
    +http-not-implemented+                 501 "Not Implemented"
    +http-bad-gateway+                     502 "Bad Gateway"
    +http-service-unavailable+             503 "Service Unavailable"
    +http-gateway-time-out+                504 "Gateway Time-out"
    +http-version-not-supported+           505 "Version not supported"))

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
