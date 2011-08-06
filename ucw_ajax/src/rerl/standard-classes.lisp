;;;; -*- lisp -*-

(in-package :it.bese.ucw)

(defclass application-with-tal-support-mixin (application)
  ((tal-generator :accessor application.tal-generator
                  :initarg :tal-generator
                  :documentation "A tal-generator object used to
lookup and compile tal pages for template-components.")))

(defclass application-with-www-root-support-mixin (application)
  ((www-roots :accessor application.www-roots
              :initarg :www-roots
              :initform nil
              :documentation "A list of directories (pathname
specifiers) or cons-cell (URL-subdir . pathname) to use when looking for static files."))
  (:default-initargs :www-roots (make-standard-ucw-www-root-list)))

(defclass standard-application (basic-application
                                application-with-session-handling-mixin
                                application-with-tal-support-mixin
                                application-with-www-root-support-mixin)
  ((javascript-log-level :accessor javascript-log-level
                         :initarg :javascript-log-level
                         :documentation "When nil, turn off js debug output. Otherwise use it as a dojo logging level."))
  (:default-initargs :dispatchers (make-standard-ucw-dispatchers))
  (:documentation "The default UCW application class."))

(defmethod initialize-instance :after ((self standard-application) &key &allow-other-keys)
  (setf (javascript-log-level self) (when (debug-on-error self)
                                      "debug")))

(defclass standard-session (basic-session)
  ())

(defclass standard-action (action-with-isolation-support basic-action)
  ()
  (:metaclass mopp:funcallable-standard-class))

(defclass ajax-action (standard-action)
  ((creation-time-current-form
    :initform (current-form)
    :accessor creation-time-current-form-of))
  (:metaclass mopp:funcallable-standard-class)
  (:default-initargs :make-new-frame nil :call-render nil :invocation-isolated nil)
  (:documentation "An ajax action in UCW is a raw action that renders an
XML document which is then processed by the client side js. The action
body may use yaclml tags and the WITH-XML-SYNTAX of yaclml to render
into the answer XML."))

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
