;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * The Standard Loggers

(deflogger ucw ()
  :level *ucw-log-level*
  :compile-time-level *ucw-compile-time-log-level*
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))

(deflogger ucw.rerl (ucw))
(deflogger ucw.rerl.actions (ucw.rerl))
(deflogger ucw.rerl.threads (ucw.rerl))
(deflogger ucw.rerl.server (ucw.rerl))
(deflogger ucw.rerl.application (ucw.rerl))
(deflogger ucw.rerl.session (ucw.rerl))
(deflogger ucw.rerl.session-frame (ucw.rerl))
(deflogger ucw.rerl.dispatcher (ucw.rerl))
(deflogger ucw.rerl.ajax (ucw.rerl))

(deflogger ucw.l10n (ucw))

(deflogger ucw.component (ucw.rerl))
(deflogger ucw.component.dojo (ucw.component))
(deflogger ucw.component.render (ucw.component))
(deflogger ucw.component.action (ucw.component))
(deflogger ucw.component.layers (ucw.component))

(deflogger ucw.backend (ucw))

(deflogger ucw.admin (ucw))

(deflogger ucw.examples (ucw))

(defun ucw.log-level ()
  (log.level (get-logger 'ucw)))

(defun (setf ucw.log-level) (level)
  (setf (log.level (get-logger 'ucw)) level))

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

