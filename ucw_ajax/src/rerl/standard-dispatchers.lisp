;; -*- lisp -*-
(in-package :it.bese.ucw)

(defclass starts-with-url-matcher (url-matcher)
  ())

(defmethod matcher-match ((matcher starts-with-url-matcher)
                          (application basic-application)
                          (context standard-request-context))
  "Returns matched url-string to handler."
  (ucw.rerl.dispatcher.dribble "~S trying to match as starts-with-url-matcher, ~S starts-with? ~S"
                               matcher (query-path-sans-prefix context) (url-string matcher))
  (when (starts-with (query-path-sans-prefix context) (url-string matcher))
    (ucw.rerl.dispatcher.dribble "~S matched" matcher)
    (values t (url-string matcher))))

(defclass session-frame-and-url-matcher (session-frame-matcher url-matcher)
  ()
  (:documentation "Matches when the url matches and there's
an identifiable session and frame in the request."))

(defmethod matcher-match ((matcher session-frame-and-url-matcher)
                          (application basic-application)
                          (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as session-frame-and-url-matcher" matcher)
  (multiple-value-bind (matchesp session frame)
      (call-next-method)
    (when (and matchesp
               (string= (url-string matcher)
                        (query-path-sans-prefix context)))
      (ucw.rerl.dispatcher.dribble "~S matched" matcher)
      (values t session frame (url-string matcher)))))

(defclass regexp-url-matcher (url-matcher)
  ((scanner :initarg :scanner
            :accessor scanner
            :documentation "CL-PPCRE scanner used for pattern
            matching. Created automagically when url-string is
            set via accessors or initform."))
  (:documentation "Regexp matcher class to match url-string
  via cl-ppcre scanner."))

(defmethod shared-initialize :after ((matcher regexp-url-matcher)
                                     slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  ;; trigger our customized accessor below
  (awhen (url-string matcher)
    (setf (url-string matcher) it)))

(defmethod (setf url-string) :after (value (matcher regexp-url-matcher))
  (setf (scanner matcher) (cl-ppcre:create-scanner value)))

(defmethod matcher-match ((matcher regexp-url-matcher)
                          (application basic-application)
                          (context standard-request-context))
  "Returns two values to handler on success: the whole match as a
string plus an array of substrings (or NILs) corresponding to the
matched registers."
  (ucw.rerl.dispatcher.dribble "~S trying to match ~S" matcher (query-path-sans-prefix context))
  (let ((result
         (multiple-value-list
           (cl-ppcre:scan-to-strings (scanner matcher)
                                     (query-path-sans-prefix context)
                                     :sharedp t))))
    (when (car result)
      (ucw.rerl.dispatcher.dribble "~S matched, regexp result is ~S" matcher result)
      (values-list
       (cons t result)))))

(defclass tal-matcher (matcher)
  ((extension :accessor extension :initform ".ucw" :initarg :extension
              :documentation "Only urls whcih end in EXTENSION
              will be checked for a corresponding .tal file."))
  (:documentation "Matcher used to match tal templates. Searches
  for a valid tal template."))

(defprint-object (self tal-matcher)
  (princ (extension self)))

(defmethod matcher-match ((matcher tal-matcher)
                          (application application-with-tal-support-mixin)
                          (context standard-request-context))
  "Returns tal template-truename to handler."
  (when-bind template-truename
      (template-truename (application.tal-generator application)
                         (make-pathname :type "tal"
                                        :defaults (query-path-sans-prefix context)))
    (ucw.rerl.dispatcher.debug "~S matched against tal file ~S" matcher template-truename)
    (values t template-truename)))

(defclass function-handler (handler)
  ((handler :accessor handler
            :initarg :handler
            :documentation "Called when the dispatchers finds a
            maching request. This function must be a zero arg'ed
            (lambda ()
               ...)"))
  (:documentation "Function handler class funcalls the handler
  lambda while providing application and context. "))

(defmethod handler-handle ((function-handler function-handler)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (when (slot-boundp function-handler 'handler)
    (ucw.rerl.dispatcher.debug "~S running handler ~S" function-handler (handler function-handler))
    (funcall (handler function-handler))))

(defclass parenscript-handler (handler)
  ((cachep :accessor cachep :initarg :cache :initform t)
   (cached-javascript :accessor cached-javascript :initform nil)
   (last-compiled :initform 0 :accessor last-compiled))
  (:documentation "This handler is used to serve a
compiled javascript string.

The source is compiled lazily on the first request.
The following dynamic variables are bound when compiling:
  *context* - the current request context
              (hint: (context.application *context*))"))

(defgeneric parenscript-handler-compile (handler)
  (:documentation "Called when (re)compilation is needed. Should return the compiled js string."))

(defgeneric parenscript-handler-timestamp (handler)
  (:documentation "Should return the timestamp of the source (used for dirtyness check)."))

(defmethod handler-handle ((handler parenscript-handler)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (with-accessors
      ((cachep cachep) (cached-javascript cached-javascript)
       (last-compiled last-compiled)) handler
    (ucw.rerl.dispatcher.debug "~S handling the request" handler)
    (when (or (not cachep)
              (not cached-javascript)
              (> (parenscript-handler-timestamp handler)
                 last-compiled))
      (ucw.rerl.dispatcher.debug "~S is compiling the file" handler)
      ;; enable a #"" syntax for i18n lookups in parenscript
      (let ((*readtable* (copy-readtable *readtable*)))
        (enable-js-sharpquote-syntax)
        (setf cached-javascript (parenscript-handler-compile handler)))
      (setf last-compiled (get-universal-time)))
    (serve-sequence cached-javascript
                    :last-modified last-compiled
                    :content-type "text/javascript")))

(defclass tal-handler (handler)
  ()
  (:documentation "This handler simply serves up TAL pages.

Tal pages can refer to the session object (and it will be
maintained across requests) but must avoid using call/answer or
actions."))

(defmethod handler-handle ((handler tal-handler)
                           (application application-with-tal-support-mixin)
                           (context standard-request-context)
                           matcher-result)
 (when (ends-with (query-path-sans-prefix context) (extension handler))
  (let ((template-truename (car matcher-result)))
    (ucw.rerl.dispatcher.info "TAL-HANDLER publishing tal ~S as ~S." template-truename (query-path-sans-prefix context))
    (let ((session (ensure-session application context)))
      (make-new-frame nil session)
      (%render-template context
                        (application.tal-generator application)
                        template-truename
                        'nil)))))


(defclass minimal-dispatcher (dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority +minimal-dispatcher-default-priority+)
  (:documentation "A dispatcher which does as little work as
  possible. The handler function must do basically
  everything (including shutdowning down request and response)."))

(defmacro make-minimal-dispatcher (url-regexp &body body)
  `(make-instance 'minimal-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             ,@body)))

(defclass simple-dispatcher (dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority +simple-dispatcher-default-priority+)
  (:documentation "This class of dispatchers avoids all of UCW's
  standard call/cc (and therefore frame/backtracking/component)
  mechanism.

Unlike all other UCW dispatchers a simple-dispatcher must not use
CALL, and must perform the rendering directly within the handler."))

(defmethod handler-handle ((dispatcher simple-dispatcher)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (ensure-session application context)
  (call-next-method))

(defmacro make-simple-dispatcher (url-regexp &body body)
  `(make-instance 'simple-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             ,@body)))



(defclass regexp-dispatcher (dispatcher
                             regexp-url-matcher
                             regexp-binding-handler
                             entry-point-handler)
  ()
  (:default-initargs :priority +regex-dispatcher-default-priority+))

(defmacro make-regexp-dispatcher (url-regexp &body body)
  "Returns a dispatcher which matches when URL-REGEXP (a regular
expression which is passed untouched to cl-ppcre:scan-to-strings)
matches and executes BODY in a with-call/cc block.

Any registers in URL-REGEXP are available through the array bound
to *dispatcher-registers*."
  `(make-instance 'regexp-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             (with-call/cc
                               (let ((self nil))
                                 ,@body)))))


(defclass regexp-binding-handler ()
  ())

(defmethod handler-handle :around ((handler regexp-binding-handler)
                                   application
                                   context
                                   matcher-result)
  (let ((*dispatcher-registers* (second matcher-result)))
    (call-next-method)))


;;;; ajax-action-dispatcher

(defclass ajax-action-dispatcher (action-dispatcher starts-with-url-matcher)
  ()
  (:default-initargs :priority +ajax-action-dispatcher-default-priority+
                     :url-string +ajax-action-dispatcher-url+)
  (:documentation "This is a specialized action dispatcher to handle ajax requests."))

(defmethod matcher-match ((matcher ajax-action-dispatcher)
                          (application basic-application)
                          (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as ajax-action-dispatcher" matcher)
  (when (starts-with (query-path-sans-prefix context) (url-string matcher))
    (multiple-value-bind (matchesp session frame action) (call-next-method)
      (declare (ignore matchesp))
      (values t session frame action))))

(defmethod handler-handle ((dispatcher ajax-action-dispatcher)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (destructuring-bind (session frame action) matcher-result
    (disallow-response-caching (context.response context))
    (if (and session frame action)
        (progn
          (ensure-session application context session)
          (handle-action action application session frame))
        (send-ajax-answer-to-expired-session))))

(defun send-ajax-answer-to-expired-session ()
  (handle-ajax-request (:succesful-when-finishes nil)
    ;; this visible MESSAGE variable is kludgy, but works... see HANDLE-AJAX-REQUEST for details.
    ;; TODO send a redirect or something that does this automatically...
    (setf -message- "Your session has expired, reload the page.")))

;;;; callback-dispatcher

(defclass callback-dispatcher (action-dispatcher starts-with-url-matcher)
  ()
  (:default-initargs :priority +callback-dispatcher-default-priority+
                     :url-string +callback-dispatcher-url+)
  (:documentation "This dispatcher handles action-less ajax requests that are only triggered to post form data."))

(defmethod matcher-match ((matcher callback-dispatcher)
                          (application basic-application)
                          (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as callback-dispatcher" matcher)
  (when (starts-with (query-path-sans-prefix context) (url-string matcher))
    (multiple-value-bind (matchesp session frame action) (call-next-method)
      (declare (ignore matchesp action))
      (values t session frame))))

(defmethod handler-handle ((dispatcher callback-dispatcher)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (destructuring-bind (session frame) matcher-result
    (disallow-response-caching (context.response context))
    (if (and session frame)
        (progn
          (ensure-session application context session)
          (call-callbacks nil frame (context.request context))
          (handle-ajax-request ()
            ;; render a <result>success</result> to calm down the client side js
            ))
        (send-ajax-answer-to-expired-session))))


;;;; polling-dispatcher

(defclass polling-dispatcher (dispatcher starts-with-url-matcher)
  ()
  (:default-initargs :url-string +polling-dispatcher-url+
                     :priority +polling-dispatcher-default-priority+)
  (:documentation "This dispatcher is handling the polling requests."))

(defmethod matcher-match ((matcher polling-dispatcher)
                          (application basic-application)
                          (context standard-request-context))
  (multiple-value-bind (matchesp url-prefix) (call-next-method)
    (declare (ignore url-prefix))
    (when matchesp
      (ucw.rerl.dispatcher.dribble "~S matched the url, extracted session id is ~S" matcher (find-session-id context))
      (let ((session (find-session application context))
            (frame nil))
        (when session
          (ucw.rerl.dispatcher.dribble "~S matched the session" matcher)
          (ensure-session application context session)
          (when-bind frame-id (find-frame-id context)
            (setf frame (find-frame-by-id session frame-id))))
        (ucw.rerl.dispatcher.debug "~S matched session ~S and frame ~S" matcher session frame)
        (values t session frame)))))

(defmethod handler-handle ((dispatcher polling-dispatcher)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (destructuring-bind (session frame) matcher-result
    (ucw.rerl.dispatcher.dribble "~S handling the request in session ~S frame ~S" dispatcher session frame)
    (disallow-response-caching (context.response context))
    (handle-polling-of-session application session frame)))

;;;; *** Tal Directory dispatcher

(defclass tal-dispatcher (dispatcher tal-matcher tal-handler)
  ()
  (:default-initargs :priority +tal-dispatcher-default-priority+)
  (:documentation "This dispatcher simply serves up TAL pages.
Tal pages can refer to the session object (and it will be
maintained across requests) but must avoid using call/answer or
actions."))

(defclass parenscript-dispatcher (dispatcher url-matcher parenscript-handler)
  ((compile-args :accessor compile-args :initarg :compile-args :initform '())
   (parenscript-file :accessor parenscript-file :initarg :parenscript-file))
  (:default-initargs :priority +parenscript-dispatcher-default-priority+)
  (:documentation "This handler is used to serve a
compiled javascript string from the given PARENSCRIPT-FILE.

The file is compiled lazily on the first request and in-package
forms are working as expected. Each form is evaluated, so using
backquote is possible and in fact a must. The following dynamic
variables are bound when compiling:
  *context* - the current request context
              (hint: (context.application *context*))"))

(defprint-object (self parenscript-dispatcher)
  (when (cachep self)
    (princ "cached")
    (princ " "))
  (princ (parenscript-file self))
  (princ " ")
  (princ (last-compiled self)))

(defmethod (setf javascript-log-level) :around (value (app standard-application))
  (when (or (not (slot-boundp app 'javascript-log-level))
            (not (equal (javascript-log-level app)
                        value)))
    (call-next-method)
    ;; invalidate the cache of all parenscript-dispatchers. but even after this they need
    ;; a browser refresh to get recompiled while the date sent in the expires header is not over.
    (dolist (dispatcher (remove-if (lambda (el)
                                     (not (typep el 'parenscript-dispatcher)))
                                   (application.dispatchers app)))
      (setf (cached-javascript dispatcher) nil))))

(defmethod parenscript-handler-compile ((self parenscript-dispatcher))
  (apply #'js:compile-parenscript-file-to-string (parenscript-file self) (compile-args self)))

(defmethod parenscript-handler-timestamp ((self parenscript-dispatcher))
  (file-write-date (parenscript-file self)))

(defun make-parenscript-dispatcher (url-regex parenscript-file &rest args &key
                                              (cache t)
                                              (priority +parenscript-dispatcher-default-priority+)
                                              &allow-other-keys)
  "Return a parenscript-dispatcher"
  (remf-keywords args :cache)
  (make-instance 'parenscript-dispatcher
                 :url-string url-regex
                 :parenscript-file parenscript-file
                 :compile-args args
                 :priority priority
                 :cache cache))

(defun make-standard-ucw-dispatchers ()
  (append
   (awhen (find-class 'i18n-parenscript-dispatcher nil)
     (list (make-instance it)))
   (list*
    (make-instance 'ajax-action-dispatcher)
    (make-instance 'callback-dispatcher)
    (make-instance 'polling-dispatcher)
    (make-parenscript-dispatcher (map-to-dynamic-ucw-url "js/per-application.js")
                                 (system-relative-pathname :ucw #P"src/per-application-parenscript.lisp")
                                 :eval-forms-p t)
    (make-parenscript-dispatcher (map-to-dynamic-ucw-url "js/functional.js")
                                 (system-relative-pathname :parenscript #P"src/lib/functional.lisp"))
    (make-basic-ucw-dispatchers))))

;; Copyright (c) 2003-2006 Edward Marco Baringer
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
