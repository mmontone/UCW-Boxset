;;; -*- lisp -*-

;;;; * The Packages

(in-package :it.bese.ucw.system)

(defpackage :it.bese.ucw
  (:nicknames :ucw)
  (:shadowing-import-from :trivial-garbage
    #:make-hash-table)
  (:use :common-lisp
        :it.bese.ucw.system
        :it.bese.arnesi
        :it.bese.yaclml
        :bordeaux-threads
        :local-time
        :trivial-garbage
        :iterate)
  (:shadow
   #:parent)
  (:export
   ;; backend classes
   #:mod-lisp-backend
   #:multithread-mod-lisp-backend
   #:aserve-backend
   #:araneida-backend
   #:httpd-backend
   #:multithread-httpd-backend
   ;; random configuration options
   #:*inspect-components*
   #:external-format-for
   ;; rerl protocol
   #:*default-server*
   #:standard-server
   #:startup-server
   #:shutdown-server
   #:restart-server
   #:server.backend
   #:server.applications
   #:debug-on-error
   #:javascript-log-level
   #:*debug-on-error*
   #:handle-action
   #:call-action
   #:call-render
   #:abort-action
   #:handle-toplevel-condition
   #:send-standard-error-page
   
   #:*context*
   #:context.window-component
   #:context.request
   #:context.response
   #:context.session
   #:context.application
   #:context.action
   #:with-dummy-context
   #:make-request-context
   #:delete-session
   #:ensure-session
   #:expire-session
   
   #:startup-application
   #:shutdown-application
   #:restart-application
   #:register-application
   #:unregister-application
   #:*default-application*
   #:standard-application
   #:application.url-prefix
   #:basic-application
   #:register-dispatcher
   #:cookie-session-application
   #:cookie-session-request-context

   #:cookies
   #:find-cookie
   #:cookie-value
   #:add-cookie
   #:make-cookie

   #:lock-of
   #:with-lock-held-on-application
   #:with-lock-held-on-session

   ;; application mixins
   #:cookie-session-application-mixin
   #:secure-application-mixin
   #:l10n-application-mixin
   #:ajax-application-mixin
   #:dirty-component-tracking-application-mixin
   #:request-context-class
   #:session-class
   #:user-track-application-mixin
   #:application.online-users

   ;; captcha
   #:*captcha-application*
   #:captcha-factory-of
   #:get-captcha

   ;; accessing the request/response objects
   #:mime-part-p
   #:mime-part-headers
   #:mime-part-body
   
   #:request
   #:response
   #:html-stream
   #:close-request
   #:get-header
   #:get-parameter
   #:map-parameters
   
   #:send-headers
   #:send-response
   #:serve-sequence

   ;; backtracking
   #:backtrack
   #:backtrack-slot

   ;; components
   #:defcomponent
   #:compute-url
   #:update-url
   #:standard-component-class
   #:component
   #:parent
   #:standard-component
   #:template-component
   #:template-component-environment
   #:simple-template-component
   #:simple-template-component-with-body
   #:standard-template-component
   #:standard-template-component-with-body
   #:show
   #:show-window
   #:html-element
   #:with-html-element-wrapper
   #:widget-component
   #:widget-component-with-body
   #:inline-widget-component
   #:inline-widget-component-with-body
   #:css-class
   #:css-style
   #:child-components
   #:find-parent-typed
   #:component.place
   #:place
   #:session-of
   
   ;; dojo widgets
   #:widget-id
   #:dojo-widget
   #:add-onload-script
   #:simple-dojo-widget
   #:dojo-content-pane
   #:dojo-html-text-editor
   #:dojo-tab-container
   #:dojo-tab
   #:wrap-in-dojo-tab
   #:dojo-inline-edit-box
   #:dojo-date-picker
   #:dojo-time-picker
   #:dojo-dropdown-date-picker
   #:dojo-dropdown-time-picker
   #:dojo-timestamp-picker
   #:ajax-render-new-tab
   #:dojo-split-container
   #:rendering-dojo-tooltip-for
   
   ;; windows
   #:window-component
   #:basic-window-component
   #:standard-window-component
   #:standard-window-component-with-body
   #:body-of
   #:basic-window-features-mixin
   #:window-component.icon
   #:window-component.stylesheet
   #:window-component.javascript
   #:window-component.title
   #:window-component.content-type
   #:render-html-head
   #:render-html-body
   
   ;; generic componet actions
   #:refresh-component
   #:ok
   #:meta-refresh
   
   ;; error message component
   #:error-message
   #:error-component
   
   ;; login component
   #:login
   #:login.username
   #:login.password
   #:try-login
   #:check-credentials
   #:login-successful

   ;; collapsible-pane component
   #:collapsible-pane
   #:collapsedp
   #:render-standard-switch
   #:within-client-side-collapsible-pane

   ;; info-message component
   #:info-message

   ;; option dialog component
   #:option-dialog
   #:respond

   ;; container component
   #:container
   #:switching-container
   #:list-container
   #:make-list-container
   #:make-horizontal-list-container
   #:make-vertical-list-container
   #:component-at
   #:add-component
   #:remove-component
   #:clear-container
   #:container.current-component-key
   #:container.current-component
   #:container.key-test
   #:container.contents
   #:switch-component
   #:find-component
   #:initialize-container
   #:component-body-mixin

   ;; ajax component
   #:ajax-component-mixin
   #:render-ajax-stub
   #:ajax-render
   #:dirtyp
   #:visiblep
   #:mark-dirty
   #:without-dirtyness-tracking
   #:js-to-lisp-rpc
   #:js-to-lisp-rpc*
   
   ;; inspector
   #:ucw-inspector
   #:inspect-anchor
   
   ;; forms
   #:inside-a-form-p
   #:current-form
   #:current-form-id
   #:register-submit-callback
   #:form-field
   #:generic-html-input
   #:dom-id
   #:value
   #:client-value
   #:tabindex
   
   #:html-form
   #:form-component-mixin
   #:form-component-with-body

   #:string-field
   #:textarea-field
   #:number-field
   #:integer-field
   #:range-field
   #:password-field
   #:checkbox-field
   #:file-upload-field
   #:select-field
   #:mapping-select-field
   #:alist-select-field
   #:hash-table-select-field
   #:plist-select-field
   #:submit-button
   #:radio-group
   #:value-widget
   #:in-field-string-field
   #:date-field
   #:date-ymd
   #:is-a-date-validator
   #:is-a-date-time-validator
   #:time-range-validator
   #:dmy-date-field
   #:mdy-date-field

   #:validator
   #:validators
   #:generate-javascript
   #:generate-javascript-check
   #:javascript-check
   #:generate-javascript-valid-handler
   #:javascript-valid-handler
   #:generate-javascript-invalid-handler
   #:javascript-invalid-handler
   #:validp
   #:is-an-integer-validator
   #:number-range-validator  
   #:length-validator
   #:min-length
   #:max-length
   #:not-empty-validator
   #:string=-validator
   #:integer-range-validator
   #:regex-validator
   #:regex
   #:hostname-validator
   #:e-mail-address-validator
   #:phone-number-validator
  
   ;; range-view component
   #:range-view
   #:render-range-view-item
   #:range-view.current-window
   #:range-view.current-window-items
   #:range-view.windows
   #:range-view.have-next-p
   #:range-view.have-previous-p
   
   ;; the date picker component
   #:generic-date-picker
   #:dropdown-date-picker
   #:date-picker.year
   #:date-picker.day
   #:date-picker.month
   #:date-picker.partial-date-p
   #:date-picker.complete-date-p

   #:redirect-component
   #:send-redirect
   
   ;; the tabbed-pane component
   #:tabbed-pane

   ;; the task component
   #:task-component
   #:start

   ;; status bar component
   #:status-bar
   #:add-message
   #:show-message

   ;; cache
   #:cached-component
   #:cached-output
   #:timeout
   #:component-dirty-p
   #:refresh-component-output
   #:timeout-cache-component
   #:num-hits-cache-component
   
   ;; transactions
   #:transaction-mixin
   #:open-transaction
   #:close-transaction

   ;; secure application
   #:secure-application-mixin
   #:secure-application-p
   #:application-find-user
   #:application-check-password
   #:application-authorize-call
   #:on-authorization-reject
   #:session-user
   #:session-authenticated-p
   #:user-login
   #:login-user
   #:logout-user
   #:exit-user
   
   ;; actions
   #:defentry-point
   #:self
   #:call
   #:call-component
   #:call-as-window
   #:answer
   #:answer-component
   #:jump
   #:jump-to-component
   #:make-place
   #:action-href
   #:action-href-body
   #:handle-raw-request
   #:action-id
   #:action
   #:basic-action
   #:ajax-action
   #:standard-action
   
   ;; disptachers
   #:minimal-dispatcher
   #:make-minimal-dispatcher
   #:simple-dispatcher
   #:make-simple-dispatcher
   #:url-dispatcher
   #:make-url-dispatcher
   #:regexp-dispatcher
   #:make-regexp-dispatcher
   #:action-dispatcher
   #:*dispatcher-registers*
   #:tal-dispatcher
   #:parenscript-dispatcher
   #:make-parenscript-dispatcher
   #:make-standard-ucw-dispatchers
   #:make-standard-ucw-www-root-list
   #:make-standard-ucw-tal-dir-list

   #:with-request-params
   
   ;; session
   #:get-session-value
   #:session.value
   #:make-new-session
   #:with-session-variables
   
   #:register-action
   #:register-ajax-action
   #:make-action
   #:make-action-body
   #:register-callback
   #:make-callback
   
   ;; l10n
   #:l10n-application
   #:reload-ucw-resources
   #:default-locale-of
   #:accepted-locales-of
   #:l10n-request-context
   #:context.locale
   #:l10n-tal-generator
   #:+missing-resource-css-class+
   #:enable-js-sharpquote-syntax
   #:enable-sharpquote<>-syntax
   #:with-sharpquote<>-syntax
   #:define-js-resources
   #:client-timezone-of
   #:render-client-timezone-probe
   
   ;; yaclml/tal
   #:render
   #:call-in-rendering-environment
   #:call-in-restored-rendering-environment
   #:in-restored-rendering-environment
   #:render-template

   ;; publishing files, directories and other "stuff"
   #:publish-directory

   ;; Helper functions
   #:read-from-client-string
   #:add-query-parameter-to-uri
   #:append-path-to-uri
   #:write-uri-sans-query
   #:make-uri
   #:write-uri
   #:print-uri-to-string
   #:print-uri-to-string-sans-query

   ;; Control utilities
   #:start-swank
   #:create-server))

(defpackage :it.bese.ucw-user
  (:nicknames :ucw-user)
  (:shadowing-import-from :trivial-garbage
    #:make-hash-table)
  (:shadowing-import-from :ucw
    #:parent)
  (:use :common-lisp
        :it.bese.ucw
        :it.bese.arnesi
        :iterate
        :local-time
        :bordeaux-threads
        :trivial-garbage
        :it.bese.yaclml))

(defpackage :it.bese.ucw.lang
  (:nicknames :ucw.lang)
  (:import-from :ucw
    #:+missing-resource-css-class+
    #:define-js-resources)
  (:export
    #:+missing-resource-css-class+
    #:define-js-resources))

(defpackage :it.bese.ucw.tags
  (:documentation "UCW convience tags.")
  (:use)
  (:nicknames #:<ucw)
  (:export
   #:component-body
   #:render-component
   #:a
   #:area
   #:form
   #:input
   #:button
   #:simple-select
   #:select
   #:option
   #:textarea

   #:integer-range-select
   #:month-day-select
   #:month-select

   #:text
   #:password
   #:submit
   #:simple-form
   #:simple-submit

   #:localized
   #:script))

(defpackage :it.bese.ucw.dojo.tags
  (:documentation "Dojo convience tags.")
  (:use)
  (:nicknames #:<dojo)
  (:export
   #:widget))


(in-package :it.bese.ucw)

(defmacro debug-only (&body body)
  (unless *load-as-production-p*
    `(progn ,@body)))

;;;;@include "rerl/protocol.lisp"

;;;; * Components

;;;;@include "components/login.lisp"

;;;;@include "components/error.lisp"

;;;;@include "components/message.lisp"

;;;;@include "components/option-dialog.lisp"

;;;;@include "components/range-view.lisp"

;;;;@include "components/redirect.lisp"

;;;;@include "components/tabbed-pane.lisp"

;;;;@include "components/task.lisp"

;;;;@include "components/ucw-inspector.lisp"

;;;; * Meta Components

;;;;@include "components/widget.lisp"

;;;;@include "components/window.lisp"

;;;;@include "components/template.lisp"

;;;;@include "components/container.lisp"

;;;; * Standard RERL Implementation

;;;;@include "rerl/standard-server.lisp"

;;;;@include "rerl/standard-application.lisp"

;;;;@include "rerl/standard-session.lisp"

;;;;@include "rerl/cookie-session.lisp"

;;;;@include "rerl/standard-session-frame.lisp"

;;;;@include "rerl/standard-action.lisp"

;;;;@include "rerl/backtracking.lisp"

;;;;@include "rerl/request-loop-error.lisp"

;;;;@include "rerl/conditions.lisp"

;;;;@include "rerl/standard-vars.lisp"

;;;; ** Standard Component

;;;; * The Backends

;;;;@include "backend/httpd.lisp"

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
