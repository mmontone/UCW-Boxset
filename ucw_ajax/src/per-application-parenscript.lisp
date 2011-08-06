;; -*- lisp -*-
;; See the file LICENCE for licence information.

(in-package :it.bese.ucw)

;;; This file is dynamically compiled for each application instance.
;;; Each form is first eval'd and then compiled with js:js* unless it
;;; evaluates to nil. *context* is bound while compiling, so
;;; (context.application *context*) is the hosting application.

;;; Please note:
;;;  - the usage of (.get-attribute node "foo") instead of node.foo is not random, on ie
;;;    dereferencing an attribute of an XMLHttpRequest does not work

`(defun $ (id)
  (return (dojo.by-id id)))

`(let ((debug-conatiner ($ "dojoDebug")))
  (when (and (not debug-conatiner)
             dj-config.is-debug)
    (setf debug-conatiner (document.create-element "DIV"))
    (setf debug-conatiner.id "dojoDebug")
    (setf debug-conatiner.style.position "absolute"
          debug-conatiner.style.top "1000px")
    (document.body.append-child debug-conatiner)))

`(let ((level ,(javascript-log-level (context.application *context*))))
  (dojo.logging.log.set-level (if level
                                  (dojo.logging.log.get-level level)
                                  10))
  (when level
    (dojo.hostenv.println (+ "per-application.js speaking, a random string at js compile-time: " ,(random-string 4)
                             ", logging level is " (dojo.logging.log.get-level-name (dojo.logging.log.get-effective-level))))))

;;;;;;;;;;;;;;;;;;;;;
;;; ucw

`(into-namespace ucw
  
  (defun get-first-child-by-tag-name (node tag-name)
    (return (aref (node.get-elements-by-tag-name tag-name) 0)))

  ;; walk the parents and see if there's a node that would
  ;; trigger an action when clicked. useful for onclick handlers to
  ;; see wether something active or some dead area was clicked
  ;; TODO this may be useless, use event propagation control instead
  (defun has-action-above ((start-node :by-id))
    (let ((node start-node))
      (while node
        (let ((node-name (.to-lower-case node.node-name)))
          (when (or (= node-name "input")
                    (= node-name "select")
                    (= node-name "a"))
            (return true))
          (setf node node.parent-node)))
      (return false)))
  
  (defun absolute-url-from (url)
    (if (> (.index-of url ":") 0) ; this test may be fragile here
        url
        (return (+ window.location.protocol "//"
                   window.location.hostname ":"
                   window.location.port
                   url))))
  
  (defun may-abandon-the-page ((forms-to-ask (ucw.form.all-registered)) (forms-to-ignore (array)))
    (log.info "Page abandoning handler called")
    (when (map-until
           (lambda (form)
             (log.debug "Checking form " form.id " before abandoning the page")
             (if form.ucw-approve-abandoning
                 (unless (form.ucw-approve-abandoning form)
                   (log.debug "Form " form.id " cancelled abandoning the page")
                   (return true))
                 (log.debug "Form " form.id " has no abandon-handler")))
           (set-difference forms-to-ask forms-to-ignore))
      (log.info "Page abandoning was cancelled")
      (return false))
    (log.info "Page abandoning was approved")
    (return true))
  
  (defun load-relative-url (url (forms-to-ignore (array)))
    (log.info "Loading relative url " url " and ignoring " forms-to-ignore.length " forms.")
    (when (ucw.may-abandon-the-page undefined forms-to-ignore)
      (let ((body (dojo.body)))
        (dolist (node body.child-nodes)
          (unless (= node ucw.io.progress.container)
            (dojo.html.hide node))))
      (setf window.location (ucw.absolute-url-from url))
      (return true))
    (return false))

  (defun generate-invocation-id ()
    ;; TODO use some timer that has some uniqueness guarantees
    (return (*math.floor (* (*math.random) 1000000))))

  (defun decorate-with-invocation-id (thing)
    (assert thing)
    (cond ((= (typeof thing) "string")
           (return (+ thing (if (< (.index-of thing #\?) 0) "?" "&") ,(escape-as-uri +action-invocation-parameter-name+) "="
                      (ucw.generate-invocation-id))))
          ((= (typeof thing) "object")
           (setf (slot-value (slot-value thing ,+action-invocation-parameter-name+) 'value) (ucw.generate-invocation-id))
           (return thing))
          (t (throw "Don't know how to decorate invocation id on that"))))

  (defun decorate-url-with-session-and-frame (url (session-id ucw.session-id) (frame-id ucw.frame-id) action-id)
    (setf url (+ url (if (< (.index-of url "?") 0)
                         "?"
                         "&")))
    (setf url (+ url ,(escape-as-uri +session-parameter-name+) "=" (encode-u-r-i-component session-id)
                 "&" ,(escape-as-uri +frame-parameter-name+) "=" (encode-u-r-i-component frame-id)))
    (when action-id
      (setf url (+ url "&" ,(escape-as-uri +action-parameter-name+) "=" (encode-u-r-i-component action-id))))
    (return url))

  (defun load-url-with-invocation-id (url forms-to-ask forms-to-ignore)
    (return (ucw.load-relative-url (ucw.decorate-with-invocation-id url) forms-to-ask forms-to-ignore)))

  (defun default-unload-event-handler (e)
    (ucw.io.polling.stop)
    (log.info "Default unload event handler will check " (slot-value (ucw.form.all-registered) 'length) " forms")
    (if (map-until (lambda (form)
                     (log.debug "Default unload handler checking form " form.id)
                     (when (and (not (= ucw.form._form-being-submitted form))
                                (ucw.form.dirty-p form false))
                       (if form.ucw-approve-abandoning
                           (unless (form.ucw-approve-abandoning form)
                             (log.debug "Form " form.id " cancelled abandoning the page")
                             (return true))
                           (log.debug "Form " form.id " has no abandon-handler"))))
                   (ucw.form.all-registered))
        (progn
          (log.debug "Default unload handler is asking for confirmation to unload the page")
          (setf e.return-value #"confirm-pending-changes"))
        (log.debug "Default unload event handler found no dirty forms")))

  (defun has-id-or-fail (node)
    (let ((id (.get-attribute node "id")))
      (unless id
        (throw (new (ucw.server-communication-error "No dom id found, but we need one here"))))))

  (defun server-communication-error (message)
    (setf this.message message)))

;;;;;;;;;;;;;;;;;;;;;
;;; widgets

`(into-namespace ucw.widget.tab-container
  (defun setup-remember-selected-tab (widget-id)
    (let ((cookie-name (+ widget-id "-current-tab"))
          (selected-widget-id (dojo.io.cookie.get cookie-name)))
      ;; TODO support control of overriding server value when specified
      ;; currently the client side cookie has precedence
      (let ((widget (dojo.widget.by-id widget-id)))
        (assert widget)
        (dojo.event.connect widget "selectChild"
                            (lambda ()
                              (let ((selected-widget widget.selected-child-widget))
                                (log.debug "Saving the selected tab of the TabContainer widget " widget-id " to " selected-widget)
                              (when selected-widget
                                (dojo.io.cookie.set cookie-name selected-widget.widget-id)))))
        (log.debug "Restoring the selected tab of the TabContainer widget " widget-id " to " selected-widget-id)
        (when selected-widget-id
          (setf widget.selected-child selected-widget-id))))))

`(into-namespace ucw.widget.switching-container
  (defun select-child ((container :by-id) (child-to-select :by-id))
    (dolist (child container.child-nodes)
      (if (= child-to-select child)
          (progn
            (when child.ucw-ajax-stub-p
              (child.ucw-downloader))
            (dojo.html.show child)
            (when container.ucw-remember-selected-child-p
              (let ((cookie-name (+ container.id "-current-child")))
                (log.debug "Saving the selected child of the switching-container " container " which is " child " into " cookie-name)
                (dojo.io.cookie.set cookie-name child.id))))
          (dojo.html.hide child))))

  (defun clear-selected-child ((container :by-id))
    (let ((cookie-name (+ container.id "-current-child")))
      (dojo.io.cookie.delete-cookie cookie-name)))

  (defun restore-selected-child ((container :by-id))
    (when container.ucw-remember-selected-child-p
      (let ((cookie-name (+ container.id "-current-child"))
            (selected-id (dojo.io.cookie.get cookie-name)))
        ;; TODO support control of overriding server value when specified
        ;; currently the client side cookie has precedence
        (when selected-id
          (if ($ selected-id)
              (progn
                (log.debug "Restoring the selected child of the switching-container " container " to " selected-id)
                (ucw.widget.switching-container.select-child container selected-id))
              (log.warn "Could not find selected child " selected-id " of switching-container " container)))))))


`(into-namespace ucw.widget.collapsible-pane
  (defun toggle (switch-node collapsed-image expanded-image)
    (let ((body switch-node.parent-node.next-sibling)
          (img switch-node.first-child))
      (assert (dojo.html.has-class body "body"))
      (assert (dojo.html.is-tag img "img"))
      (if (dojo.html.is-showing body)
          (progn
            (dojo.html.hide body)
            (setf img.src collapsed-image))
          (progn
            (dojo.html.show body)
            (setf img.src expanded-image))))))

;;;;;;;;;;;;;;;;;;;;;
;;; event

`(into-namespace ucw.event

  (defun connect (node events handler)
    (assert (and node events handler))
    (if (dojo.lang.is-array-like events)
        (dolist (event events)
          (dojo.event.connect node event handler))
        (dojo.event.connect node events handler)))

  (defun kw-connect (params)
    (if (dojo.lang.is-array-like params.src-func)
        (progn
          (setf params (dojo.lang.shallow-copy params))
          (dolist (event params.src-func)
            (setf params.src-func event)
            (dojo.event.kw-connect params)))
        (dojo.event.kw-connect params)))

  (defun install-event-handler ((node :by-id) event-name event-params)
    (log.debug "Installing " event-name " event handler on " node " with params " event-params)
    (setf args (dojo.lang.to-array arguments))
    (.shift args)
    (.shift args)
    (ucw.event.connect node event-name (ucw.event.make-action-event-handler event-params)))

  ;; params is passed to ucw.io.execute-ajax-action after a shallow-copy but the following are also used here:
  ;; :url :ajaxp :forms-to-submit :forms-to-abandon :progress-label
  ;; do NOT modify anything in params, it's a constant that lives together with the event handler
  (defun make-action-event-handler (params)
    (return
      (lambda (e)
        (with-slots (url session-id frame-id action-id ajaxp forms-to-submit forms-to-abandon progress-label) params
          (let ((has-forms-to-submit (and forms-to-submit
                                          (> (slot-value forms-to-submit 'length) 0)))
                (forms-to-submit-count (if has-forms-to-submit
                                           (slot-value forms-to-submit 'length)
                                           0))
                (has-forms-to-abandon (and forms-to-abandon
                                           (> (slot-value forms-to-abandon 'length) 0)))
                (action-href (ucw.decorate-url-with-session-and-frame url session-id frame-id action-id)))
            (if ajaxp
                (progn
                  (log.info "Triggering AJAX action " action-href " with " forms-to-submit-count " forms to submit: " forms-to-submit)
                  (ucw.io.execute-ajax-action (dojo.lang.shallow-copy params)))
                (progn
                  (log.info "Triggering action " action-href " with " forms-to-submit-count " forms to submit: " forms-to-submit)
                  (if has-forms-to-submit
                      (progn
                        (when (> (slot-value forms-to-submit 'length) 1)
                          (throw "Internal error: Submitting more then one form with an action is only supported with AJAX"))
                        (let ((form ($ (aref forms-to-submit 0))))
                          (ucw.form.set-action form url session-id frame-id action-id)
                          (.onsubmit form) ; TODO ff needs it, others?
                          (.submit form)))
                      (ucw.load-url-with-invocation-id action-href undefined forms-to-abandon))))
            (when e
              (dojo.event.browser.stop-event e))
            (return false)))))))

;;;;;;;;;;;;;;;;;;;;;
;;; field

`(into-namespace ucw.field
  
  (defun register ((form :by-id) field-ids dirty-p-checker validator validate-initially)
    (assert (and form field-ids))
    (unless (dojo.lang.is-array field-ids)
      (setf field-ids (array field-ids)))
    (dolist (field-id field-ids)
      (let ((field (ucw.field.by-id field-id)))
        (log.debug "Registering field " field " under form " form)
        (unless form.ucw-form-fields
          (setf form.ucw-form-fields (new dojo.collections.*dictionary)))
        (.add form.ucw-form-fields field-id field)
        (when (= dirty-p-checker undefined)
          (setf dirty-p-checker (lambda ()
                                  (return (slot-value field 'ucw-dirty))))
          (setf (slot-value field 'ucw-dirty) false)
          (ucw.event.connect field (list "onchange" "onkeyup")
                             (lambda ()
                               (setf (slot-value field 'ucw-dirty) true))))

        (setf (slot-value field 'ucw-dirty-p) dirty-p-checker)
        (ucw.field.setup-validator field validator validate-initially))))

  (defun by-id (field-id)
    (let ((result field-id))
      (when (dojo.lang.is-string field-id)
        (aif (dojo.widget.by-id field-id)
             (setf result it)
             (aif ($ field-id)
                  (setf result it)
                  (log.error "Could not find anything that looks like a field by the id '" field-id "'"))))
      (return result)))
  
  (defun get-value (field-id)
    (let ((field (ucw.field.by-id field-id)))
      (return (if (instanceof field dojo.widget.*widget)
                  (.get-value field)
                  field.value))))

  (defun setup-validator (field validator validate-initially)
    (setf (slot-value field 'ucw-validate)
          (lambda ()
            (let ((validp true)
                  (validatedp false))
              (when validator
                (log.debug "Validating " field " with the provided UCW validator")
                (setf validp (validator (slot-value field 'value)))
                (setf validatedp true))
              (when (and validp
                         (instanceof field dojo.widget.*widget)
                         field.is-valid)
                (log.debug "Validating " field " with the dojo isValid() protocol")
                (setf validp (field.is-valid))
                (setf validatedp true))
              (when validatedp
                (ucw.field.set-valid field validp)))))
    (when validator
      ;; TODO it doesn't work for complex dojo widgets like Editor
      (ucw.event.connect field (list "onchange" "onkeyup")
                         (lambda (e)
                           ((slot-value field 'ucw-validate))
                           (return true))))
    (when validate-initially
      (.ucw-validate field)))

  (defun set-valid (field valid)
    (when (instanceof field dojo.widget.*widget)
      (setf field field.dom-node))
    (if valid
        (progn
          (dojo.html.remove-class field "ucw-form-field-invalid")
          (dojo.html.add-class field "ucw-form-field-valid"))
        (progn
          (dojo.html.remove-class field "ucw-form-field-valid")
          (dojo.html.add-class field "ucw-form-field-invalid"))))

  (defun setup-simple-checkbox ((checkbox :by-id) (hidden :by-id) enabled-tooltip disabled-tooltip)
    (ucw.event.connect checkbox "onchange"
                       (lambda (event)
                         (let ((enabled checkbox.checked))
                           (log.debug "Propagating checkbox.checked of " checkbox.id " to the hidden field " hidden.id " named " hidden.name)
                           (setf hidden.value (if enabled
                                                  "on"
                                                  ""))
                           (setf checkbox.title
                                 (if enabled
                                     enabled-tooltip
                                     disabled-tooltip)))))
    (setf checkbox.ucw-set-checked (lambda (enabled)
                                     (if (= checkbox.checked enabled)
                                         (return false)
                                         (progn
                                           (setf checkbox.checked enabled)
                                           ;; we need to be in sync, so call onchange explicitly
                                           (checkbox.onchange)
                                           (return true)))))
    (setf checkbox.ucw-is-checked (lambda ()
                                    (return checkbox.checked))))

  (defun setup-custom-checkbox ((link :by-id) (image :by-id) (hidden :by-id)
                                enabled-image enabled-tooltip disabled-image disabled-tooltip)
    (let ((enabled (not (= hidden.value ""))))
      (setf image.src (if enabled
                          enabled-image
                          disabled-image))
      (setf image.title (if enabled
                            enabled-tooltip
                            disabled-tooltip)))
    (setf link.ucw-set-checked (lambda (enabled)
                                 (setf hidden.value (if enabled
                                                        "on"
                                                        ""))
                                 (setf image.src (if enabled
                                                     enabled-image
                                                     disabled-image))
                                 (setf image.title (if enabled
                                                       enabled-tooltip
                                                       disabled-tooltip))))
    (setf link.ucw-is-checked (lambda ()
                                (return (not (= hidden.value "")))))
    (setf link.name hidden.name) ; copy name of the form input
    (setf link.onclick (lambda (event)
                         (link.ucw-set-checked (not (link.ucw-is-checked))))))

  (defun is-number (value)
    (let ((pieces (.split value "/")))
      (return
        (or (not (is-na-n value))
            (and (= pieces.length 2)
                 (not (is-na-n (aref pieces 0)))
                 (not (is-na-n (aref pieces 1)))
                 (not (= 0 (aref pieces 0)))
                 (not (= 0 (aref pieces 1)))))))))

;;;;;;;;;;;;;;;;;;;;;
;;; form

`(into-namespace ucw.form

  (setf _registry (new dojo.collections.*dictionary))
  (setf _form-being-submitted nil) ; this is an internal global needed by default-unload-event-handler
  
  (defun register ((form :by-id))
    (log.info "Registering form " form.id " while registry size is " ucw.form._registry.count)
    (.add ucw.form._registry form.id form)
    (setf form.ucw-form-fields (new dojo.collections.*dictionary)))

  (defun find-registered ((form :by-id))
    (return (.item ucw.form._registry form.id)))

  (defun all-registered ()
    (let ((result (array)))
      (ucw.form.iterate-registered
       (lambda (form)
         (.push result form)))
      (return result)))
  
  (defun iterate-registered (visitor)
    (dolist (form (.get-value-list ucw.form._registry))
      (awhen ($ form.id)
        (if (dojo.dom.is-descendant-of it document.body)
            (let ((result (visitor it)))
              (unless (dojo.lang.is-undefined result)
                (return result)))
            (progn
              (log.debug "Dropping form " it.id " from the registry")
              (.remove ucw.form._registry it.id))))))

  (defun get-all-fields ((form :by-id))
    (return (form.ucw-form-fields.get-value-list)))

  (defun dirty-p ((form :by-id) (default-value false))
    (log.info "Checking dirtyness of form " form.id)
    (awhen form.ucw-dirty-p
      (log.debug "Form " form.id #.(format nil " has a custom dirty-p checker that we are about to call: ~%") it)
      (if (dojo.lang.is-function it)
          (return (it form))
          (return it)))
    (when form.ucw-form-fields
      (dolist (field (form.ucw-form-fields.get-value-list))
        (if ($ field.id)
            (progn
              (log.debug "Checking dirtyness of field " field.id
                         " with ucw-dirty-p " field.ucw-dirty-p)
              ;; TODO this may skip submit buttons and other constant form-fields
              (let ((result default-value))
                (awhen field.ucw-dirty-p
                  (if (dojo.lang.is-function it)
                      (setf result (it field))
                      (setf result it)))
                (when result
                  (log.debug "Field is dirty")
                  (return true))))
            (progn
              (log.debug "Field is not in the DOM anymore, removing from the registry: " field)
              (form.ucw-form-fields.remove field.id))))
      (return false))
    (return default-value))

  (defun ask-user-abandon-handler ((form :by-id))
    (log.info "Default ask-user abandon handler for form " form.id)
    (if (ucw.form.dirty-p form)
        (return (confirm #"confirm-pending-changes"))
        (return true)))

  (defun auto-submit-abandon-handler ((form :by-id) params)
    (log.debug "Default auto-submit abandon handler for form " form.id)
    (when (ucw.form.dirty-p form)
      (setf params (dojo.lang.shallow-copy params))
      (setf params.url ,(strcat (application.url-prefix (context.application *context*))
                                +callback-dispatcher-url+))
      (setf params.method "post")
      (setf params.sync true)
      (setf params.forms-to-submit (array form))
      ;; in case of an error we simply allow the abandoning (this is a question of policy)
      (ucw.io.bind params))
    (return true))

  (defun get-default-action ((form :by-id))
    (return form.default-ucw-action))
  
  (defun set-default-action ((form :by-id) (action :by-id))
    (log.debug "Setting default action " action " on form " form)
    (setf form.default-ucw-action action))

  (defun has-default-action ((form :by-id))
    (return (not (= (ucw.form.get-default-action form) nil))))

  (defun set-action ((form :by-id) url session-id frame-id action-id progress-node)
    (log.debug "Setting form action of " form " to " url ", " session-id ", " frame-id ", " action-id)
    (ucw.decorate-with-invocation-id form)
    (let ((pos (.index-of url "?")))
      (when (>= pos 0)
        (setf url (.substring url 0 pos))))
    (setf form.action (ucw.absolute-url-from url))
    (let ((session-input (slot-value form ,+session-parameter-name+))
          (frame-input (slot-value form ,+frame-parameter-name+))
          (action-input (slot-value form ,+action-parameter-name+)))
      (assert (and session-input frame-input action-input))
      (setf session-input.value session-id)
      (setf frame-input.value frame-id)
      (setf action-input.value action-id)
      (when progress-node
        (setf form._ucw-progress-node progress-node))))
  
  (defun get-action ((form :by-id))
    (return form.action))
  
  (defun has-action ((form :by-id))
    (return (not (dojo.string.is-blank (ucw.form.get-action form)))))
  
  (defun default-onsubmit ((form :by-id))
    (macrolet ((hide-progress ()
                 `(awhen form._ucw-progress-node
                   (ucw.io.progress.remove it))))
      (log.info "Onsubmit of " form)
      (setf ucw.form._form-being-submitted form)
      (ucw.decorate-with-invocation-id form)
      ;; TODO it is not clear which one should have priority, and what does it mean to have a default action and a form action
      ;; maybe the default action concept should be dropped? that would probably be less flexible, though...
      (when (ucw.form.has-action form)
        (log.debug "Form has an action annotated, submitting " (ucw.form.get-action form))
        (return true))
      (when-bind default-node (ucw.form.get-default-action form)
        (log.debug "Form has a default action: " default-node ", submitting it")
        (default-node.onclick))
      (hide-progress)
      (return false))))

;;;;;;;;;;;;;;;;;;;;;
;;; io

`(into-namespace ucw.io

  (defun postprocess-inserted-node (original-node imported-node)
    (log.debug "Parsing dojo widgets under " imported-node.id)
    (dojo.widget.create-widget imported-node)
    ;; seems like IE drops script nodes when setf'ing node.inner-h-t-m-l, so let's walk the original-node
    ;; this must be another branidead try to make m$ crap secure...
    (if dojo.render.html.opera
        (progn
          ;; TODO check other Opera versions, this is probably version dependent...
          (log.warn "NOT evaluating script tags, Opera does that automatically, version is " navigator.app-version))
        (progn
          (log.debug "Evaluating script tags under " original-node.id)
          (ucw.io.eval-script-tags original-node))))

  (defun eval-script-tags (node)
    (try
     (progn
       (log.debug "About to look for and eval some script tags")
       (ucw.io.eval-script-tags-rec (if (dojo.lang.is-array-like node)
                                        node
                                        (array node)))
       (log.debug "Eval'ing script tags was successful"))
     (:catch (e)
       (log.error "Error while eval'ing script tags: " e.message))))

  (defun eval-script-tags-rec (nodes)
    (dolist (node nodes)
      ;;(log.debug "Script tag? " node.tag-name)
      (when (dojo.html.is-tag node "script")
        (let ((type (node.get-attribute "type")))
          (if (= type "text/javascript")
              (let ((script node.text))
                (unless (dojo.string.is-blank script)
                  ;;(log.debug "Eval'ing script " (.substring script 0 128))
                  (with-ucw-error-handler
                      (eval script))))
              (throw (+ "Script tag with unexpected type: '" type "'")))))
      (arguments.callee node.child-nodes)))

  (defun bind (params)
    (setf params (dojo.lang.shallow-copy params))
    (macrolet ((default (name value)
                   `(when (= (slot-value params ',name) undefined)
                     (setf (slot-value params ',name) ,value))))
      
      (default method "post")
      (default sync false) ;; TODO make true the default and if true then find a way to numb event handlers meanwhile
      (default mimetype "text/xml")
      (default error ucw.io.process-ajax-error)
      (default load ucw.io.process-ajax-answer)
      (default encoding ,(symbol-name (application.charset (context.application *context*))))
      (default content (create))

      (setf params.content (dojo.lang.shallow-copy params.content))

      ;; submit some forms as per caller request
      (when (and params.forms-to-submit
                 (> params.forms-to-submit.length 0))
        (dolist (form params.forms-to-submit)
          (log.debug "Gathering values from form " form.id " as requested by :forms-to-submit")
          (dolist (field (ucw.form.get-all-fields form))
            (let ((value (ucw.field.get-value field))
                  (name))
              (if (instanceof field dojo.widget.*widget)
                  (macrolet ((wcase (&rest clauses)
                               `(cond ,@(iter (for entry :in clauses)
                                              (for test = (first entry))
                                              (for body = (rest entry))
                                              (collect `(,(if (eq test t)
                                                              t
                                                              `(and ,test
                                                                    (instanceof field ,test)))
                                                         ,@body))))))
                    (wcase (dojo.widget.*editor2
                            (setf name field.textarea.name))
                           (t
                            ;; the dojo guys say this is ok for any widget: http://trac.dojotoolkit.org/ticket/3283
                            (setf name field.name))))
                  (setf name field.name))
              (log.debug "Value of " field ", named" name " is " value)
              (assert name)
              (setf (slot-value params.content name) value)))))

      (when (and params.url
                 params.session-id)
        (log.debug "Decorating ucw.io.bind url with session, frame and action params. Before decoration url is: " params.url)
        (setf params.url (ucw.decorate-with-invocation-id
                          (ucw.decorate-url-with-session-and-frame
                           params.url params.session-id params.frame-id params.action-id))))

      ;; absolutize url if it's a relative one.
      (when params.url
        (setf params.url (ucw.absolute-url-from params.url)))

      (let ((progress-label-remover (lambda ()
                                      (awhen params.progress-node
                                        (ucw.io.progress.remove it)))))
        (ucw.event.kw-connect (create
                               :src-obj params
                               :src-func (list "load" "error")
                               :advice-type "before"
                               :advice-func progress-label-remover))

        (log.debug "Calling dojo.io.bind with " params)
        (let ((result (dojo.io.bind params)))
          (dojo.event.connect-before params "abort" (lambda ()
                                                      (progress-label-remover)
                                                      (setf ucw.io.polling.enabled-p false)))
          (return result)))))

  (defun eval-js-at-url (url error-handler)
    (ucw.io.bind (create :sync true
                         :url url
                         :session-id ucw.session-id
                         :frame-id ucw.frame-id
                         :load (lambda (type data event)
                                 (log.debug "About to eval received script in eval-js-at-url")
                                 (eval data))
                         :error error-handler
                         :mimetype "text/plain"
                         :method "get")))
  
  ;; the accepted extra params over dojo.io.bind are:
  ;; :handler :error-handler :mimetype :method :encoding :forms-to-submit
  ;; :forms-to-abandon :forms-to-ask :progress-label
  ;; when forms-to-submit contains a single form then the action is submitted together with the form
  (defun execute-ajax-action (params)
    (with-ucw-error-handler
      (macrolet ((only-one-of (primary secondary &key (defaulting t))
                   (let ((primary-name (strcat ":" (string-downcase (symbol-name primary))))
                         (secondary-name (strcat ":" (string-downcase (symbol-name secondary)))))
                     `(if (slot-value params ',primary)
                       (when (slot-value params ',secondary)
                         (log.debug "WARNING: ajax-action got params with both " ,primary-name " and "
                                    ,secondary-name "! Ignoring " ,secondary-name "..."))
                       ,(when defaulting
                              `(setf (slot-value params ',primary) (slot-value params ',secondary))))))
                 (default (name value)
                     `(unless (slot-value params ',name)
                       (setf (slot-value params ',name) ,value))))
        
        ;; do some sanity checks, defaulting and by-id lookups on the params
        (only-one-of load handler)
        (only-one-of error error-handler)
        (only-one-of mimetype mime-type)
        (default forms-to-ask (ucw.form.all-registered))
        (default forms-to-abandon (array))
        (default forms-to-submit (array))

        ;; TODO the extra args we are using (like handler) should be removed before calling dojo to avoid possible conflicts

        (log.debug "execute-ajax-action with params:")
        (when (dojo.logging.log.is-enabled-for (dojo.logging.log.get-level "debug"))
          (dojo.debug params))

        ;; accept the elements of these arrays as id's, too
        (map-into dojo.by-id params.forms-to-abandon)
        (map-into dojo.by-id params.forms-to-submit)
        (map-into dojo.by-id params.forms-to-ask)

        ;; and now ask some forms for permission to abandon the page
        (setf params.forms-to-ask (set-difference params.forms-to-ask params.forms-to-abandon))
        (log.info "AJAX onclick will ask " params.forms-to-ask.length " forms for permission to abandon the page")
        (unless (ucw.may-abandon-the-page params.forms-to-ask params.forms-to-submit)
          (return false))

        (log.debug "Triggering AJAX action " params.url " with progress label " params.progress-label)

        ;; display the progress indicator
        ;; TODO for async requests, we could propagate params.abort to the progress code after bind was called and support aborting
        (when (and params.progress-label
                  (not (= params.progress-label "")))
          (setf params.progress-node (ucw.io.progress.display params.progress-label)))
        ;; let's fire the AJAX request!
        (let ((ok false))
          (try
           (let ((result (ucw.io.bind params)))
             (setf ok true)
             (return result))
           (:finally
            (unless ok
              ;; if some error happened before firing the request, so noone will call our error handler to remove it
              (awhen params.progress-node
                (ucw.io.progress.remove it)))))))))

  ;; Makes an XMLHTTP-received node suitable for inclusion in the document.
  (defun import-ajax-received-xhtml-node (node)
    (log.debug "Importing ajax answer node with id " (.get-attribute node "id"))
    (cond
      (dojo.render.html.opera
       (return (document.import-node node true)))

      (dojo.render.html.ie
       ;; ie is randomly dropping the script tags (m$ is as lame as usual...)
       ;; i couldn't find anything that affects the behaviour, my best guess is that it may depend
       ;; on how the script reached the browser: scripts in the original document may have
       ;; more permissions? either way, handle script tags specially to overcome it.
       (let ((result))
         (cond ((dojo.html.is-tag node "script")
                (setf result (document.create-element "script"))
                (setf result.src           node.src
                      result.type          node.type
                      result.language      node.language
                      result.inner-t-e-x-t node.inner-t-e-x-t))
               (t
                ;; create a node and setf its innerXML property
                ;; this will parse the xhtml we received and convert it
                ;; to dom nodes that ie will not bark on.
                (cond ((dojo.html.is-tag node "tr")
                       ;; TODO
                       (log.warn "Trying to import a TR node, this is broken on IE")
                       (setf result (document.create-element "table")))
                      (t (setf result (document.create-element "div"))))
                (log.debug "Asssigning innerHTML")
                (setf result.inner-h-t-m-l node.xml)
                (log.debug "innerHTML was assigned succesfully")
                (assert (= 1 result.child-nodes.length))
                (setf result (dojo.dom.first-element result))))
         (log.debug "Succesfully imported answer node, returning")
         (return result)))
      (dojo.render.html.mozilla
       (return node)))

    (log.warn "Unknown browser in import-ajax-received-xhtml-node, this will probably cause some troubles later. Browser is " navigator.user-agent)
    (return node))

  ;; Return a lambda that when passed a root node, will call the visitor with each of those children
  ;; that have the given tag-name.
  (defun make-node-walker (tag-name visitor (import-node-p true) (toplevel-p false))
    (return (lambda (root)
              (dolist (toplevel-node root.child-nodes)
                (log.debug "Walking at node " toplevel-node.tag-name)
                ;; node.get-elements-by-tag-name returns recursively all nodes of a document node, so that won't work here
                (when (= toplevel-node.tag-name tag-name)
                  (if toplevel-p
                      (let ((node toplevel-node)
                            (original-node node)
                            (id (.get-attribute node "id")))
                        (log.debug "Processing " tag-name " node with id " id)
                        (when import-node-p
                          (setf node (ucw.io.import-ajax-received-xhtml-node node)))
                        (visitor node original-node))
                      (progn
                        (log.debug "Will process " toplevel-node.child-nodes.length " nodes of type '" tag-name "'")
                        (dolist (node (dojo.lang.to-array toplevel-node.child-nodes)) ; create a copy and iterate on that
                          (let ((original-node node)
                                (id (.get-attribute node "id")))
                            (log.debug "Processing " tag-name " node with id " id)
                            (when import-node-p
                              (setf node (ucw.io.import-ajax-received-xhtml-node node)))
                            (visitor node original-node))))))))))

  ;; Returns a lambda that can be used as a dojo.io.bind :load handler.  Will do some sanity checks
  ;; on the ajax answer, report any possible server errors, then walk the nodes with the given
  ;; tag-name and call the visitor on them.  If the visitor returns a node, then postprocess the
  ;; returned node as an added dom html fragment.
  (defun make-node-walking-ajax-answer-processor (tag-name visitor (import-node-p true) (toplevel-p false))
    (let ((node-walker (ucw.io.make-node-walker tag-name
                                                (lambda (node original-node)
                                                  (when (visitor node original-node)
                                                    (log.debug "Calling postprocess-inserted-node on node " node.tag-name)
                                                    (ucw.io.postprocess-inserted-node original-node node)))
                                                import-node-p
                                                toplevel-p)))
      (return (lambda (type data event)
                (log.debug "Type is" type)
                (log.debug "Data is" data)
                (with-ajax-answer (data)
                  (node-walker data))))))
  
  (let ((dom-replacer
         (ucw.io.make-node-walking-ajax-answer-processor "dom-replacements"
          (lambda (replacement-node)
            (let ((id (.get-attribute replacement-node "id")))
              (cond ((and id ($ id))
                     (let ((old-node ($ id))
                           (parent-node (slot-value old-node 'parent-node)))
                       (dojo.html.hide old-node)
                       (log.debug "About to replace old node with id " id)
                       (.replace-child parent-node replacement-node old-node)
                       (log.debug "Successfully replaced node with id " id)
                       (return true)))
                    ((dojo.html.is-tag replacement-node "script")
                     (log.debug "Found a toplevel script node in dom-replacements, calling eval...")
                     (ucw.io.eval-script-tags replacement-node))
                    (t (log.warn "Replacement node with id '" id "' was not found on the client side"))))))))
    (setf ucw.io.process-ajax-answer
          (lambda (type data event)
            (with-ucw-error-handler
              ;; replace some components (dom nodes)
              (log.debug "Calling dom-replacer...")
              (dom-replacer type data event)
              (log.debug "...dom-replacer returned")
              
              ;; look for 'script' tags and execute them with 'current-ajax-answer' bound
              (let ((script-evaluator
                     (ucw.io.make-node-walking-ajax-answer-processor "script"
                      (lambda (script-node)
                        ;; TODO handle/assert for script type attribute
                        (let ((script (dojo.dom.text-content script-node)))
                          (log.debug "About to eval AJAX-received script " #\Newline script)
                          ;; isolate the local bindings from the script to be executed
                          ;; and only bind with the given name what we explicitly list here
                          ((lambda (_script current-ajax-answer)
                             (eval _script)) script data)
                          (log.debug "Finished eval-ing AJAX-received script")))
                      false true)))
                (log.debug "Calling script-evaluator...")
                (script-evaluator type data event)
                (log.debug "...script-evaluator returned"))))))

  (defun process-ajax-error (type error)
    (log.error "Processing AJAX error, type " type ", error " error)
    ,(if (debug-on-error (context.application *context*))
         `debugger
         `(alert #"network-error"))))

;;;;;;;;;;;;;;;;;;;;;
;;; polling

`(into-namespace ucw.io.polling
  
  (setf enabled-p false)
  (setf delay ,*default-polling-delay*)
  
  (defun set-delay (delay)
    (unless (= ucw.io.polling.delay delay)
      (log.debug "Changing polling delay from " ucw.io.polling.delay " to " delay)
      (setf ucw.io.polling.delay delay)))

  (defun stop ()
    (log.debug "Stopping polling")
    (setf ucw.io.polling.enabled-p false)
    (awhen ucw.io.polling.timer
      (window.clear-timeout it)
      (setf ucw.io.polling.timer nil))
    (awhen ucw.io.polling.aborter
      (it)))

  (defun is-polling ()
    (return ucw.io.polling.enabled-p))

  (defun start (delay)
    (log.debug "ucw.io.polling.start while is-polling is " (ucw.io.polling.is-polling))
    (when delay
      (ucw.io.polling.set-delay delay))
    (unless (ucw.io.polling.is-polling)
      (setf ucw.io.polling.enabled-p true)
      (ucw.io.polling.wind-up-timer)))

  (defun wind-up-timer ((func ucw.io.polling.poller))
    (log.debug "wind-up-timer winding up the timer with delay " ucw.io.polling.delay)
    (if (> ucw.io.polling.delay 0)
        (setf ucw.io.polling.timer
              (window.set-timeout func ucw.io.polling.delay))
        (func)))
  
  (defun poller ()
    (unless (> ucw.io.polling.delay ,*default-polling-delay*)
      (ucw.io.polling.set-delay ,*default-polling-delay*))
    (log.debug "Poller speeking")
    (let ((params (create :url (ucw.decorate-url-with-session-and-frame
                                ,(strcat (application.url-prefix (context.application *context*))
                                         +polling-dispatcher-url+
                                         "?"))
                          :forms-to-abandon (ucw.form.all-registered) ; TODO this could be much smarter here, auto submitting forms, etc
                          :error-handler (lambda (type error)
                                           (log.warn "An error happened while polling, type " type ", error " error)
                                           (when ucw.io.polling.enabled-p
                                             (when (< ucw.io.polling.delay #.(* 60 1000))
                                               (ucw.io.polling.set-delay (+ ucw.io.polling.delay #.(* 5 1000))))
                                             (ucw.io.polling.wind-up-timer)))
                          :handler (lambda ()
                                     (.apply ucw.io.process-ajax-answer this arguments)
                                     (when ucw.io.polling.enabled-p
                                       (ucw.io.polling.wind-up-timer)))
                          :progress-label ""
                          :sync false)))
      (ucw.io.execute-ajax-action params)
      (setf ucw.io.polling.aborter params.abort)
      (log.debug "Poller triggered ajax request and exiting"))))

;;;;;;;;;;;;;;;;;;;;;
;;; i18n

`(into-namespace ucw.i18n

  (setf resources (create))
  
  (defun lookup (name)
    (let ((value (aref ucw.i18n.resources name)))
      (unless value
        (log.error "Resource not found for key '" name "'")
        (setf value name))
      (return value)))

  (defun define ()
    (setf names-and-values arguments)
    (log.debug "Defining " names-and-values.length " i18n resources")
    (do ((idx 0 (+ idx 2)))
        ((>= idx names-and-values.length))
      (let ((name (aref names-and-values idx))
            (value (aref names-and-values (1+ idx))))
        (setf (aref ucw.i18n.resources name) value)))))

;;;;;;;;;;;;;;;;;;;;;
;;; progress

`(into-namespace ucw.io.progress

  (dojo.add-on-load (lambda ()
                      (let ((node (document.create-element "div")))
                        (setf node.id "ucw-progress-container")
                        (.append-child (dojo.body) node)
                        (setf ucw.io.progress.container node))))

  (defun display ((label #"progress-label.default"))
    (unless ucw.io.progress.container
      (log.warn "ucw.io.progress is not set up, not showing progress indicator")
      (return undefined))
    (log.debug "Displaying progress label '" label "'")
    (let ((node (document.create-element "div")))
      (dojo.html.add-class node "ucw-progress-indicator")
      (setf node.inner-h-t-m-l label)
      (dojo.html.set-opacity node 0.1)
      (setf node.title #"progress.tooltip")
      ;; install an onclick handler that removes the node in case it got stuck
      (dojo.event.connect node "onclick"
                          (lambda ()
                            (dojo.dom.remove-node node)))
      ;; set up a timer to avoid flickering of fast actions
      (setf node.ucw-progress-timer
            (window.set-timeout (lambda ()
                                  (.append-child ucw.io.progress.container node)
                                  (setf node.ucw-fade-animation (dojo.lfx.html.fade-in node 300))
                                  (.play node.ucw-fade-animation))
                                200))
      (return node)))

  (defun remove (node)
    (unless node
      (return undefined))
    (log.debug "Removing progress label " node)
    (window.clear-timeout node.ucw-progress-timer)
    (when node.parent-node
      (when node.ucw-fade-animation
        (.stop node.ucw-fade-animation))
      (.play (dojo.lfx.html.fade-out node 300 undefined
                                     (lambda ()
                                       (dojo.dom.remove-node node)))))))

(when (find-class 'i18n-parenscript-dispatcher nil)
  `(progn
    (let ((url ,(strcat (application.url-prefix (context.application *context*))
                        +i18n-parenscript-dispatcher-url+)))
      (log.info "About to download and eval i18n js at " url)
      (ucw.io.eval-js-at-url url (lambda (type error)
                                   (log.warn "Failed to download i18n js" type ", error " error)))
      (log.info "Sucessfully downloaded and eval'd i18n js"))))

`(log.debug "per-application.js is done")
