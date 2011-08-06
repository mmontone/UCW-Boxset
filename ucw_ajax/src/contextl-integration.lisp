(in-package :ucw)

(eval-always
  (use-package :contextl :ucw))

(export '(contextl-context-capturing-ajax-component-mixin
          contextl-aware-widget-component
          layered-component-mixin)
        :ucw)

(defcomponent contextl-context-capturing-ajax-component-mixin (ajax-component-mixin)
  ((render-time-layer-context
    :initform nil
    :accessor render-time-layer-context-of))
  (:documentation "A component mixin that captures the layer context when it's first rendered and restores it at later renderings."))

(defmethod call-in-rendering-environment :around ((self contextl-context-capturing-ajax-component-mixin) trunk)
  (aif (render-time-layer-context-of self)
       (progn
         (ucw.component.layers.dribble "Reinstating layer context of ~A to ~A" self it)
         (multiple-value-prog1
             (funcall-with-layer-context it #'call-next-method)
           (ucw.component.layers.dribble "Leaving the reinstated layer context of ~A" self)))
       (let ((current-layer-context (current-layer-context)))
         (ucw.component.layers.dribble "Saving the layer context ~A into ~A" current-layer-context self)
         (setf (render-time-layer-context-of self) current-layer-context)
         (call-next-method))))

(defcomponent contextl-aware-widget-component (contextl-context-capturing-ajax-component-mixin
                                               widget-component)
  ())

(defcomponent layered-component-mixin ()
  ((layer
    :initarg :layer
    :accessor layer-of)))

(defmethod call-in-rendering-environment :around ((self layered-component-mixin) trunk)
  (ucw.component.layers.dribble "Entering layer ~A which is wrapping component ~A" (layer-of self) self)
  (funcall-with-layer-context (adjoin-layer (layer-of self)
                                            (current-layer-context))
                              #'call-next-method)
  (ucw.component.layers.dribble "Leaving layer ~A that was wrapping component ~A" (layer-of self) self))

;; TODO delme?
#+nil((defclass contextl-context-capturing-action-mixin ()
  ((render-time-layer-context
    :initform nil
    :accessor render-time-layer-context-of))
  (:documentation "An action mixin that captures the layer context at
instantiation time and restores it when the action is executed."))

(defmethod register-action-in-frame :before (frame (action contextl-context-capturing-action-mixin))
  (setf (render-time-layer-context-of action) (current-layer-context)))

(defmethod call-action :around ((action contextl-context-capturing-action-mixin)
                                application session frame)
  (let ((previous-contextl-context (render-time-layer-context-of action)))
    (assert previous-contextl-context () "This should not happen: we are execuring a contextl-context-capturing-action-mixin without a valid render-time-layer-context slot.")
    (ucw.component.layers.debug "Reinstating layer context in CALL-ACTION of ~A" action)
    (funcall-with-layer-context previous-contextl-context #'call-next-method)
    (ucw.component.layers.debug "Leaving the reinstated layer context of CALL-ACTION of ~A" action)))

(defclass contextl-aware-ajax-action (contextl-context-capturing-action-mixin
                                      ajax-action)
  ()
  (:metaclass mopp:funcallable-standard-class)))

