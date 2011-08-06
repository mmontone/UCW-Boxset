;; -*- lisp -*-
;; See the file LICENCE for licence information.

(in-package :it.bese.ucw)

(defmacro with-lock-held-on-server (server &body body)
  (rebinding (server)
    `(multiple-value-prog1
      (progn
        (ucw.rerl.threads.dribble "Entering with-lock-held-on-server for server ~S in thread ~S" ,server (current-thread))
        (with-recursive-lock-held ((lock-of ,server))
          ,@body))
      (ucw.rerl.threads.dribble "Leaving with-lock-held-on-server for server ~S in thread ~S" ,server (current-thread)))))

(defmacro with-lock-held-on-application (app &body body)
  (rebinding (app)
    `(multiple-value-prog1
      (progn
        (ucw.rerl.threads.dribble "Entering with-lock-held-on-application for app ~S in thread ~S" ,app (current-thread))
        (with-recursive-lock-held ((lock-of ,app))
          ,@body))
      (ucw.rerl.threads.dribble "Leaving with-lock-held-on-application for app ~S in thread ~S" ,app (current-thread)))))

(defmacro with-lock-held-on-session (session &body body)
  (rebinding (session)
    `(multiple-value-prog1
      (progn
        (ucw.rerl.threads.dribble "Entering with-lock-held-on-session for ~S in thread ~S" ,session (current-thread))
        (with-recursive-lock-held ((lock-of ,session))
          ,@body))
      (ucw.rerl.threads.dribble "Leaving with-lock-held-on-session for ~S in thread ~S" ,session (current-thread)))))

(defmacro with-session-variables ((&rest names) &body body)
  "Create local bindings for the listed NAMES and set them to
\(session.value ',name session\). If BODY gracefully completes then
save the values of the local variables back into the session."
  (with-unique-names (session)
    `(let ((,session (context.session *context*)))
      (let (,@(iter (for name in names)
                    (collect `(,name (session.value ',name ,session)))))
        (multiple-value-prog1
            (progn
              ,@body)
          ,@(iter (for name in names)
                  (collect `(setf (session.value ',name ,session) ,name))))))))

(defun send-redirect (target &optional (response (context.response *context*)))
  (setf (get-header response "Status") +http-moved-temporarily+
        (get-header response "Location") target)
  (<:html
   (<:head
    (<:title "302 - Redirect"))
   (<:body
    (<:p "Page has moved to "
         (<:a :href target (<:ah target))))))

(defun make-standard-ucw-www-root-list ()
  (let ((ucw-home (asdf:component-pathname (asdf:find-system :ucw))))
    (list (cons "static/ucw/" (merge-pathnames "wwwroot/ucw/" ucw-home))
          (cons "static/dojo/" (merge-pathnames "wwwroot/dojo/" ucw-home)))))

(defun make-standard-ucw-tal-dir-list ()
  (let ((ucw-home (asdf:component-pathname (asdf:find-system :ucw))))
    (list (merge-pathnames "wwwroot/ucw/tal/" ucw-home))))

