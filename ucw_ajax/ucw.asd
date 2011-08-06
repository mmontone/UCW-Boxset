;;; -*- lisp -*-

;;;; ASDF system definition file for UCW


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :it.bese.ucw.system)
    (defpackage :it.bese.ucw.system
      (:nicknames #:ucw.system)
      (:export #:*load-as-production-p*)
      (:use :common-lisp :asdf)))

  ;;; try to load asdf-system-connections
  (unless (asdf:find-system :asdf-system-connections nil)
    (when (find-package :asdf-install)
      (eval (read-from-string "(asdf-install:install '#:asdf-system-connections)")))
    (unless (asdf:find-system :asdf-system-connections nil)
      (error "The UCW system requires asdf-system-connections. See http://www.cliki.net/asdf-system-connections for details and download instructions.")))
  (asdf:operate 'asdf:load-op :asdf-system-connections))

(in-package :it.bese.ucw.system)

(defparameter *load-as-production-p* t
  "When T, load the UCW lisp files so that it will be used in a production system.
This means that debug-only blocks are skipped and various variables are initialized accordingly.")

(defclass ucw-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component ucw-source-file))
  (let ((*features* *features*))
    (unless *load-as-production-p*
      (pushnew :debug *features*))
    (call-next-method)))

(defclass ucw-system (system)
  ((test-system :initform :ucw.core.test :initarg :test-system :accessor test-system-of)))

(defmacro defsystem* (name &body args)
  `(defsystem ,name :default-component-class ucw-source-file
    ,@args))

(defsystem* :ucw.core
  :description "Core features of UnCommon Web"
  :long-description "Contains the base features essential for a useful
Read Eval Render Loop (RERL)."
  :author "Marco Baringer <mb@bese.it>"
  :licence "BSD (sans advertising clause)"
  :version "0.4"
  :class ucw-system
  :test-system :ucw.core.test
  :components
  ((:module :src
    :components ((:file "packages")
		 (:file "config" :depends-on ("packages" "vars"))
                 (:file "helpers" :depends-on ("packages" "vars"))
                 (:file "loggers" :depends-on ("packages" "vars"))
                 (:file "vars" :depends-on ("packages"))
                 (:file "control" :depends-on ("config" :backend :rerl))
                 (:module :backend
                  :components ((:file "accept-headers"))
                  :depends-on ("packages" "loggers" :rerl))
                 (:module :rerl
                  :components ((:file "protocol")
                               (:file "rerl-variables")
                               (:file "rerl-utils" :depends-on ("protocol" "rerl-variables"))
                               (:file "conditions" :depends-on ("protocol"))
                               (:file "backtracking" :depends-on ("basic-classes"))
                               (:file "request-loop-error" :depends-on ("conditions" "rerl-utils" "basic-action"))
                               (:file "basic-classes" :depends-on ("protocol"
                                                                   "rerl-variables"))
                               (:file "basic-action" :depends-on ("protocol"
                                                                  "standard-session-frame"
                                                                  "basic-classes"))
                               (:file "basic-application" :depends-on ("rerl-utils"
                                                                       "basic-classes"))
                               (:module :standard-component
                                        :components ((:file "standard-component" :depends-on ("standard-component-class"))
                                                     (:file "control-flow" :depends-on ("standard-component"))
                                                     (:file "standard-component-class")
                                                     (:file "transactions" :depends-on ("standard-component")))
                                        :depends-on ("backtracking"
                                                     "rerl-utils"
                                                     "request-loop-error"
                                                     "basic-application"
                                                     "standard-session-frame"
                                                     "basic-action"
                                                     "basic-classes"))
                               (:file "basic-dispatchers" :depends-on ("request-loop-error"
                                                                       "basic-application"
                                                                       "basic-action"))
                               (:file "standard-request-context" :depends-on ("rerl-utils"
                                                                              "basic-classes"
                                                                              :standard-component))
                               (:file "standard-server" :depends-on ("rerl-utils"
                                                                     "request-loop-error"
                                                                     "basic-classes"))
                               (:file "basic-session" :depends-on ("rerl-utils"
                                                                   "basic-classes"
                                                                   "standard-session-frame"))
                               (:file "standard-session-frame" :depends-on ("rerl-utils"
                                                                            "backtracking"
                                                                            "basic-classes")))
                  :depends-on ("packages" "loggers" "helpers" "vars")))))
  :properties ((version "0.4"))
  :depends-on (:arnesi :swank :iterate
               :usocket :rfc2109 :net-telent-date :cl-fad
               :trivial-garbage :bordeaux-threads
               ;; the rest is needed until package.lisp is cut properly
               :yaclml :local-time
               ))

(defsystem* :ucw.basic
  :author "Marco Baringer <mb@bese.it>"
  :licence "BSD (sans advertising clause)"
  :version "0.4"
  :class ucw-system
  :components
  ((:module :src
    :components ((:file "parenscript-utils")
                 (:module :rerl
                  :components ((:file "standard-classes")
                               (:file "standard-session")
                               (:file "standard-action" :depends-on ("standard-classes"))
                               (:file "standard-dispatchers" :depends-on ("standard-classes"
                                                                          "standard-action"))
                               (:file "standard-application" :depends-on ("standard-classes"))))
                 (:module :yaclml
                  :components ((:file "tal")
                               (:file "ucw-tags")
                               (:file "dojo-tags")
                               (:file "yaclml"))
                  :depends-on (:rerl)))))
  :properties ((version "0.4"))
  :depends-on (:ucw.core :yaclml :swank :iterate :parenscript :cl-ppcre
               :usocket :rfc2109 :net-telent-date :cl-fad
               :trivial-garbage :bordeaux-threads))

(defsystem* :ucw
  :description "UnCommon Web - A Common Lisp Web Application Framework."
  :long-description "A continuation based, component oriented
dynamic web application framework written in Common Lisp."
  :author "Marco Baringer <mb@bese.it>"
  :licence "BSD (sans advertising clause)"
  :version "0.4"
  :class ucw-system
  :test-system :ucw.test
  :components
  ((:module :src
            :components ((:file "parenscript-utils")
                         (:file "captcha")
                         (:module :application-mixins
                                  :components ((:file "cookie-session-application")
                                               (:file "secure-application")
                                               (:file "ajax-application"))
                                  :serial t)
                         (:module :components
                                  :components ((:file "user-login" :depends-on ("form"
                                                                                "status-bar"
                                                                                "window"))
                                               (:file "ajax")
                                               (:file "container" :depends-on ("widget"))
                                               (:file "collapsible-pane" :depends-on ("widget"))
                                               (:file "cached")
                                               (:file "error" :depends-on ("ucw-inspector" "window"))
                                               (:file "form" :depends-on ("widget"))
                                               (:file "login")
                                               (:file "message")
                                               (:file "meta-refresh-component")
                                               (:file "task")
                                               (:file "option-dialog" :depends-on ("template"))
                                               (:file "range-view" :depends-on ("template"))
                                               (:file "redirect")
                                               (:file "status-bar")
                                               (:file "tabbed-pane" :depends-on ("container" "template"))
                                               (:file "template")
                                               (:file "transaction-mixin")
                                               (:file "ucw-inspector")
                                               (:file "widget" :depends-on ("html-element"))
                                               (:file "dojo-widgets" :depends-on ("widget" "ajax" "form"))
                                               (:file "html-element")
                                               (:file "window"))
                                  :depends-on (:application-mixins "parenscript-utils")))))
  :properties ((version "0.4"))
  :depends-on (:ucw.basic :yaclml :swank :iterate :parenscript :cl-ppcre
               :usocket :rfc2109 :net-telent-date :cl-fad
               :trivial-garbage :bordeaux-threads :local-time))

;; UCW applications included in ucw itself

(defsystem* :ucw.admin
    :components ((:module :src
                  :pathname "src/admin/"
                  :components ((:file "admin")
                               (:file "admin-inspector"))))
    :depends-on (:ucw))

(defsystem* :ucw.examples
    :components ((:module :examples
		  :components ((:module :src
                                        :components ((:file "examples")
                                                     (:file "counter" :depends-on ("examples"))
                                                     (:file "cache" :depends-on ("examples"))
                                                     (:file "forms" :depends-on ("examples"))
                                                     (:file "sum" :depends-on ("examples"))
                                                     (:file "shared-counter"))))))
    :depends-on (:ucw))

;; Backends

(defsystem* :ucw.httpd
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "common")
                             (:file "basic-backend" :depends-on ("common"))
                             (:file "httpd" :depends-on ("basic-backend" "common"))
                             (:file "multithread-httpd" :depends-on ("httpd")))))
  :depends-on (:ucw.core :rfc2388-binary :puri :cl-ppcre))

(defsystem* :ucw.mod-lisp
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "mod-lisp"))))
  :depends-on (:ucw.core :ucw.httpd))

(defsystem* :ucw.iolib
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "common")
                             (:file "basic-backend" :depends-on ("common"))
                             (:file "iolib" :depends-on ("basic-backend" "common")))))
  :depends-on (:ucw.core :rfc2388-binary :puri :net.sockets :cl-ppcre))

(defsystem* :ucw.aserve
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "aserve" :depends-on ("aserve-locator"))
                             (:file "aserve-locator"))))
  :depends-on (:ucw.core :aserve :cl-ppcre))

(defsystem* :ucw.araneida
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "common")
                             (:file "araneida" :depends-on ("common")))))
  :depends-on (:ucw.core :araneida :rfc2388-binary :cl-ppcre))

;; This ensures that we're loading the right versions of arnesi and
;; yaclml (add similar code for rfc2388, mod_lisp, aserve et al. when
;; they have them).

(defun ensure-system-has-feature
    (system-name version-string &optional (hint ""))
  (let* ((features (asdf:component-property (asdf:find-system system-name) :features))
         (message (format nil "UCW requires the ~A feature of system ~S.
~S currently only provides ~S.~%~A"
                          version-string system-name system-name features hint)))
    (unless (member version-string features :test #'string-equal)
      (error message))))

(defmethod perform :before ((op t) (system ucw-system))
  (ensure-system-has-feature :arnesi "cc-interpreter"
                             "Try pull'ing the latest arnesi or send an email to bese-devel@common-lisp.net")
  (ensure-system-has-feature :arnesi "getenv"
                             "Try pull'ing the latest arnesi or send an email to bese-devel@common-lisp.net")
  (ensure-system-has-feature :yaclml "v0.5.2"
                             "Try pull'ing the latest yaclml or send an email to bese-devel@common-lisp.net"))

(defmethod perform :around ((o t) (system ucw-system))
  (progv
      (list
       (read-from-string "arnesi:*call/cc-returns*")
       ;; If you want the walker to warn about undefined variables and
       ;; functions change this to T. Since this code "breaks" (sort of)
       ;; loading ucw with ASDF on SBCL we leave it off by default.
       (read-from-string "arnesi:*warn-undefined*"))
      (list nil nil)
    (call-next-method)))

;;; Export the variables in the ucw.system package, so that between
;;; (asdf:find-system :ucw) and (asdf:oos 'asdf:load-op :ucw) users
;;; get the chance to set these variables when loading UCW
;;; programmatically. For more details on the variables themselves see
;;; src/vars.lisp

(macrolet ((def (&rest names)
               `(progn
                 ,@(loop for name in names
                         collect `(defvar ,name)
                         collect `(export ',name)))))
  (def
    *ucw-config-file*
    *ucw-swank-port*
    *ucw-backend-type*
    *ucw-backend-host*
    *ucw-backend-port*
    *ucw-server-class*
    *ucw-applications-directory*
    *ucw-systems*
    *ucw-applications*
    *ucw-log-root-directory*
    *ucw-log-level*
    *ucw-compile-time-log-level*))

;;;; * Test

(defsystem* :ucw.core.test
  :components ((:module :test
                :components
                ((:file "package")
                 (:file "test-environment" :depends-on ("package"))
                 (:module "core"
                          :depends-on ("test-environment")
                          :serial t
                          :components ((:file "server")
                                       (:file "application")
                                       (:file "dispatcher")
                                       (:file "entry-point")
                                       (:file "component")
                                       (:file "action")
                                       (:file "callbacks")))
                 (:file "stress" :depends-on ("core")))))
  :depends-on (:ucw.core :stefil :drakma :arnesi :iterate))

(defsystem* :ucw.basic.test
  :components (#+nil(:module :test
                :components
                ((:file "ajax-action"))))
  :depends-on (:ucw.basic :ucw.core.test))

(defsystem* :ucw.test
  :components ((:module :test
                        :components
                        ((:file "package") ; reload the package to import more ucw:: symbols
                         (:file "ajax-action" :depends-on ("package")))))
  :depends-on (:ucw :cxml :flexi-streams :ucw.basic.test))

(defmethod perform ((op test-op) (system ucw-system))
  (format *debug-io* "~%*** Testing ~A using the ~A test system~%~%" system (test-system-of system))
  (operate 'load-op (test-system-of system))
  (in-package :ucw-test)
  (declaim (optimize (debug 3)))
  (warn "Issued a (declaim (optimize (debug 3))) for easy C-c C-c'ing")
  ;; KLUDGE ASDF wraps everything in a WITH-COMPILATION-UNIT and at the end it prevents starting the
  ;; tests on SBCL due to The Big Compiler Lock.
  (eval (read-from-string "(bordeaux-threads:make-thread
                             (lambda ()
                               (stefil:funcall-test-with-feedback-message 'ucw-test:test)))"))
  (values))

(defmethod operation-done-p ((op test-op) (system ucw-system))
  nil)


;;;; * Integration with other systems

(defsystem-connection ucw-and-contextl
  :requires (:ucw :contextl)
  :components ((:module :src
                        :components ((:file "contextl-integration")))))

(defsystem-connection ucw-and-cl-l10n
  :requires (:ucw :cl-l10n)
  :components ((:module :src
                        :components ((:module :l10n
                                              :components ((:file "l10n")
                                                           (:file "l10n-application" :depends-on ("l10n"))))))))

(defsystem-connection ucw-examples-and-cl-l10n
  :requires (:ucw.examples :cl-l10n)
  :components ((:module :examples
                        :components ((:module :src
                                              :components ((:file "l10n")))))))

;;;; * Introduction

;;;; This is the source code to UnCommon Web (aka UCW), "the UnCommon
;;;; Web Application Framework"

;;;;@include "src/packages.lisp"

;;;;@include "src/loggers.lisp"

;;;;@include "src/helpers.lisp"

;;;;@include "src/vars.lisp"


;; Copyright (c) 2003-2006 Edward Marco Baringer
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
;;  - Neither the name of Luca Capello, Edward Marco Baringer, nor
;;    BESE, nor the names of its contributors may be used to endorse
;;    or promote products derived from this software without specific
;;    prior written permission.
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
