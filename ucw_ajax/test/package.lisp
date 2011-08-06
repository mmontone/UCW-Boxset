(in-package :cl-user)

(defpackage #:it.bese.ucw.test
  (:nicknames :ucw-test)
  (:use :common-lisp :ucw :stefil :iterate :arnesi :mopp :yaclml)
  (:shadow #:parent #:test #:uri #:deftest)
  (:export #:test))

(in-package :ucw-test)

(eval-always
  ;; import all the internal symbol of UCW
  (iter (for symbol :in-package #.(find-package :ucw) :external-only nil)
        (when (and (eq (symbol-package symbol) #.(find-package :ucw))
                   (not (find-symbol (symbol-name symbol) #.(find-package :ucw-test))))
          (import symbol))))

