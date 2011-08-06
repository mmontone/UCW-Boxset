;;; -*- lisp -*-

;;;; * Startup file for ucw_dev

(in-package :common-lisp-user)

(defparameter *ucw-directory* (merge-pathnames #P"ucw_dev/"
					       (make-pathname :directory (pathname-directory *load-pathname*))))

(load (merge-pathnames #P"start-common.lisp" *load-pathname*))
