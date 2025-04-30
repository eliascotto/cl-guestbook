(in-package :cl-user)
(defpackage guestbook.core
  (:use :cl)
  (:import-from :guestbook.config
                :*static-directory*))
(in-package :guestbook.core)

(syntax:use-syntax :annot)

(defvar *server* nil)

(defparameter *app*
  (lack:builder
    (:static
      :path (lambda (path)
              (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
                  path
                  nil))
      :root *static-directory*)
    guestbook.web:*web*))

@export
(defun start (&rest args
              &key
                (server :hunchentoot)
                (port 3210)
                (debug nil)
              &allow-other-keys)
  "Starts the server."
  (unless (null *server*)
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server."
        (stop))))

  (setf *server* (apply #'clack:clackup *app*
                   :server server
                   :port port
                   :debug debug
                   args))
  (format t "Server started"))

@export
(defun stop ()
  "Stops the server."
  (unless (null *server*)
    (clack:stop *server*)
    (format t "Server stopped")
    (setf *server* nil)))
