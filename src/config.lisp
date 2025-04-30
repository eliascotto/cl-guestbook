(in-package :cl-user)
(defpackage guestbook.config
  (:use :cl))
(in-package :guestbook.config)

(syntax:use-syntax :annot)

@export
(defparameter *application-root* (asdf:system-source-directory :guestbook))
@export
(defparameter *static-directory* (merge-pathnames #P"static/" *application-root*))
@export
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

@export
(defvar *config*
  '(:databases ((:maindb :sqlite3 :database-name "/Users/elia/dev/Lisp/guestbook/guestbook.sqlite"))
    :schema-file "db/schema.sql"))
