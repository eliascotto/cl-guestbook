(in-package :cl-user)
(defpackage guestbook.web
  (:use :cl
        :caveman2)
  (:import-from :guestbook.config
                :*template-directory*)
  (:import-from :guestbook.db
                :add-message
                :delete-message
                :get-all-messages)
  (:export :*web*))
(in-package :guestbook.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Templates

(djula:add-template-directory *template-directory*)

(defun render (template-path &optional env)
  "Renders a Djula template from TEMPLATE-PATH.
ENV is an optional plist of variables passed to the template."
  (apply #'djula:render-template*
         (djula:compile-template* (princ-to-string template-path))
         nil
         env))

;;
;; Routes

(defroute "/" ()
  (render #P"index.html"
          (list :messages (get-all-messages))))


(defroute ("/message" :method :POST) ()
  (let* ((body-params (request-body-parameters *request*))
         (name-param (assoc "name" body-params :test #'string=))
         (message-param (assoc "message" body-params :test #'string=)))
    (if (and (consp name-param) (consp message-param))
      (add-message (cdr name-param)
                   (cdr message-param))
      (format t "Missing body parameters: received ~A~%" body-params)))
  (redirect "/"))


(defroute ("/message/delete" :method :POST) ()
  (let* ((body-params (request-body-parameters *request*))
         (id-param (assoc "id" body-params :test #'string=)))
    (if (consp id-param)
        (let ((id (ignore-errors (parse-integer (cdr id-param)))))
          (when id
            (delete-message id)))
        (format t "Missing id parameter.")))
  (redirect "/"))


(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app code))
  (render #P"404.html"))
