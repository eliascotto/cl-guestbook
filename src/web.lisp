(in-package :cl-user)
(defpackage guestbook.web
  (:use :cl
        :caveman2
        :guestbook.db)
  (:import-from :guestbook.config
                :*template-directory*)
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

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  "Renders a Djula template from TEMPLATE-PATH, caching it for reuse.
ENV is an optional plist of variables passed to the template."
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      ;; If the template is not already compiled, it is compiled, and stored in *TEMPLATE-REGISTRY*.
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

;;
;; Utils

(defun decode-ts (ts)
  "Converts a universal time value TS into a human-readable timestamp string,
formatted as 'YYYY-MM-DD HH:MM:SS'."
  (multiple-value-bind (sec min hour day month year)
    (decode-universal-time ts)
   (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour min sec)))

;;
;; Database access

(defun add-message (name message)
  (with-connection (db)
    (let ((sql "INSERT INTO message (username, ts, content) VALUES (?, ?, ?)")
          (ts (get-universal-time)))
     (dbi:do-sql *connection* sql (list name ts message)))))


(defun delete-message (id)
  (with-connection (db)
    (let ((sql "DELETE FROM message WHERE id = ?"))
      (dbi:do-sql *connection* sql (list id)))))


(defun get-all-messages ()
  (with-connection (db)
    (let* ((sql "SELECT * FROM message ORDER BY ts DESC")
           (messages (dbi:fetch-all
                      (dbi:execute
                       (dbi:prepare *connection* sql)))))
      ;; Transform unix timestamp into readable format
      (mapcar (lambda (row)
                (setf (getf row :|ts|) (decode-ts (getf row :|ts|)))
                row)
              messages))))

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
