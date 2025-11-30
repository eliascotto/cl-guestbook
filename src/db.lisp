(in-package :cl-user)
(defpackage guestbook.db
  (:use :cl)
  (:import-from :guestbook.config
                :*config*
                :*application-root*)
  (:export :db
           :*connection*
           :with-connection
           :init-db
           :format-timestamp
           :add-message
           :delete-message
           :get-all-messages))
(in-package :guestbook.db)

(syntax:use-syntax :annot)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (getf *config* :databases))))

@export
(defun db (&optional (db :maindb))
  (apply #'dbi:connect-cached (connection-settings db)))

@export
(defvar *connection* nil)

@export
(defmacro with-connection (conn &body body)
  "Executes BODY with *CONNECTION* dynamically bound to CONN.
CONN should be a database connection object, typically from `db`.

Usage: (with-connection (db :maindb)
         (dbi:do-sql *connection* \"SELECT * FROM users\"))"
  `(let ((*connection* ,conn))
     (unless *connection*
       (error "Database connection cannot be NIL"))
     ,@body))


(defun create-db (db-path schema-path)
  "Creates a sqlite3 database in DB-PATH initialised with sql from file in SCHEMA-PATH."
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))

    (uiop:run-program (list "sqlite3"
                            (namestring db-path)
                            "-init"
                            (namestring schema-path))
                      :ignore-error-status t
                      :output output
                      :error-output error-output)

    ;; Print captured outputs
    (let ((output-str (get-output-stream-string output))
          (error-str (get-output-stream-string error-output)))
      (unless (string= output-str "")
        (format t "SQLite Output: ~A~%" output-str))
      (unless (string= error-str "")
        (format t "SQLite Error: ~A~%" error-str)))))


@export
(defun init-db (&optional (db-key :maindb))
  "Creates a new sqlite3 database and runs the schema specified in the config."
  (let* ((db-settings (connection-settings db-key))
         (schema-file (getf *config* :schema-file)))

    (unless (equalp (first db-settings) :sqlite3)
      (error "INIT-DB supports only SQLITE3 databases. Got: ~A for DB key: ~A"
             (first db-settings) db-key))

    (unless schema-file
      (error "Schema file path not found in config. Expected key :schema-file in config."))

    (let ((db-path (merge-pathnames (getf (cdr db-settings) :database-name) *application-root*))
          (schema-path (merge-pathnames schema-file *application-root*)))

      (unless (uiop:file-exists-p schema-path)
        (error "Schema file not found: ~A (Resolved from: ~A + ~A)"
               schema-path guestbook.config:*application-root* schema-file))

      (uiop:delete-file-if-exists db-path)
      (create-db db-path schema-path))))


;;
;; Utilities

(defun format-timestamp (universal-time)
  "Converts a universal time value into a human-readable timestamp string,
formatted as 'YYYY-MM-DD HH:MM:SS'."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour min sec)))


;;
;; Message operations

(defun add-message (name message)
  "Inserts a new message into the database with the given NAME and MESSAGE content."
  (with-connection (db)
    (let ((sql "INSERT INTO message (username, ts, content) VALUES (?, ?, ?)")
          (ts (get-universal-time)))
      (dbi:do-sql *connection* sql (list name ts message)))))


(defun delete-message (id)
  "Deletes the message with the given ID from the database."
  (with-connection (db)
    (let ((sql "DELETE FROM message WHERE id = ?"))
      (dbi:do-sql *connection* sql (list id)))))


(defun get-all-messages ()
  "Retrieves all messages from the database, ordered by timestamp descending.
Timestamps are formatted as human-readable strings."
  (with-connection (db)
    (let* ((sql "SELECT * FROM message ORDER BY ts DESC")
           (messages (dbi:fetch-all
                      (dbi:execute
                       (dbi:prepare *connection* sql)))))
      (mapcar (lambda (row)
                (setf (getf row :|ts|) (format-timestamp (getf row :|ts|)))
                row)
              messages))))
