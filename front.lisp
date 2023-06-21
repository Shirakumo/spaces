#|
 This file is a part of Spaces
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Yukari Hafner <shinmera@tymoon.eu>
|#

(in-package #:spaces)

(defun user-directory (user)
  (handler-case
      (pathname-utils:subdirectory (environment-module-directory #.*package* :data)
                                   (princ-to-string (user:id user)))
    (error () (error 'request-not-found :message "This space does not exist."))))

(defun user-pathname (user path)
  (merge-pathnames (etypecase path
                     (string (parse-path-safely (string-left-trim "/" path)))
                     (pathname path))
                   (user-directory user)))

(defun user-files (user)
  (let ((files ())
        (base (user-directory user)))
    (dolist (path (directory (merge-pathnames #p"**/*.*" base)))
      (push (enough-namestring path base) files))
    (sort files #+sbcl #'sb-unicode:unicode< #-sbcl #'string<)))

(defun check-mime-type (type)
  (find type (config :allowed-mime-types) :test #'string=))

(defun api-return (message &key (type :ok))
  (if (string= "true" (post/get "browser"))
      (redirect (merge-url (referer) :parameters (ecase type
                                                   (:ok `(("message" . ,message)))
                                                   (:error `(("error" . ,message))))))
      (api-output NIL :message message :status (ecase type
                                                 (:ok 200)
                                                 (:error 500)))))

(defun upload-html (content path user)
  ;; Delete any injection section the user might have snuck in or left in.
  (let ((finds ()))
    (labels ((scanren (node)
               (loop for child across (plump:children node)
                     do (when (plump:element-p child)
                          (if (string-equal "spaces-injection" (plump:attribute child "id"))
                              (push child finds)
                              (scanren child))))))
      (scanren content)
      (mapc #'plump:remove-child finds)))
  ;; Inject our button again to ensure its structure and data is up to date.
  (let* ((target (or (first (plump:get-elements-by-tag-name content "body")) content))
         (injection (aref (plump:children (plump:parse (@template "static.html"))) 0))
         (meta (plump:make-element injection "meta")))
    (setf (plump:attribute meta "name") "path")
    (setf (plump:attribute meta "content") (enough-namestring path (user-directory user)))
    (plump:append-child target injection))
  ;; Now save the file.
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (plump:serialize content stream)))

(define-page view-index "spaces/^$" ()
  (setf (header "Cache-Control") "public")
  (r-clip:with-clip-processing ("index.html" "text/html")
    (r-clip:process T)))

(define-page view-user-content "spaces/(.+?)(:?/(.*))?$" (:uri-groups (user path))
  (when (string= user "static")
    (invoke-restart 'abort-handling))
  (setf (header "Cache-Control") "public")
  (let ((file (user-pathname user (or path ""))))
    (unless (pathname-name file)
      (setf file (make-pathname :name "index" :defaults file)))
    (unless (pathname-type file)
      (setf file (make-pathname :type "html" :defaults file)))
    (handler-case
        (serve-file file)
      (file-to-serve-does-not-exist ()
        (if (or (string-equal user (user:username (auth:current "anonymous")))
                (user:check (auth:current "anonymous") (perm spaces admin)))
            (serve-file (@template "placeholder.html"))
            (error 'request-not-found :message "This page does not exist."))))))

(define-api spaces/save (path content &optional user) (:access (perm spaces post))
  (let* ((user (cond (user
                      (user:check (auth:current) (perm spaces admin))
                      (user:get user))
                     (T
                      (auth:current))))
         (path (user-pathname user path)))
    (let ((plump:*tag-dispatchers* plump:*html-tags*))
      (upload-html (plump:parse content) path user))
    (api-return "File uploaded")))

(define-api spaces/upload (files[] &optional path user) (:access (perm spaces post))
  (let* ((user (cond (user
                      (user:check (auth:current) (perm spaces admin))
                      (user:get user))
                     (T
                      (auth:current))))
         (path (user-pathname user (or path ""))))
    (dolist (file files[])
      (let ((path (merge-pathnames path (second file))))
        (check-mime-type (third file))
        (cond ((string= (third file) "text/html")
               (let ((plump:*tag-dispatchers* plump:*html-tags*))
                 (upload-html (plump:parse (first file)) path user)))
              ((string= (third file) "application/xhtml+xml")
               (let ((plump:*tag-dispatchers* plump:*xml-tags*))
                 (upload-html (plump:parse (first file)) path user)))
              (T
               (uiop:copy-file (first file) path)))))
    (api-return "File uploaded")))

(define-api spaces/delete (files[] &optional user) (:access (perm spaces post))
  (let* ((user (cond (user
                      (user:check (auth:current) (perm spaces admin))
                      (user:get user))
                     (T
                      (auth:current))))
         (path (user-pathname user "")))
    (dolist (file files[])
      (let ((path (merge-pathnames file path)))
        (cond ((ignore-errors (probe-file path))
               (delete-file path)
               (api-return "File deleted"))
              (T
               (api-return "File does not exist" :type :error)))))))

(define-api spaces/edit () (:access (perm spaces post))
  (r-clip:with-clip-processing ("edit.html" "text/html")
    (r-clip:process T)))

(define-api spaces/authorized (user) ()
  (let ((current (auth:current "anonymous")))
    (api-output (if (or (and (string-equal user (user:username current))
                             (user:check current (perm spaces post)))
                        (user:check current (perm spaces admin)))
                    :true :false))))
