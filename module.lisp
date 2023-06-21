#|
 This file is a part of Spaces
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Yukari Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:spaces
  (:use #:cl #:radiance))
(in-package #:spaces)

(define-trigger startup ()
  (user:add-default-permissions (perm spaces post))
  (setf (config :allowed-mime-types) '("text/plain" "text/html" "application/xhtml+xml"
                                       "text/css" "text/javascript"
                                       "image/png" "image/jpeg"  "image/gif" "image/webp"
                                       "audio/wav" "video/webm"
                                       "font/woff" "font/woff2" "font/ttf" "font/otf")))
