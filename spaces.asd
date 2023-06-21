#|
  This file is a part of Spaces
  (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Yukari Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem #:spaces
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "0.0.0"
  :license "zlib"
  :description ""
  :homepage "https://shirakumo.github.io/spaces/"
  :bug-tracker "https://github.com/shirakumo/spaces/issues"
  :source-control (:git "https://github.com/shirakumo/spaces.git")
  :serial T
  :components ((:file "module")
               (:file "front"))
  :depends-on ((:interface :auth)
               :pathname-utils
               :r-clip
               :i-json))
