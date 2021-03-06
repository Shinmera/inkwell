#|
 This file is a part of Inkwell
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem inkwell
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An API client for the Splatoon 2 Splatnet."
  :homepage "https://github.com/Shinmera/inkwell"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "objects")
               (:file "api")
               (:file "documentation"))
  :depends-on (:drakma
               :yason
               :alexandria
               :local-time
               :documentation-utils))
