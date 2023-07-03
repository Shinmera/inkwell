(asdf:defsystem inkwell
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
