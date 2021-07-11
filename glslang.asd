;;;; glslang.asd

(asdf:defsystem #:glslang
  :description "CFFI bindings for glslang"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:cffi)
  :components ((:module "bindings"
                :components ((:file "package")
                             (:file "bindings")))))
