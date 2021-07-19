;;;; glslang.asd

(asdf:defsystem #:shaderc
  :description "CFFI bindings for shaderc"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:cffi)
  :components ((:module "bindings"
                :components ((:file "package")
                             (:file "bindings")))))
