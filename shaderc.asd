;;;; glslang.asd

(asdf:defsystem #:shaderc
  :description "CFFI bindings for shaderc"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:alexandria #:cffi)
  :components ((:file "package")
               (:file "early")
               (:file "bindings")
               (:file "shaderc")))
