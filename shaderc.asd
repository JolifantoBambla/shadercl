;;;; glslang.asd

(asdf:defsystem #:shaderc
  :description "CFFI bindings for shaderc"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :maintainer "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :homepage "https://jolifantobambla.github.io/shaderc/"
  :bug-tracker "https://github.com/JolifantoBambla/shaderc/issues"
  :source-control (:git "https://github.com/JolifantoBambla/shaderc.git")
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:alexandria #:cffi)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "early")
                             (:file "bindings")
                             (:file "shaderc"))))
  :in-order-to ((test-op (test-op shaderc/tests))))

(asdf:defsystem #:shaderc/tests
  :depends-on (#:shaderc
               #:rove)
  :components ((:module "test"
                :components ((:file "tests"))))
  :perform (test-op :after (op c) (uiop:symbol-call :rove :run c)))
