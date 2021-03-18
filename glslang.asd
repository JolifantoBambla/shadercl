(defsystem glslang
  :version "0.0.0"
  :license "MIT"
  :description "Common Lisp bindings for glslang."
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :maintainer "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :depends-on (cffi cl-autowrap)
  :serial t
  :components ((:file "package")
               (:file "bindings")))
