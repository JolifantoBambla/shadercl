(asdf:defsystem :claw-glslang
  :description "Thin wrapper over glslang"
  :version "1.0.0"
  :author "Lukas Herzberger"
  :license "MIT"
  :depends-on (:claw-glslang-bindings))


(asdf:defsystem :claw-glslang/wrapper
  :description "Thin wrapper over glslang"
  :version "1.0.0"
  :author "Lukas Herzberger"
  :license "MIT"
  :depends-on (:alexandria :uiop :cffi :claw-utils :claw)
  :pathname "src/"
  :serial t
  :components ((:file "claw")
               (:module :glslang-includes :pathname "lib/glslang")))
