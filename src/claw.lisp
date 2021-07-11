(uiop:define-package :glslang
  (:use :cl))

(claw:defwrapper (:claw-glslang/wrapper
                  (:headers "glslang/Include/glslang_c_interface.h"
                            "glslang/Include/glslang_c_shader_types.h")
                  (:includes :glslang-includes)
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :x86-64 :windows) "x86_64-w64-mingw32")
                            ((:and :x86-64 :drawin) "x86_64-apple-darwin-gnu"))
                  (:persistent :claw-glslang-bindings
                   :asd-path "../claw-glslang-bindings.asd"
                   :bindings-path "../bindings/")
                  (:include-definitions "^(glslang|GLSLANG)_\\w+"))
  :in-package :%glslang
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :symbolicate-names (:in-pipeline (:by-removing-prefixes "glslang_" "GLSLANG_")))
