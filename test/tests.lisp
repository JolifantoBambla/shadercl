;;;; tests.lisp

(defpackage #:shaderc/tests
  (:use #:cl
        #:rove
        #:shaderc))
(in-package #:shaderc/tests)

(defparameter +vertex-shader-format-string+ "
#version 400

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

~a

layout (std140, binding = 0) uniform buffer {
       mat4 mvpc;
} uniformBuffer;

layout (location = 0) in vec4 pos;
layout (location = 1) in vec4 inColor;

layout (location = 0) out vec4 outColor;

void main() {
   outColor = inColor;
   gl_Position = uniformBuffer.mvpc * pos;
}
")

(defparameter +valid-vertex-shader+
  (format nil +vertex-shader-format-string+ ""))

(defparameter +valid-vertex-shader-standard-include+
  (format nil +vertex-shader-format-string+ "#include <lib.glsl>"))

(defparameter +valid-vertex-shader-relative-include+
  (format nil +vertex-shader-format-string+ "#include \"lib.glsl\""))

(defparameter +test-dir+
  (namestring
   (asdf:system-relative-pathname 'shaderc/tests
                                  #P"test/")))

(deftest compile-to-spv
  (testing "fails on invalid shader code"
    (ok (signals (compile-to-spv "bogus shader" :vertex-shader)
            'shaderc-error)))
  (testing "fails on invalid shader stage"
    (ok (signals (compile-to-spv +valid-vertex-shader+ :fragment-shader)
            'shaderc-error)))
  (testing "compiles valid shader"
    (ok (arrayp (compile-to-spv +valid-vertex-shader+ :vertex-shader))))
  (testing "fails on failed relative inclusion"
    (ok (signals (compile-to-spv +valid-vertex-shader-relative-include+ :vertex-shader
                                 :options (make-instance 'compile-options-set))
            'shaderc-error)))
  (testing "fails on failed standard inclusion"
    (setf *default-include-dirs* nil)
    (ok (signals (compile-to-spv +valid-vertex-shader-standard-include+ :vertex-shader
                                 :options (make-instance 'compile-options-set))
            'shaderc-error)))
  (testing "compiles valid shader with resolved relative inclusion"
    (ok (arrayp (compile-to-spv +valid-vertex-shader-relative-include+ :vertex-shader
                                :tag (namestring (merge-pathnames "shader.vert" +test-dir+))
                                :options (make-instance 'compile-options-set)))))
  (testing "compiles valid shader with resolved standard inclusion"
    (pushnew +test-dir+ *default-include-dirs*)
    (ok (arrayp (compile-to-spv +valid-vertex-shader-standard-include+ :vertex-shader
                                :options (make-instance 'compile-options-set)))))
  (testing "compiles valid shader with resolved absolute inclusion"
    (ok (arrayp (compile-to-spv (format nil +vertex-shader-format-string+
                                        (format nil "#include \"~a\""
                                                (namestring (merge-pathnames "lib.glsl" +test-dir+))))
                                :vertex-shader
                                :options (make-instance 'compile-options-set))))))

