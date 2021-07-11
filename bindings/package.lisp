;;;; package.lisp

(defpackage #:%glslang
  (:documentation "CFFI bindings for libglslang")
  (:use #:cl #:cffi)
  (:export
   #:size-t
   ;; callbacks
   #:glsl-include-system-func
   #:glsl-include-local-func
   #:glsl-free-include-result-func
   ;; enums & bitmasks
   #:client-t
   #:executable-t
   #:messages-t
   #:optimization-level-t
   #:profile-t
   #:reflection-options-t
   #:source-t
   #:stage-mask-t
   #:stage-t
   #:target-client-version-t
   #:target-language-t
   #:target-language-version-t
   #:texture-sampler-transform-mode-t
   ;; structs
   #:limits-t
   #:resource-t
   #:input-t
   #:glsl-include-result-t
   #:glsl-include-callback-t
   #:program-t
   #:shader-t
   ;; functions
   #:initialize-process
   #:finalize-process
   #:shader-create
   #:shader-delete
   #:shader-preprocess
   #:shader-parse
   #:shader-get-preprocessed-code
   #:shader-get-info-log
   #:shader-get-info-debug-log
   #:program-create
   #:program-delete
   #:program-add-shader
   #:program-link
   #:program-spirv-generate
   #:program-spirv-get-size
   #:program-spirv-get
   #:program-spirv-get-ptr
   #:program-spirv-get-messages
   #:program-get-info-log
   #:program-get-info-debug-log))

