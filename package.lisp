;;;; package.lisp

(defpackage #:%shaderc
  (:documentation "CFFI bindings for shaderc.")
  (:use #:cl #:cffi)
  (:export
   #:size-t
   ;; callbacks
   #:include-resolve-fn
   #:include-result-release-fn
   ;; enums
   #:target-env
   #:env-version
   #:spirv-version
   #:compilation-status
   #:source-language
   #:shader-kind
   #:profile
   #:optimization-level
   #:limit
   #:uniform-kind
   #:include-type
   ;; structs
   #:include-result
   ;; opaque handles
   #:compiler
   #:compile-options
   #:compilation-result
   ;; functions
   #:compiler-initialize
   #:compiler-release
   #:compile-options-initialize
   #:compile-options-clone
   #:compile-options-release
   #:compile-options-add-macro-definition
   #:compile-options-set-source-language
   #:compile-options-set-generate-debug-info
   #:compile-options-set-optimization-level
   #:compile-options-set-forced-version-profile
   #:compile-options-set-include-callbacks
   #:compile-options-set-suppress-warnings
   #:compile-options-set-target-env
   #:compile-options-set-target-spirv
   #:compile-options-set-warnings-as-errors
   #:compile-options-set-limit
   #:compile-options-set-auto-bind-uniforms
   #:compile-options-set-auto-combined-image-sampler
   #:compile-options-set-hlsl-io-mapping
   #:compile-options-set-hlsl-offsets
   #:compile-options-set-binding-base
   #:compile-options-set-binding-base-for-stage
   #:compile-options-set-auto-map-locations
   #:compile-options-set-hlsl-register-set-and-bindings-for-stage
   #:compile-options-set-hlsl-register-set-and-binding
   #:compile-options-set-hlsl-functionality-1
   #:compile-options-set-invert-y
   #:compile-options-set-nan-clamp
   #:compile-into-spv
   #:compile-into-spv-assembly
   #:compile-into-preprocessed-text
   #:assemble-into-spv
   #:result-release
   #:result-get-length
   #:result-get-num-warnings
   #:result-get-num-errors
   #:result-get-compilation-status
   #:result-get-bytes
   #:result-get-error-message
   #:get-spv-version
   #:parse-version-profile))

(defpackage #:shaderc
  (:nicknames #:sc)
  (:use #:cl)
  (:export
   #:with-compiler
   #:with-compile-options
   #:set-compile-options-from-set
   ;; compile options
   #:compile-options-set
   #:macros
   #:lang
   #:generate-debug-info
   #:optimization-level
   #:forced-version-profile
   #:suppress-warnings
   #:target-env
   #:target-spirv
   #:warnings-as-errors
   #:limits
   #:auto-bind-uniforms
   #:auto-combined-image-sampler
   #:hlsl-io-mapping
   #:hlsl-offsets
   #:binding-bases
   #:auto-map-locations
   #:hlsl-register-set-and-bindings
   #:enable-hlsl-functionality-1
   #:invert-y
   #:clamp-nan))
