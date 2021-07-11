;;;; bindings.lisp

(in-package #:%shadercl)

(define-foreign-library shaderc
  (:unix (:or "libshaderc_shared.so" "libshaderc_shared.so.1")))

(use-foreign-library shaderc)

;;;; base types

(if (= 8 (foreign-type-size :pointer))
    (defctype size-t :uint64)
    (defctype size-t :uint32))

;;; env.h
(defcenum (target-env :unsigned-int)
  (:vulkan 0)
  :opengl
  :opengl-compat
  :webgpu ;; deprecated
  (:default 0))

(defcenum (env-version :unsigned-int)
  (:vulkan-1-0 4194304)
  (:vulkan-1-1 4198400)
  (:vulkan-1-2 4202496)
  (:opengl-4-5 450)
  (:webgpu)) ;; deprecated

(defcenum (spirv-version :unsigned-int)
  (:spv-1-0 65536)
  (:spv-1-1 65792)
  (:spv-1-2 66048)
  (:spv-1-3 66304)
  (:spv-1-4 66560)
  (:spv-1-5 66816))

;;; status.h
(defcenum (compilation-status :unsigned-int)
  (:success 0)
  (:invalid-stage 1)
  (:compilation-error 2)
  (:internal-error 3)
  (:null-result-object 4)
  (:invalid-assembly 5)
  (:validation-error 6)
  (:transformation-error 7)
  (:configuration-error 8))

;;; shaderc.h
(defcenum (source-language :unsinged-int)
  :glsl
  :hlsl)

(defcenum (shader-kind :unsigned-int)
  (:vertex-shader 0)
  :fragment-shader
  :compute-shader
  :geometry-shader
  :tess-control-shader
  :tess-evaluation-shader
  (:glsl-vertex-shader 0)
  (:glsl-fragment-shader 1)
  (:glsl-compute-shader 2)
  (:glsl-geometry-shader 3)
  (:glsl-tess-control-shader 4)
  (:glsl-tess-evaluation-shader 5)
  :glsl-infer-from-source
  :glsl-default-vertex-shader
  :glsl-default-fragment-shader
  :glsl-default-compute-shader
  :glsl-default-geometry-shader
  :glsl-default-tess-control-shader
  :glsl-default-tess-evaluation-shader
  :spirv-assembly
  :raygen-shader
  :anyhit-shader
  :closesthit-shader
  :miss-shader
  :intersection-shader
  :callable-shader
  (:glsl-raygen-shader 14)
  (:glsl-anyhit-shader 15)
  (:glsl-closesthit-shader 16)
  (:glsl-miss-shader 17)
  (:glsl-intersection-shader 18)
  (:glsl-callable-shader 19)
  :glsl-default-raygen-shader
  :glsl-default-anyhit-shader
  :glsl-default-closesthit-shader
  :glsl-default-miss-shader
  :glsl-default-intersection-shader
  :glsl-default-callable-shader
  :task-shader
  :mesh-shader
  (:glsl-task-shader 26)
  (:glsl-mesh-shader 27)
  :glsl-default-task-shader
  :glsl-default-mesh-shader)

(defcenum (profile :unsigned-int)
  :none
  :core
  :compatibility ;; disabled -> produces error
  :es)

(defcenum (optimization-level :unsigned-int)
  :zero
  :size
  :performance)

(defcenum (limit :unsigned-int)
  :max-lights
  :max-clip-planes
  :max-texture-units
  :max-texture-coords
  :max-vertex-attribs
  :max-vertex-uniform-components
  :max-varying-floats
  :max-vertex-texture-image-units
  :max-combined-texture-image-units
  :max-texture-image-units
  :max-fragment-uniform-components
  :max-draw-buffers
  :max-vertex-uniform-vectors
  :max-varying-vectors
  :max-fragment-uniform-vectors
  :max-vertex-output-vectors
  :max-fragment-input-vectors
  :min-program-texel-offset
  :max-program-texel-offset
  :max-clip-distances
  :max-compute-work-group-count-x
  :max-compute-work-group-count-y
  :max-compute-work-group-count-z
  :max-compute-work-group-size-x
  :max-compute-work-group-size-y
  :max-compute-work-group-size-z
  :max-compute-uniform-components
  :max-compute-texture-image-units
  :max-compute-image-uniforms
  :max-compute-atomic-counters
  :max-compute-atomic-counter-buffers
  :max-varying-components
  :max-vertex-output-components
  :max-geometry-input-components
  :max-geometry-output-components
  :max-fragment-input-components
  :max-image-units
  :max-combined-image-units-and-fragment-outputs
  :max-combined-shader-output-resources
  :max-image-samples
  :max-vertex-image-uniforms
  :max-tess-control-image-uniforms
  :max-tess-evaluation-image-uniforms
  :max-geometry-image-uniforms
  :max-fragment-image-uniforms
  :max-combined-image-uniforms
  :max-geometry-texture-image-units
  :max-geometry-output-vertices
  :max-geometry-total-output-components
  :max-geometry-uniform-components
  :max-geometry-varying-components
  :max-tess-control-input-components
  :max-tess-control-output-components
  :max-tess-control-texture-image-units
  :max-tess-control-uniform-components
  :max-tess-control-total-output-components
  :max-tess-evaluation-input-components
  :max-tess-evaluation-output-components
  :max-tess-evaluation-texture-image-units
  :max-tess-evaluation-uniform-components
  :max-tess-patch-components
  :max-patch-vertices
  :max-tess-gen-level
  :max-viewports
  :max-vertex-atomic-counters
  :max-tess-control-atomic-counters
  :max-tess-evaluation-atomic-counters
  :max-geometry-atomic-counters
  :max-fragment-atomic-counters
  :max-combined-atomic-counters
  :max-atomic-counter-bindings
  :max-vertex-atomic-counter-buffers
  :max-tess-control-atomic-counter-buffers
  :max-tess-evaluation-atomic-counter-buffers
  :max-geometry-atomic-counter-buffers
  :max-fragment-atomic-counter-buffers
  :max-combined-atomic-counter-buffers
  :max-atomic-counter-buffer-size
  :max-transform-feedback-buffers
  :max-transform-feedback-interleaved-components
  :max-cull-distances
  :max-combined-clip-and-cull-distances
  :max-samples)

(defcenum (uniform-kind :unsigned-int)
  :image
  :sampler
  :texture
  :buffer
  :storage-buffer
  :access-view)

(defctype shader-compiler :pointer)

(declaim (inline compiler-initialize))
(defcfun (":compiler-initialize"
          compiler-initialize)
    shader-compiler)

(declaim (inline compiler-release))
(defcfun (":compiler-release"
          compiler-release)
    :void
  shader-compiler shader-compiler)

(defctype compile-options :pointer)

;; todo: rest...

