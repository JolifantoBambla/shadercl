;;;; bindings.lisp

(in-package #:%glslang)

(define-foreign-library glslang
  (:unix "libglslang.a")) ;; oh fuck, there is only a static lib included in the sdk... ok, shaderc it is then

(use-foreign-library glslang)

;;;; base types

(if (= 8 (foreign-type-size :pointer))
    (defctype size-t :uint64)
    (defctype size-t :uint32))


(defctype glsl-include-system-func :pointer)
(defctype glsl-include-local-func :pointer)
(defctype glsl-free-include-result-func :pointer)

;;;; enums & bitmasks
(defcenum (stage-t :unsigned-int)
  (:vertex 0)
  (:tesscontrol 1)
  (:tessevaluation 2)
  (:geometry 3)
  (:fragment 4)
  (:compute 5)
  (:raygen-nv 6)
  (:intersect-nv 7)
  (:anyhit-nv 8)
  (:closesthit-nv 9)
  (:miss-nv 10)
  (:callable-nv 11)
  (:task-nv 12)
  (:mesh-nv 13)
  (:count 14))
               
(defcenum (stage-mask-t :unsigned-int)
  (:vertex-mask 1)
  (:tesscontrol-mask 2)
  (:tessevaluation-mask 4)
  (:geometry-mask 8)
  (:fragment-mask 16)
  (:compute-mask 32)
  (:raygen-nv-mask 64)
  (:intersect-nv-mask 128)
  (:anyhit-nv-mask 256)
  (:closesthit-nv-mask 512)
  (:miss-nv-mask 1024)
  (:callable-nv-mask 2048)
  (:task-nv-mask 4096)
  (:mesh-nv-mask 8192)
  (:mask-count 8193))

(defcenum (source-t :unsigned-int)
  (:none 0)
  (:glsl 1)
  (:hlsl 2)
  (:count 3))

(defcenum (client-t :unsigned-int)
  (:none 0)
  (:vulkan 1)
  (:opengl 2)
  (:count 3))

(defcenum (target-language-t :unsigned-int)
  (:none 0)
  (:spv 1)
  (:count 2))

(defcenum (target-client-version-t :unsigned-int)
  (:vulkan-1-0 4194304)
  (:vulkan-1-1 4198400)
  (:vulkan-1-2 4202496)
  (:opengl-450 450)
  (:client-version-count 4))
               
(defcenum (target-language-version-t :unsigned-int)
  (:spv-1-0 65536)
  (:spv-1-1 65792)
  (:spv-1-2 66048)
  (:spv-1-3 66304)
  (:spv-1-4 66560)
  (:spv-1-5 66816)
  (:language-version-count 6))

(defcenum (executable-t :unsigned-int)
  (:vertex-fragment 0)
  (:fragment 1))


(defcenum (optimization-level-t :unsigned-int)
  (:no-generation 0)
  (:none 1)
  (:simple 2)
  (:full 3)
  (:level-count 4))

(defcenum (texture-sampler-transform-mode-t :unsigned-int)
  (:keep 0)
  (:upgrade-texture-remove-sampler 1)
  (:count 2))

(defcenum (messages-t :unsigned-int)
  (:default-bit 0)
  (:relaxed-errors-bit 1)
  (:suppress-warnings-bit 2)
  (:ast-bit 4)
  (:spv-rules-bit 8)
  (:vulkan-rules-bit 16)
  (:only-preprocessor-bit 32)
  (:read-hlsl-bit 64)
  (:cascading-errors-bit 128)
  (:keep-uncalled-bit 256)
  (:hlsl-offsets-bit 512)
  (:debug-info-bit 1024)
  (:hlsl-enable-16bit-types-bit 2048)
  (:hlsl-legalization-bit 4096)
  (:hlsl-dx9-compatible-bit 8192)
  (:builtin-symbol-table-bit 16384)
  (:count 16385))

(defcenum (profile-t :unsigned-int)
  (:bad-profile 0)
  (:no-profile 1)
  (:core-profile 2)
  (:compatibility-profile 4)
  (:es-profile 8)
  (:profile-count 9))

(defcenum (reflection-options-t :unsigned-int)
  (:default-bit 0)
  (:strict-array-suffix-bit 1)
  (:basic-array-suffix-bit 2)
  (:intermediate-ioo-bit 4)
  (:separate-buffers-bit 8)
  (:all-block-variables-bit 16)
  (:unwrap-io-blocks-bit 32)
  (:all-io-variables-bit 64)
  (:shared-std140-ssbo-bit 128)
  (:shared-std140-ubo-bit 256)
  (:count 257))

;;;; structs

(defcstruct (limits-t)
  (non-inductive-for-loops :boolean)
  (while-loops :boolean)
  (do-while-loops :boolean)
  (general-uniform-indexing :boolean)
  (general-attribute-matrix-vector-indexing :boolean)
  (general-varying-indexing :boolean)
  (general-sampler-indexing :boolean)
  (general-variable-indexing :boolean)
  (general-constant-matrix-vector-indexing :boolean))

(defcstruct (resource-t)
  (max-lights :int)
  (max-clip-planes :int)
  (max-texture-units :int)
  (max-texture-coords :int)
  (max-vertex-attribs :int)
  (max-vertex-uniform-components :int)
  (max-varying-floats :int)
  (max-vertex-texture-image-units :int)
  (max-combined-texture-image-units :int)
  (max-texture-image-units :int)
  (max-fragment-uniform-components :int)
  (max-draw-buffers :int)
  (max-vertex-uniform-vectors :int)
  (max-varying-vectors :int)
  (max-fragment-uniform-vectors :int)
  (max-vertex-output-vectors :int)
  (max-fragment-input-vectors :int)
  (min-program-texel-offset :int)
  (max-program-texel-offset :int)
  (max-clip-distances :int)
  (max-compute-work-group-count-x :int)
  (max-compute-work-group-count-y :int)
  (max-compute-work-group-count-z :int)
  (max-compute-work-group-size-x :int)
  (max-compute-work-group-size-y :int)
  (max-compute-work-group-size-z :int)
  (max-compute-uniform-components :int)
  (max-compute-texture-image-units :int)
  (max-compute-image-uniforms :int)
  (max-compute-atomic-counters :int)
  (max-compute-atomic-counter-buffers :int)
  (max-varying-components :int)
  (max-vertex-output-components :int)
  (max-geometry-input-components :int)
  (max-geometry-output-components :int)
  (max-fragment-input-components :int)
  (max-image-units :int)
  (max-combined-image-units-and-fragment-outputs :int)
  (max-combined-shader-output-resources :int)
  (max-image-samples :int)
  (max-vertex-image-uniforms :int)
  (max-tess-control-image-uniforms :int)
  (max-tess-evaluation-image-uniforms :int)
  (max-geometry-image-uniforms :int)
  (max-fragment-image-uniforms :int)
  (max-combined-image-uniforms :int)
  (max-geometry-texture-image-units :int)
  (max-geometry-output-vertices :int)
  (max-geometry-total-output-components :int)
  (max-geometry-uniform-components :int)
  (max-geometry-varying-components :int)
  (max-tess-control-input-components :int)
  (max-tess-control-output-components :int)
  (max-tess-control-texture-image-units :int)
  (max-tess-control-uniform-components :int)
  (max-tess-control-total-output-components :int)
  (max-tess-evaluation-input-components :int)
  (max-tess-evaluation-output-components :int)
  (max-tess-evaluation-texture-image-units :int)
  (max-tess-evaluation-uniform-components :int)
  (max-tess-patch-components :int)
  (max-patch-vertices :int)
  (max-tess-gen-level :int)
  (max-viewports :int)
  (max-vertex-atomic-counters :int)
  (max-tess-control-atomic-counters :int)
  (max-tess-evaluation-atomic-counters :int)
  (max-geometry-atomic-counters :int)
  (max-fragment-atomic-counters :int)
  (max-combined-atomic-counters :int)
  (max-atomic-counter-bindings :int)
  (max-vertex-atomic-counter-buffers :int)
  (max-tess-control-atomic-counter-buffers :int)
  (max-tess-evaluation-atomic-counter-buffers :int)
  (max-geometry-atomic-counter-buffers :int)
  (max-fragment-atomic-counter-buffers :int)
  (max-combined-atomic-counter-buffers :int)
  (max-atomic-counter-buffer-size :int)
  (max-transform-feedback-buffers :int)
  (max-transform-feedback-interleaved-components :int)
  (max-cull-distances :int)
  (max-combined-clip-and-cull-distances :int)
  (max-samples :int)
  (max-mesh-output-vertices-nv :int)
  (max-mesh-output-primitives-nv :int)
  (max-mesh-work-group-size-x-nv :int)
  (max-mesh-work-group-size-y-nv :int)
  (max-mesh-work-group-size-z-nv :int)
  (max-task-work-group-size-x-nv :int)
  (max-task-work-group-size-y-nv :int)
  (max-task-work-group-size-z-nv :int)
  (max-mesh-view-count-nv :int)
  (maxDualSourceDrawBuffersEXT :int)
  (limits (:struct limits-t)))

(defcstruct (input-t)
  (language source-t)
  (stage stage-t)
  (client client-t)
  (client-version target-client-version-t)
  (target-language target-language-t)
  (target-language-version target-language-version-t)
  (code :string) ;; shader source code
  (default-version :int)
  (default-profile profile-t)
  (force-default-version-and-profile :int)
  (forward-compatible :int)
  (messages messages-t)
  (resource (:pointer (:struct resource-t))))

(defcstruct (glsl-include-result-t)
  (header-name :string)
  (header-data :string)
  (header-length size-t))

(defcstruct (glsl-include-callback-t)
  (include-system glsl-include-system-func)
  (include-local glsl-include-local-func)
  (free-include-result glsl-free-include-result-func))

(defcstruct (program-t))

(defcstruct (shader-t))

;;;; functions

;;; process
(declaim (inline initialize-process))
(defcfun ("glslang_initialize_process"
          initialize-process)
    :int)
              
(declaim (inline finalize-process))
(defcfun ("glslang_finalize_process"
          finalize-process)
    :void)

;;; shader
(declaim (inline shader-create))
(defcfun ("glslang_shader_create"
          shader-create)
    (:pointer (:struct shader-t))
  (input (:pointer (:struct input-t))))
              
(declaim (inline shader-delete))
(defcfun ("glslang_shader_delete"
          shader-delete)
    :void
  (shader (:pointer (:struct shader-t))))

(declaim (inline shader-preprocess))
(defcfun ("glslang_shader_preprocess"
          shader-preprocess)
    :int
  (shader (:pointer (:struct shader-t)))
  (input (:pointer (:struct input-t))))

(declaim (inline shader-parse))
(defcfun ("glslang_shader_parse"
          shader-parse)
    :int
  (shader (:pointer (:struct shader-t)))
  (input (:pointer (:struct input-t))))

(declaim (inline shader-get-preprocessed-code))
(defcfun ("glslang_shader_get_preprocessed_code"
          shader-get-preprocessed-code)
    :string
  (shader (:pointer (:struct shader-t))))

(declaim (inline shader-get-info-log))
(defcfun ("glslang_shader_get_info_log"
          shader-get-info-log)
    :string
  (shader (:pointer (:struct shader-t))))

(declaim (inline shader-get-info-debug-log))
(defcfun ("glslang_shader_get_info_debug_log"
          shader-get-info-debug-log)
    :string
  (shader (:pointer (:struct shader-t))))

;;; program
(declaim (inline program-create))
(defcfun ("glslang_program_create"
          program-create)
    (:pointer (:struct program-t)))

(declaim (inline program-delete))
(defcfun ("glslang_program_delete"
          program-delete)
    :void
  (program (:pointer (:struct program-t))))

(declaim (inline program-add-shader))
(defcfun ("glslang_program_add_shader"
          program-add-shader)
    :void
  (program (:pointer (:struct program-t)))
  (shader (:pointer (:struct shader-t))))

(declaim (inline program-link))
(defcfun ("glslang_program_link"
          program-link)
    :int
  (program (:pointer (:struct program-t)))
  (messages messages-t)) ;; an int in the header file

(declaim (inline program-spirv-generate))
(defcfun ("glslang_program_SPIRV_generate"
          program-spirv-generate)
    :void
  (program (:pointer (:struct program-t)))
  (stage stage-t))

(declaim (inline program-spirv-get-size))
(defcfun ("glslang_program_SPIRV_get_size"
          program-spirv-get-size)
    size-t
  (program (:pointer (:struct program-t))))

(declaim (inline program-spirv-get))
(defcfun ("glslang_program_SPIRV_get"
          program-spirv-get)
    :void
  (program (:pointer (:struct program-t)))
  (return-arg (:pointer :unsigned-int))) ;; I guess this is a return argument (byte array of size program-spirv-get-size)

(declaim (inline program-spirv-get-ptr))
(defcfun ("glslang_program_SPIRV_get_ptr"
          program-spirv-get-ptr)
    (:pointer :unsigned-int)
  (program (:pointer (:struct program-t))))

(declaim (inline program-spirv-get-messages))
(defcfun ("glslang_program_SPIRV_get_messages"
          program-spirv-get-messages)
    :string
  (program (:pointer (:struct program-t))))

(declaim (inline program-get-info-log))
(defcfun ("glslang_program_get_info_log"
          program-get-info-log)
    :string
  (program (:pointer (:struct program-t))))

(declaim (inline program-get-info-debug-log))
(defcfun ("glslang_program_get_info_debug_log"
          program-get-info-debug-log)
    :string
  (program (:pointer (:struct program-t))))

