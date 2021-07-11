(uiop:define-package :%glslang (:use))
(uiop:define-package :claw-glslang-bindings~pristine (:use :cl))
(common-lisp:in-package :claw-glslang-bindings~pristine)

(defparameter %glslang::+c-iface-h-included+ nil)

(defparameter %glslang::+export+ nil)

(cffi:defcenum (%glslang::client-t :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:85:9"
               (:none 0)
               (:vulkan 1)
               (:opengl 2)
               (:count 3))

(cffi:defcenum (%glslang::executable-t :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:120:9"
               (:vertex-fragment 0)
               (:fragment 1))

(cffi:defcenum (%glslang::messages-t :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:139:9"
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

(cffi:defcenum (%glslang::optimization-toplevel-t
                :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:123:9"
               (:no-generation 0)
               (:none 1)
               (:simple 2)
               (:full 3)
               (:level-count 4))

(cffi:defcenum (%glslang::profile-t :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:175:9"
               (:bad-profile 0)
               (:no-profile 1)
               (:core-profile 2)
               (:compatibility-profile 4)
               (:es-profile 8)
               (:profile-count 9))

(cffi:defcenum (%glslang::reflection-options-t
                :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:160:9"
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

(cffi:defcenum (%glslang::source-t :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:77:9"
               (:none 0)
               (:glsl 1)
               (:hlsl 2)
               (:count 3))

(cffi:defcenum (%glslang::stage-mask-t :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:58:9"
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

(cffi:defcenum (%glslang::stage-t :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:39:9"
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

(cffi:defcenum (%glslang::client-version-t
                :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:100:9"
               (:vulkan-1-0 4194304)
               (:vulkan-1-1 4198400)
               (:vulkan-1-2 4202496)
               (:opengl-450 450)
               (:client-version-count 4))

(cffi:defcenum (%glslang::target-language-t
                :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:93:9"
               (:none 0)
               (:spv 1)
               (:count 2))

(cffi:defcenum (%glslang::target-language-version-t
                :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:109:9"
               (:spv-1-0 65536)
               (:spv-1-1 65792)
               (:spv-1-2 66048)
               (:spv-1-3 66304)
               (:spv-1-4 66560)
               (:spv-1-5 66816)
               (:language-version-count 6))

(cffi:defcenum (%glslang::texture-sampler-transform-mode-t
                :unsigned-int)
               "src/lib/glslang/glslang/Include/glslang_c_shader_types.h:132:9"
               (:keep 0)
               (:upgrade-texture-remove-sampler 1)
               (:count 2))

(declaim (inline %glslang::finalize-process))

(cffi:defcfun ("glslang_finalize_process" %glslang::finalize-process)
              :void
              &rest)

(declaim (inline %glslang::initialize-process))

(cffi:defcfun ("glslang_initialize_process"
               %glslang::initialize-process)
              :int
              &rest)

(cffi:defcstruct (%glslang::program-s :size 0))

(cffi:defctype %glslang::program-t (:struct %glslang::program-s))

(cffi:defctype %glslang::stage-t %glslang::stage-t)

(declaim (inline %glslang::program-spirv-generate))

(cffi:defcfun ("glslang_program_SPIRV_generate"
               %glslang::program-spirv-generate)
              :void
              (%glslang::program (:pointer %glslang::program-t))
              (%glslang::stage %glslang::stage-t))

(declaim (inline %glslang::program-spirv-get))

(cffi:defcfun ("glslang_program_SPIRV_get"
               %glslang::program-spirv-get)
              :void
              (%glslang::program (:pointer %glslang::program-t))
              (%glslang::arg1 (:pointer :unsigned-int)))

(declaim (inline %glslang::program-spirv-get-messages))

(cffi:defcfun ("glslang_program_SPIRV_get_messages"
               %glslang::program-spirv-get-messages)
              :string
              (%glslang::program (:pointer %glslang::program-t)))

(declaim (inline %glslang::program-spirv-get-ptr))

(cffi:defcfun ("glslang_program_SPIRV_get_ptr"
               %glslang::program-spirv-get-ptr)
              (:pointer :unsigned-int)
              (%glslang::program (:pointer %glslang::program-t)))

(declaim (inline %glslang::program-spirv-get-size))

(cffi:defcfun ("glslang_program_SPIRV_get_size"
               %glslang::program-spirv-get-size)
              :int)

(cffi:defcstruct (%glslang::shader-s :size 0))

(cffi:defctype %glslang::shader-t (:struct %glslang::shader-s))

(declaim (inline %glslang::program-add-shader))

(cffi:defcfun ("glslang_program_add_shader"
               %glslang::program-add-shader)
              :void
              (%glslang::program (:pointer %glslang::program-t))
              (%glslang::shader (:pointer %glslang::shader-t)))

(declaim (inline %glslang::program-create))

(cffi:defcfun ("glslang_program_create" %glslang::program-create)
              (:pointer %glslang::program-t)
              &rest)

(declaim (inline %glslang::program-delete))

(cffi:defcfun ("glslang_program_delete" %glslang::program-delete)
              :void
              (%glslang::program (:pointer %glslang::program-t)))

(declaim (inline %glslang::program-get-info-debug-log))

(cffi:defcfun ("glslang_program_get_info_debug_log"
               %glslang::program-get-info-debug-log)
              :string
              (%glslang::program (:pointer %glslang::program-t)))

(declaim (inline %glslang::program-get-info-log))

(cffi:defcfun ("glslang_program_get_info_log"
               %glslang::program-get-info-log)
              :string
              (%glslang::program (:pointer %glslang::program-t)))

(declaim (inline %glslang::program-link))

(cffi:defcfun ("glslang_program_link" %glslang::program-link)
              :int
              (%glslang::program (:pointer %glslang::program-t))
              (%glslang::messages :int))

(cffi:defctype %glslang::source-t %glslang::source-t)

(cffi:defctype %glslang::client-t %glslang::client-t)

(cffi:defctype %glslang::target-client-version-t
               %glslang::client-version-t)

(cffi:defctype %glslang::target-language-t
               %glslang::target-language-t)

(cffi:defctype %glslang::target-language-version-t
               %glslang::target-language-version-t)

(cffi:defctype %glslang::profile-t
               %glslang::profile-t)

(cffi:defctype %glslang::messages-t
               %glslang::messages-t)

(cffi:defcstruct (%glslang::resource-s :size 1))

(cffi:defctype %glslang::resource-t (:struct %glslang::resource-s))

(cffi:defcstruct (%glslang::input-s :size 64)
                 (%glslang::language %glslang::source-t :offset 0)
                 (%glslang::stage %glslang::stage-t :offset 4)
                 (%glslang::client %glslang::client-t :offset 8)
                 (%glslang::client-version
                  %glslang::target-client-version-t :offset 12)
                 (%glslang::target-language
                  %glslang::target-language-t :offset 16)
                 (%glslang::target-language-version
                  %glslang::target-language-version-t :offset 20)
                 (%glslang::code :string :offset 24)
                 (%glslang::default-version :int :offset 32)
                 (%glslang::default-profile %glslang::profile-t
                  :offset 36)
                 (%glslang::force-default-version-and-profile :int
                  :offset 40)
                 (%glslang::forward-compatible :int :offset 44)
                 (%glslang::messages %glslang::messages-t :offset 48)
                 (%glslang::resource (:pointer %glslang::resource-t)
                  :offset 56))

(cffi:defctype %glslang::input-t (:struct %glslang::input-s))

(declaim (inline %glslang::shader-create))

(cffi:defcfun ("glslang_shader_create" %glslang::shader-create)
              (:pointer %glslang::shader-t)
              (%glslang::input (:pointer %glslang::input-t)))

(declaim (inline %glslang::shader-delete))

(cffi:defcfun ("glslang_shader_delete" %glslang::shader-delete)
              :void
              (%glslang::shader (:pointer %glslang::shader-t)))

(declaim (inline %glslang::shader-get-info-debug-log))

(cffi:defcfun ("glslang_shader_get_info_debug_log"
               %glslang::shader-get-info-debug-log)
              :string
              (%glslang::shader (:pointer %glslang::shader-t)))

(declaim (inline %glslang::shader-get-info-log))

(cffi:defcfun ("glslang_shader_get_info_log"
               %glslang::shader-get-info-log)
              :string
              (%glslang::shader (:pointer %glslang::shader-t)))

(declaim (inline %glslang::shader-get-preprocessed-code))

(cffi:defcfun ("glslang_shader_get_preprocessed_code"
               %glslang::shader-get-preprocessed-code)
              :string
              (%glslang::shader (:pointer %glslang::shader-t)))

(declaim (inline %glslang::shader-parse))

(cffi:defcfun ("glslang_shader_parse" %glslang::shader-parse)
              :int
              (%glslang::shader (:pointer %glslang::shader-t))
              (%glslang::input (:pointer %glslang::input-t)))

(declaim (inline %glslang::shader-preprocess))

(cffi:defcfun ("glslang_shader_preprocess"
               %glslang::shader-preprocess)
              :int
              (%glslang::shader (:pointer %glslang::shader-t))
              (%glslang::input (:pointer %glslang::input-t)))

(cffi:defcstruct (%glslang::limits-s :size 1))

(cffi:defctype %glslang::executable-t
               %glslang::executable-t)

(cffi:defctype %glslang::optimization-level-t
               %glslang::optimization-toplevel-t)

(cffi:defctype %glslang::reflection-options-t
               %glslang::reflection-options-t)

(cffi:defctype %glslang::stage-mask-t
               %glslang::stage-mask-t)

(cffi:defctype %glslang::texture-sampler-transform-mode-t
               %glslang::texture-sampler-transform-mode-t)

(cffi:defctype %glslang::limits-t (:struct %glslang::limits-s))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '%glslang::client-version :%glslang)
  (export '%glslang::profile-t :%glslang)
  (export '%glslang::program-spirv-get-ptr :%glslang)
  (export '%glslang::shader-create :%glslang)
  (export '%glslang::target-client-version-t :%glslang)
  (export '%glslang::client-t :%glslang)
  (export '%glslang::program-delete :%glslang)
  (export '%glslang::initialize-process :%glslang)
  (export '%glslang::+c-iface-h-included+ :%glslang)
  (export '%glslang::reflection-options-t :%glslang)
  (export '%glslang::shader-delete :%glslang)
  (export '%glslang::stage-t :%glslang)
  (export '%glslang::reflection-options-t :%glslang)
  (export '%glslang::stage-t :%glslang)
  (export '%glslang::code :%glslang)
  (export '%glslang::language :%glslang)
  (export '%glslang::program-add-shader :%glslang)
  (export '%glslang::shader-get-info-debug-log :%glslang)
  (export '%glslang::resource-s :%glslang)
  (export '%glslang::optimization-level-t :%glslang)
  (export '%glslang::target-language-t :%glslang)
  (export '%glslang::target-language-version-t :%glslang)
  (export '%glslang::program-get-info-debug-log :%glslang)
  (export '%glslang::stage :%glslang)
  (export '%glslang::program-spirv-generate :%glslang)
  (export '%glslang::messages-t :%glslang)
  (export '%glslang::program-spirv-get-messages :%glslang)
  (export '%glslang::source-t :%glslang)
  (export '%glslang::executable-t :%glslang)
  (export '%glslang::client-t :%glslang)
  (export '%glslang::program-link :%glslang)
  (export '%glslang::limits-s :%glslang)
  (export '%glslang::messages :%glslang)
  (export '%glslang::shader-get-info-log :%glslang)
  (export '%glslang::program-create :%glslang)
  (export '%glslang::shader-parse :%glslang)
  (export '%glslang::optimization-toplevel-t :%glslang)
  (export '%glslang::target-language-t :%glslang)
  (export '%glslang::messages-t :%glslang)
  (export '%glslang::target-language :%glslang)
  (export '%glslang::program-s :%glslang)
  (export '%glslang::stage-mask-t :%glslang)
  (export '%glslang::force-default-version-and-profile :%glslang)
  (export '%glslang::program-get-info-log :%glslang)
  (export '%glslang::target-language-version-t
          :%glslang)
  (export '%glslang::program-t :%glslang)
  (export '%glslang::program-spirv-get-size :%glslang)
  (export '%glslang::texture-sampler-transform-mode-t
          :%glslang)
  (export '%glslang::limits-t :%glslang)
  (export '%glslang::shader-get-preprocessed-code :%glslang)
  (export '%glslang::shader-t :%glslang)
  (export '%glslang::resource :%glslang)
  (export '%glslang::input-s :%glslang)
  (export '%glslang::executable-t :%glslang)
  (export '%glslang::stage-mask-t :%glslang)
  (export '%glslang::texture-sampler-transform-mode-t :%glslang)
  (export '%glslang::forward-compatible :%glslang)
  (export '%glslang::source-t :%glslang)
  (export '%glslang::client-version-t
          :%glslang)
  (export '%glslang::default-version :%glslang)
  (export '%glslang::input-t :%glslang)
  (export '%glslang::+export+ :%glslang)
  (export '%glslang::default-profile :%glslang)
  (export '%glslang::target-language-version :%glslang)
  (export '%glslang::client :%glslang)
  (export '%glslang::shader-preprocess :%glslang)
  (export '%glslang::shader-s :%glslang)
  (export '%glslang::resource-t :%glslang)
  (export '%glslang::profile-t :%glslang)
  (export '%glslang::program-spirv-get :%glslang)
  (export '%glslang::finalize-process :%glslang))

