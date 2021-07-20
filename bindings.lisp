;;;; bindings.lisp

(in-package #:%shaderc)

(define-foreign-library shaderc
  (:unix (:or "libshaderc_shared.so" "libshaderc_shared.so.1")))

(use-foreign-library shaderc)

;;; env.h
(defcenum (target-env :unsigned-int)
  "VULKAN: SPIR-V under Vulkan semantics
OPENGL: SPIR-V under OpenGL semantics
OPENGL-COMPAT: SPIR-V under OpenGL semantics, including compatibility profile functions
  NOTE: SPIR-V code generation is not supported for shaders under OpenGL
  compatibility profile.
WEBGPU: Deprecated, SPIR-V under WebGPU semantics"
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
  "The known versions of SPIR-V."
  (:spv-1-0 65536)
  (:spv-1-1 65792)
  (:spv-1-2 66048)
  (:spv-1-3 66304)
  (:spv-1-4 66560)
  (:spv-1-5 66816))

;;; status.h
(defcenum (compilation-status :unsigned-int)
  "Indicate the status of a compilation."
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
(defcenum (source-language :unsigned-int)
  "Source language kind."
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
  "NONE: Used if and only if GLSL version did not specify
  profiles.
COMPATIBILITY: Disabled. This generates an error"
  :none
  :core
  :compatibility ;; disabled -> produces error
  :es)

(defcenum (optimization-level :unsigned-int)
  "Optimization level.

ZERO: no optimization
SIZE: optimize towards reducing code size
PERFORMANCE: optimize towards performance"
  :zero
  :size
  :performance)

(defcenum (limit :unsigned-int)
  "Resource limits."
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
  "Uniform resource kinds.
In Vulkan, uniform resources are bound to the pipeline via descriptors
with numbered bindings and sets.

IMAGE: Image and image buffer.
SAMPLER: Pure sampler.
TEXTURE: Sampled texture in GLSL, and Shader Resource View in HLSL.
BUFFER: Uniform Buffer Object (UBO) in GLSL.  Cbuffer in HLSL.
STORAGE-BUFFER: Shader Storage Buffer Object (SSBO) in GLSL.
UNORDERED-ACCESS-VIEW: Unordered Access View, in HLSL.  (Writable storage image or storage
  buffer.)"
  :image
  :sampler
  :texture
  :buffer
  :storage-buffer
  :unordered-access-view)

(defctype compiler :pointer
  "An opaque handle to an object that manages all compiler state.")

(declaim (inline compiler-initialize))
(defcfun ("shaderc_compiler_initialize"
          compiler-initialize)
    compiler
  "Returns a shaderc_compiler_t that can be used to compile modules.
A return of NULL indicates that there was an error initializing the compiler.
Any function operating on shaderc_compiler_t must offer the basic
thread-safety guarantee.
[http://herbsutter.com/2014/01/13/gotw-95-solution-thread-safety-and-synchronization/]
That is: concurrent invocation of these functions on DIFFERENT objects needs
no synchronization; concurrent invocation of these functions on the SAME
object requires synchronization IF AND ONLY IF some of them take a non-const
argument.")

(declaim (inline compiler-release))
(defcfun ("shaderc_compiler_release"
          compiler-release)
    :void
  "Releases the resources held by the shaderc_compiler_t.
After this call it is invalid to make any future calls to functions
involving this shaderc_compiler_t."
  (compiler compiler))

(defctype compile-options :pointer
  "An opaque handle to an object that manages options to a single compilation
result.")

(declaim (inline compile-options-initialize))
(defcfun ("shaderc_compile_options_initialize"
          compile-options-initialize)
    compile-options
  "Returns a default-initialized shaderc_compile_options_t that can be used
to modify the functionality of a compiled module.
A return of NULL indicates that there was an error initializing the options.
Any function operating on shaderc_compile_options_t must offer the
basic thread-safety guarantee.")

(declaim (inline compile-options-clone))
(defcfun ("shaderc_compile_options_clone"
          compile-options-clone)
    compile-options
  "Returns a copy of the given shaderc_compile_options_t.
If NULL is passed as the parameter the call is the same as
shaderc_compile_options_init."
  (options compile-options))

(declaim (inline compile-options-release))
(defcfun ("shaderc_compile_options_release"
          compile-options-release)
    :void
  "Releases the compilation options. It is invalid to use the given
shaderc_compile_options_t object in any future calls. It is safe to pass
NULL to this function, and doing such will have no effect."
  (options compile-options))

(declaim (inline %compile-options-add-macro-definition))
(defcfun ("shaderc_compile_options_add_macro_definition"
          %compile-options-add-macro-definition)
    :void
  "Adds a predefined macro to the compilation options. This has the same
effect as passing -Dname=value to the command-line compiler.  If value
is NULL, it has the same effect as passing -Dname to the command-line
compiler. If a macro definition with the same name has previously been
added, the value is replaced with the new value. The macro name and
value are passed in with char pointers, which point to their data, and
the lengths of their data. The strings that the name and value pointers
point to must remain valid for the duration of the call, but can be
modified or deleted after this function has returned. In case of adding
a valueless macro, the value argument should be a null pointer or the
value_length should be 0u."
  (options compile-options)
  (name :string)
  (name-length size-t)
  (value :string)
  (value-length size-t))

(declaim (inline compile-options-add-macro-definition))
(defun compile-options-add-macro-definition (options name value)
  (%compile-options-add-macro-definition
   options
   name (length name)
   value (length value)))

(declaim (inline compile-options-set-source-language))
(defcfun ("shaderc_compile_options_set_source_language"
          compile-options-set-source-language)
    :void
  "Sets the source language.  The default is GLSL."
  (options compile-options)
  (lang source-language))

(declaim (inline compile-options-set-generate-debug-info))
(defcfun ("shaderc_compile_options_set_generate_debug_info"
          compile-options-set-generate-debug-info)
    :void
  "Sets the compiler mode to generate debug information in the output."
  (options compile-options))

(declaim (inline compile-options-set-optimization-level))
(defcfun ("shaderc_compile_options_set_optimization_level"
          compile-options-set-optimization-level)
    :void
  "Sets the compiler optimization level to the given level. Only the last one
takes effect if multiple calls of this function exist."
  (options compile-options)
  (level optimization-level))

(declaim (inline compile-options-set-forced-version-profile))
(defcfun ("shaderc_compile_options_set_forced_version_profile"
          compile-options-set-forced-version-profile)
    :void
  "Forces the GLSL language version and profile to a given pair. The version
number is the same as would appear in the #version annotation in the source.
Version and profile specified here overrides the #version annotation in the
source. Use profile: 'shaderc_profile_none' for GLSL versions that do not
define profiles, e.g. versions below 150."
  (options compile-options)
  (version :int)
  (profile profile))

;; Source text inclusion via #include is supported with a pair of callbacks
;; to an "includer" on the client side.  The first callback processes an
;; inclusion request, and returns an include result.  The includer owns
;; the contents of the result, and those contents must remain valid until the
;; second callback is invoked to release the result.  Both callbacks take a
;; user_data argument to specify the client context.
;; To return an error, set the source_name to an empty string and put your
;; error message in content.

(defcstruct (include-result :class c-include-result)
  "An include result.

SOURCE-NAME: The name of the source file.  The name should be fully resolved
  in the sense that it should be a unique name in the context of the
  includer.  For example, if the includer maps source names to files in
  a filesystem, then this name should be the absolute path of the file.
  For a failed inclusion, this string is empty.

CONTENT: The text contents of the source file in the normal case.
  For a failed inclusion, this contains the error message.

USER-DATA: User data to be passed along with this request."
  (source-name :string)
  (source-name-length size-t)
  (content :string)
  (content-length size-t)
  (user-data :pointer))

(defclass include-result ()
  ((source-name
    :initarg :source-name
    :accessor source-name
    :initform "")
   (content
    :initarg :content
    :accessor content
    :initform "")
   (user-data
    :initarg :user-data
    :accessor user-data
    :initform (cffi:null-pointer)))
  (:documentation  "An include result.

SOURCE-NAME - The unique name of the included resource in the context of the includer (i.e. you).
  Usually this will be the name of an included file, but since you can override how include
  requests are resolved, this can be any string.
  For failed inclusions this must be empty!

CONTENT - The source code of the included file as a string OR an error message if the inclusion failed.

USER-DATA - A CFFI:FOREIGN-POINTER that can be used for passing a client context to the include resolver.
  In the default include resolver callback this is unused.
  Defaults to: CFFI:NULL-POINTER
"))

(defmethod translate-into-foreign-memory (value (type c-include-result) ptr)
  (with-foreign-slots
      ((source-name
        source-name-length
        content
        content-length
        user-data)
       ptr
       (:struct include-result))
    (setf source-name (source-name value)
          source-name-length (length (source-name value))
          content (content value)
          content-length (length (content value))
          user-data (user-data value))))

(defmethod expand-into-foreign-memory (value (type c-include-result) ptr)
  `(with-foreign-slots
       ((source-name
         source-name-length
         content
         content-length
         user-data)
        ,ptr
        (:struct include-result))
     (setf source-name (source-name ,value)
           source-name-length (length (source-name ,value))
           content (content ,value)
           content-length (length (content ,value))
           user-data (user-data ,value))))

(defcenum include-type
  "The kinds of include requests."
  :relative  ;; e.g. #include "source
  :standard) ;; e.g. #include <source>

;; should be a cffi:callback with the following signature:
;; typedef shaderc_include_result* (*shaderc_include_resolve_fn)(
;;    void* user_data, const char* requested_source, int type,
;;    const char* requesting_source, size_t include_depth);
(defctype include-resolve-fn :pointer
  "An includer callback type for mapping an #include request to an include
result.  The user_data parameter specifies the client context.  The
requested_source parameter specifies the name of the source being requested.
The type parameter specifies the kind of inclusion request being made.
The requesting_source parameter specifies the name of the source containing
the #include request.  The includer owns the result object and its contents,
and both must remain valid until the release callback is called on the result
object.")

;; should be a cffi:callback with the following signature:
;; typedef void (*shaderc_include_result_release_fn)(
;;     void* user_data, shaderc_include_result* include_result);
(defctype include-result-release-fn :pointer
  "An includer callback type for destroying an include result.")

(declaim (inline compile-options-set-include-callbacks))
(defcfun ("shaderc_compile_options_set_include_callbacks"
          compile-options-set-include-callbacks)
    :void
  "Sets includer callback functions."
  (options compile-options)
  (resolver include-resolve-fn)
  (result-releaser include-result-release-fn)
  (user-data :pointer))

(declaim (inline compile-options-set-suppress-warnings))
(defcfun ("shaderc_compile_options_set_suppress_warnings"
          compile-options-set-suppress-warnings)
    :void
  "Sets the compiler mode to suppress warnings, overriding warnings-as-errors
mode. When both suppress-warnings and warnings-as-errors modes are
turned on, warning messages will be inhibited, and will not be emitted
as error messages."
  (options compile-options))

(declaim (inline compile-options-set-target-env))
(defcfun ("shaderc_compile_options_set_target_env"
          compile-options-set-target-env)
    :void
  "Sets the target shader environment, affecting which warnings or errors will
be issued.  The version will be for distinguishing between different versions
of the target environment.  The version value should be either 0 or
a value listed in shaderc_env_version.  The 0 value maps to Vulkan 1.0 if
|target| is Vulkan, and it maps to OpenGL 4.5 if |target| is OpenGL."
  (options compile-options)
  (target target-env)
  (version :uint32))

(declaim (inline compile-options-set-target-spirv))
(defcfun ("shaderc_compile_options_set_target_spirv"
          compile-options-set-target-spirv)
    :void
  "Sets the target SPIR-V version. The generated module will use this version
of SPIR-V.  Each target environment determines what versions of SPIR-V
it can consume.  Defaults to the highest version of SPIR-V 1.0 which is
required to be supported by the target environment.  E.g. Default to SPIR-V
1.0 for Vulkan 1.0 and SPIR-V 1.3 for Vulkan 1.1."
  (options compile-options)
  (version spirv-version))

(declaim (inline compile-options-set-warnings-as-errors))
(defcfun ("shaderc_compile_options_set_warnings_as_errors"
          compile-options-set-warnings-as-errors)
    :void
  "Sets the compiler mode to treat all warnings as errors. Note the
suppress-warnings mode overrides this option, i.e. if both
warning-as-errors and suppress-warnings modes are set, warnings will not
be emitted as error messages."
  (options compile-options))

(declaim (inline compile-options-set-limit))
(defcfun ("shaderc_compile_options_set_limit"
          compile-options-set-limit)
    :void
  "Sets a resource limit."
  (options compile-options)
  (limit limit)
  (value :int))

(declaim (inline compile-options-set-auto-bind-uniforms))
(defcfun ("shaderc_compile_options_set_auto_bind_uniforms"
          compile-options-set-auto-bind-uniforms)
    :void
  "Sets whether the compiler should automatically assign bindings to uniforms
that aren't already explicitly bound in the shader source."
  (options compile-options)
  (auto-bind :bool))

(declaim (inline compile-options-set-auto-combined-image-sampler))
(defcfun ("shaderc_compile_options_set_auto_combined_image_sampler"
          compile-options-set-auto-combined-image-sampler)
    :void
  "Sets whether the compiler should automatically remove sampler variables
and convert image variables to combined image-sampler variables."
  (options compile-options)
  (upgrade :bool))

(declaim (inline compile-options-set-hlsl-io-mapping))
(defcfun ("shaderc_compile_options_set_hlsl_io_mapping"
          compile-options-set-hlsl-io-mapping)
    :void
  "Sets whether the compiler should use HLSL IO mapping rules for bindings.
Defaults to false."
  (options compile-options)
  (hlsl-io-map :bool))

(declaim (inline compile-options-set-hlsl-offsets))
(defcfun ("shaderc_compile_options_set_hlsl_offsets"
          compile-options-set-hlsl-offsets)
    :void
  "Sets whether the compiler should determine block member offsets using HLSL
packing rules instead of standard GLSL rules.  Defaults to false.  Only
affects GLSL compilation.  HLSL rules are always used when compiling HLSL."
  (options compile-options)
  (hlsl-offsets :bool))

(declaim (inline compile-options-set-binding-base))
(defcfun ("shaderc_compile_options_set_binding_base"
          compile-options-set-binding-base)
    :void
  "Sets the base binding number used for for a uniform resource type when
automatically assigning bindings.  For GLSL compilation, sets the lowest
automatically assigned number.  For HLSL compilation, the regsiter number
assigned to the resource is added to this specified base."
  (options compile-options)
  (kind uniform-kind)
  (base :uint32))

(declaim (inline compile-options-set-binding-base-for-stage))
(defcfun ("shaderc_compile_options_set_binding_base_for_stage"
          compile-options-set-binding-base-for-stage)
    :void
  "Like shaderc_compile_options_set_binding_base, but only takes effect when
compiling a given shader stage.  The stage is assumed to be one of vertex,
fragment, tessellation evaluation, tesselation control, geometry, or compute."
  (options compile-options)
  (shader-kind shader-kind)
  (kind uniform-kind)
  (base :uint32))

(declaim (inline compile-options-set-auto-map-locations))
(defcfun ("shaderc_compile_options_set_auto_map_locations"
          compile-options-set-auto-map-locations)
    :void
  "Sets whether the compiler should automatically assign locations to
uniform variables that don't have explicit locations in the shader source."
  (options compile-options)
  (auto-map :bool))

(declaim (inline compile-options-set-hlsl-register-set-and-bindings-for-stage))
(defcfun ("shaderc_compile_options_set_hlsl_register_set_and_binding_for_stage"
          compile-options-set-hlsl-register-set-and-bindings-for-stage)
    :void
  "Sets a descriptor set and binding for an HLSL register in the given stage.
This method keeps a copy of the string data."
  (options compile-options)
  (shader-kind shader-kind)
  (reg :string)
  (set :string)
  (binding :string))

(declaim (inline compile-options-set-hlsl-register-set-and-binding))
(defcfun ("shaderc_compile_options_set_hlsl_register_set_and_binding"
          compile-options-set-hlsl-register-set-and-binding)
    :void
  "Like shaderc_compile_options_set_hlsl_register_set_and_binding_for_stage,
but affects all shader stages."
  (options compile-options)
  (reg :string)
  (set :string)
  (binding :string))

(declaim (inline compile-options-set-hlsl-functionality-1))
(defcfun ("shaderc_compile_options_set_hlsl_functionality1"
          compile-options-set-hlsl-functionality-1)
    :void
  "Sets whether the compiler should enable extension
SPV_GOOGLE_hlsl_functionality1."
  (options compile-options)
  (enable :bool))

(declaim (inline compile-options-set-invert-y))
(defcfun ("shaderc_compile_options_set_invert_y"
          compile-options-set-invert-y)
    :void
  "Sets whether the compiler should invert position.Y output in vertex shader."
  (options compile-options)
  (enable :bool))

(declaim (inline compile-options-set-nan-clamp))
(defcfun ("shaderc_compile_options_set_nan_clamp"
          compile-options-set-nan-clamp)
    :void
  "Sets whether the compiler generates code for max and min builtins which,
if given a NaN operand, will return the other operand. Similarly, the clamp
builtin will favour the non-NaN operands, as if clamp were implemented
as a composition of max and min."
  (options compile-options)
  (enable :bool))

(defctype compilation-result :pointer
  "An opaque handle to the results of a call to any shaderc_compile_into_*()
function.")

(declaim (inline %compile-into-spv))
(defcfun ("shaderc_compile_into_spv"
          %compile-into-spv)
    compilation-result
  "Takes a GLSL source string and the associated shader kind, input file
name, compiles it according to the given additional_options. If the shader
kind is not set to a specified kind, but shaderc_glslc_infer_from_source,
the compiler will try to deduce the shader kind from the source
string and a failure in deducing will generate an error. Currently only
#pragma annotation is supported. If the shader kind is set to one of the
default shader kinds, the compiler will fall back to the default shader
kind in case it failed to deduce the shader kind from source string.
The input_file_name is a null-termintated string. It is used as a tag to
identify the source string in cases like emitting error messages. It
doesn't have to be a 'file name'.
The source string will be compiled into SPIR-V binary and a
shaderc_compilation_result will be returned to hold the results.
The entry_point_name null-terminated string defines the name of the entry
point to associate with this GLSL source. If the additional_options
parameter is not null, then the compilation is modified by any options
present.  May be safely called from multiple threads without explicit
synchronization. If there was failure in allocating the compiler object,
null will be returned."
  (compiler compiler)
  (source-text :string)
  (source-text-size size-t)
  (shader-kind shader-kind)
  (input-file-name :string)
  (entry-point-name :string)
  (additional-options compile-options))

(declaim (inline compile-into-spv))
(defun compile-into-spv (compiler source-text shader-kind input-file-name entry-point additional-options)
  (%compile-into-spv compiler
                     source-text (length source-text)
                     shader-kind
                     input-file-name
                     entry-point
                     additional-options))

(declaim (inline %compile-into-spv-assembly))
(defcfun ("shaderc_compile_into_spv_assembly"
          %compile-into-spv-assembly)
    compilation-result
  "Like shaderc_compile_into_spv, but the result contains SPIR-V assembly text
instead of a SPIR-V binary module.  The SPIR-V assembly syntax is as defined
by the SPIRV-Tools open source project."
  (compiler compiler)
  (source-text :string)
  (source-text-size size-t)
  (shader-kind shader-kind)
  (input-file-name :string)
  (entry-point-name :string)
  (additional-options compile-options))

(declaim (inline compile-into-spv-assembly))
(defun compile-into-spv-assembly (compiler source-text shader-kind input-file-name entry-point additional-options)
  (%compile-into-spv-assembly compiler
                              source-text (length source-text)
                              shader-kind
                              input-file-name
                              entry-point
                              additional-options))

(declaim (inline %compile-into-preprocessed-text))
(defcfun ("shaderc_compile_into_preprocessed_text"
          %compile-into-preprocessed-text)
    compilation-result
  "Like shaderc_compile_into_spv, but the result contains preprocessed source
code instead of a SPIR-V binary module"
  (compiler compiler)
  (source-text :string)
  (source-text-size size-t)
  (shader-kind shader-kind)
  (input-file-name :string)
  (entry-point-name :string)
  (additional-options compile-options))

(declaim (inline compile-into-preprocessed-text))
(defun compile-into-preprocessed-text (compiler source-text shader-kind input-file-name entry-point additional-options)
  (%compile-into-preprocessed-text compiler
                                   source-text (length source-text)
                                   shader-kind
                                   input-file-name
                                   entry-point
                                   additional-options))

(declaim (inline %assemble-into-spv))
(defcfun ("shaderc_assemble_into_spv"
          %assemble-into-spv)
    compilation-result
  "Takes an assembly string of the format defined in the SPIRV-Tools project
(https://github.com/KhronosGroup/SPIRV-Tools/blob/master/syntax.md),
assembles it into SPIR-V binary and a shaderc_compilation_result will be
returned to hold the results.
The assembling will pick options suitable for assembling specified in the
additional_options parameter.
May be safely called from multiple threads without explicit synchronization.
If there was failure in allocating the compiler object, null will be
returned."
  (compiler compiler)
  (source-assembly :string)
  (source-assembly-size size-t)
  (additional-options compile-options))

(declaim (inline assemble-into-spv))
(defun assemble-into-spv (compiler source-assembly additional-options)
  (%assemble-into-spv compiler
                      source-assembly (length source-assembly)
                      additional-options))
 
;; The following functions, operating on shaderc_compilation_result_t objects,
;; offer only the basic thread-safety guarantee.

(declaim (inline result-release))
(defcfun ("shaderc_result_release"
          result-release)
    :void
  "Releases the resources held by the result object. It is invalid to use the
result object for any further operations."
  (result compilation-result))

(declaim (inline result-get-length))
(defcfun ("shaderc_result_get_length"
          result-get-length)
    size-t
  "Returns the number of bytes of the compilation output data in a result
object."
  (result compilation-result))

(declaim (inline result-get-num-warnings))
(defcfun ("shaderc_result_get_num_warnings"
          result-get-num-warnings)
    size-t
  "Returns the number of warnings generated during the compilation."
  (result compilation-result))

(declaim (inline result-get-num-errors))
(defcfun ("shaderc_result_get_num_errors"
          result-get-num-errors)
    size-t
  "Returns the number of errors generated during the compilation."
  (result compilation-result))

(declaim (inline result-get-compilation-status))
(defcfun ("shaderc_result_get_compilation_status"
          result-get-compilation-status)
    compilation-status
  "Returns the compilation status, indicating whether the compilation succeeded,
or failed due to some reasons, like invalid shader stage or compilation
errors."
  (result compilation-result))

(declaim (inline result-get-bytes))
(defcfun ("shaderc_result_get_bytes"
          result-get-bytes)
    :pointer ;; to the start of the compilation output data bytes
  "Returns a pointer to the start of the compilation output data bytes, either
SPIR-V binary or char string. When the source string is compiled into SPIR-V
binary, this is guaranteed to be castable to a uint32_t*. If the result
contains assembly text or preprocessed source text, the pointer will point to
the resulting array of characters."
  (result compilation-result))

(declaim (inline result-get-error-message))
(defcfun ("shaderc_result_get_error_message"
          result-get-error-message)
    :string
  "Returns a null-terminated string that contains any error messages generated
during the compilation."
  (result compilation-result))

(declaim (inline get-spv-version))
(defcfun ("shaderc_get_spv_version"
          get-spv-version)
    :void
  "Provides the version & revision of the SPIR-V which will be produced"
  (version (:pointer :uint))
  (revision (:pointer :uint)))

(declaim (inline parse-version-profile))
(defcfun ("shaderc_parse_version_profile"
          parse-version-profile)
    :bool
  "Parses the version and profile from a given null-terminated string
containing both version and profile, like: '450core'. Returns false if
the string can not be parsed. Returns true when the parsing succeeds. The
parsed version and profile are returned through arguments."
  (str :string)
  (version (:pointer :int))
  (profile (:pointer profile)))

