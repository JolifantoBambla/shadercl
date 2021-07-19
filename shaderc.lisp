(in-package #:shaderc)

(defclass compile-options-set ()
  ((macros
    :initarg :macros
    :accessor macros
    :initform (make-hash-table :test 'equal)) ;; pairs of (name value)
   (lang
    :initarg :lang
    :accessor lang
    :initform :glsl) ;; :glsl or :hlsl 
   (generate-debug-info
    :initarg :generate-debug-info
    :accessor generate-debug-info
    :initform nil)
   (optimization-level
    :initarg :optimization-level
    :accessor optimization-level
    :initform :zero)
   (forced-version-profile
    :initarg :forced-version-profile
    :accessor forced-version-profile
    :initform nil)
   ;; todo: include callbacks
   (suppress-warnings
    :initarg :suppress-warnings
    :accessor suppress-warnings
    :initform nil)
   (target-env
    :initarg :target-env
    :accessor target-env
    :initform nil) ;; or a pair of (target-env env-version)
   (target-spirv
    :initarg :target-spirv
    :accessor target-spirv
    :initform nil)
   (warnings-as-errors
    :initarg :warnings-as-errors
    :accessor warnings-as-errors
    :initform nil)
   (limits
    :initarg :limits
    :accessor limits
    :initform (make-hash-table :test 'equal)) ;; pairs of (limit value)
   (auto-bind-uniforms
    :initarg :auto-bind-uniforms
    :accessor auto-bind-uniforms
    :initform nil)
   (auto-combined-image-sampler
    :initarg :auto-combined-image-sampler
    :accessor auto-combined-image-sampler
    :initform nil)
   (hlsl-io-mapping
    :initarg :hlsl-io-mapping
    :accessor hlsl-io-mapping
    :initform nil)
   (hlsl-offsets
    :initarg :hlsl-offsets
    :accessor hlsl-offsets
    :initform nil)
   (binding-bases
    :initarg :binding-bases
    :accessor binding-bases
    :initform nil) ;; tuples of (uniform-kind base &optional stage) 
   (auto-map-locations
    :initarg :auto-map-locations
    :accessor auto-map-locations
    :initform nil)
   (hlsl-register-set-and-bindings
    :initarg :hlsl-register-set-and-bindings
    :accessor hlsl-register-set-and-bindings
    :initform nil) ;; tuples of (reg set binding &optional stage)
   (enable-hlsl-functionality-1
    :initarg :enable-hlsl-functionality-1
    :accessor enable-hlsl-functionality-1
    :initform nil)
   (invert-y
    :initarg :invert-y
    :accessor invert-y
    :initform nil)
   (clamp-nan
    :initarg :clamp-nan
    :accessor clamp-nan
    :initform nil))
  (:documentation "A set of compilation options.

MACROS - A hash map of predefined macros and their values which should be added to the compilation options.
  The name of each macro must be a string. The value of a macro can be a string or NIL.
  Passing these macros to the compilation options has the same  effect as passing -Dname=value to the command-line compiler.
  If value is NIL, it has the same effect as passing -Dname to the command-line compiler.

LANG - The source language (i.e. a %SHADERC:SOURCE-LANGUAGE).
  Defaults to :GLSL.

GENERATE-DEBUG-INFO - If this is truthy, debug information is generated.

OPTIMIZATION-LEVEL - The optimization level to use during compilation (i.e. a %SHADERC:OPTIMIZATION-LEVEL.
  Defaults to :ZERO.

FORCED-VERSION-PROFILE - Forces the GLSL language version and profile to a given pair in the format: (version profile)
  The version is the same as it would appear in the #version annotation in the source.
  The profile is a %SHADERC:PROFILE.
  Version and profile specified here overrides the #version annotation in the source.

SUPPRESS-WARNINGS - If this is truthy, warnings are suppressed.
  This overrides WARNINGS-AS-ERRORS.

TARGET-ENV - The target shader environment, affecting which warnings or errors will be issued.
  The target environment must be given in a list in the following format: (target-env env-version),
  where target-env is a %SHADERC:TARGET-ENV and env-version is a %SHADERC:ENV-VERSION.

TARGET-SPIRV - The target SPIR-V version (i.e. a %SHADERC:SPIRV-VERSION).
  Generated modules will use this version of SPIR-V.

WARNINGS-AS-ERRORS - If this is truthy, warnings are treated as errors.
  This is overridden by SUPPRESS-WARNINGS if both are set.

LIMITS - A hash map of limits for resources.
  Its keys are keywords from %SHADERC:LIMIT and its values are integers.

AUTO-BIND-UNIFORMS - Sets whether the compiler should automatically assign bindings to uniforms that aren't already explictly bound.
  Defaults to: NIL

AUTO-COMBINED-IMAGE-SAMPLER - Sets whether the compiler should automatically remove sampler variables and convert image variables to combined
  image-sampler variables.
  Defaults to: NIL

HLSL-IO-MAPPING - Sets whether the compiler should use HLSL IO mapping rules for bindings.
  Defaults to: NIL

HLSL-OFFSETS - Sets whether the compiler should determine block member offsets using HLSL packing rules instead of standard GLSL rules.
  Only affects GLSL compilation. HLSL rules are always used when compiling HLSL.
  Defaults to NIL.

BINDING-BASES - A list of base binding number configuration for uniform resource types.
  Each element in the list has the format: (uniform-kind base &optional stage)
  The uniform-kind is a %SHADERC:UNIFORM-KIND, base is an unsigned integer and stage is %SHADERC:SHADER-KIND.
  If stage is not set, it affects all shader stages except for those where it is explicitly overridden by another entry in the list.

AUTO-MAP-LOCATIONS - Sets whether the compiler should automatically assign locations to uniform variables that don't have explicit
  locations in the shader source.
  Defaults to: NIL

HLSL-REGISTER-SET-AND-BINDINGS - A list of descriptor set and binding configurations for HLSL registers.
  Each element in the list has the format: (register set binding &optional stage).
  register, set and binding are all strings.
  stage is a %SHADERC:SHADER-KIND.
  If stage is not set, the descriptor set and binding for the HLSL register affect all stages except for those where it is explicitly
  overridden by another entry in the list.

ENABLE-HLSL-FUNCTIONALITY-1 - Sets whether the compiler should enable extension SPV_GOOGLE_hlsl_functionality1.
  Defaults to: NIL

INVERT-Y - Sets whether the compiler should invert position.Y output in vertex shader.
  Defaults to: NIL

CLAMP-NAN - Sets whether the compiler generates code for max and min builtins which, if given a NaN operand, will return the other operand.
  Similarly, the clamp builtin will favour the non-NaN operands, as if clamp were implemented as a composition of max and min.
  Defaults to: NIL
"))

(defun set-compile-options-from-set (compile-options options)
  (loop for name being the hash-key using (hash-value value) in (macros options) do
        (%shaderc:compile-options-add-macro-definition compile-options name value))
  (when (lang options)
    (%shaderc:compile-options-set-source-language compile-options (lang options)))
  (when (generate-debug-info options)
    (%shaderc:compile-options-set-generate-debug-info compile-options))
  (when (optimization-level options)
    (%shaderc:compile-options-set-optimization-level compile-options (optimization-level options)))
  (when (forced-version-profile options)
    (%shaderc:compile-options-set-forced-version-profile compile-options
                                                         (first (forced-version-profile options))
                                                         (second (forced-version-profile options))))
  ;; todo: include callbacks
  (when (suppress-warnings options)
    (%shaderc:compile-options-set-suppress-warnings compile-options))
  (when (target-env options)
    (%shaderc:compile-options-set-target-env compile-options
                                             (first (target-env options))
                                             (second (target-env options))))
  (when (target-spirv options)
    (%shaderc:compile-options-set-target-spirv compile-options (target-spirv options)))
  (when (warnings-as-errors options)
    (%shaderc:compile-options-set-warnings-as-errors compile-options))
  (loop for name being the hash-key using (hash-value value) in (limits options) do
        (%shaderc:compile-options-set-limit compile-options name value))
  (%shaderc:compile-options-set-auto-bind-uniforms compile-options (auto-bind-uniforms options))
  (%shaderc:compile-options-set-auto-combined-image-sampler compile-options (auto-combined-image-sampler options))
  (%shaderc:compile-options-set-hlsl-io-mapping compile-options (hlsl-io-mapping options))
  (%shaderc:compile-options-set-hlsl-offsets compile-options (hlsl-offsets options))
  (loop for b in (binding-bases options)
        if (= (length b) 3)
        do (%shaderc:compile-options-set-binding-base-for-stage compile-options (first b) (second b) (third b))
        else do (%shaderc:compile-options-set-binding-base compile-options (first b) (second b)))
  (%shaderc:compile-options-set-auto-map-locations compile-options (auto-map-locations options))
  (loop for opt in (hlsl-register-set-and-bindings options)
        if (= (length opt) 4)
        do (%shaderc:compile-options-set-hlsl-register-set-and-bindings-for-stage compile-options
                                                                                  (first opt)
                                                                                  (second opt)
                                                                                  (third opt)
                                                                                  (fourth opt))
        else do (%shaderc:compile-options-set-hlsl-register-set-and-binding compile-options
                                                                            (first opt)
                                                                            (second opt)
                                                                            (third opt)))
  (%shaderc:compile-options-set-hlsl-functionality-1 compile-options (enable-hlsl-functionality-1 options))
  (%shaderc:compile-options-set-invert-y compile-options (invert-y options))
  (%shaderc:compile-options-set-nan-clamp compile-options (clamp-nan options))
  compile-options)

(defmacro with-compiler ((compiler) &body body)
  `(let ((,compiler (%shaderc:compiler-initialize)))
     (unwind-protect
          (progn ,@body)
       (%shaderc:compiler-release ,compiler))))

(defmacro with-compile-options ((compile-options
                                 options)
                                &body body)
  `(let ((,compile-options (%shaderc:compile-options-initialize)))
     (unwind-protect
          (progn
            (set-compile-options-from-set ,compile-options ,options)
            ,@body)
       (%shaderc:compile-options-release ,compile-options))))



