(in-package #:shaderc)

(declaim (inline resolve-relative-include))
(defun resolve-relative-include (requested-file requesting-file)
  "Resolves a relative include request issued by a shader source.

REQUESTED-FILE can hold either an absolute path (if it starts with a \"/\") or a path relative to the REQUESTING-FILE.
REQUESTING-FILE holds the file path of the requesting source (this will be the file name or tag given to the compiler or the resolved path of a previously included file when resolving nested includes).

Returns the absolute file path (TRUENAME) of the requested file if it exists, NIL otherwise.
"
  (if (and (alexandria:starts-with-subseq "/" requested-file)
           (probe-file requested-file))
      requested-file
      (probe-file
       (merge-pathnames requested-file
                        (directory-namestring requesting-file)))))

(declaim (inline resolve-standard-include))
(defun resolve-standard-include (requested-file include-directories)
  "Resolves a standard include request issued by a shader source.

REQUESTED-FILE is the file name of the requested source.
INCLUDE-DIRECTORIES are the directories to search for REQUESTED-FILE.

Returns the absolute file path (TRUENAME) of the requested file if it exists, NIL otherwise.
"
  (loop for dir in include-directories
        for file = (probe-file (merge-pathnames requested-file dir))
        when file return file))

(declaim (inline resolve-include))
(defun resolve-include (requested-file include-type requesting-file include-dirs)
  "Resolves an include request issued by a shader source.

See RESOLVE-STANDARD-INCLUDE
See RESOLVE-RELATIVE-INCLUDE
"
  (if (eq :standard include-type)
      (resolve-standard-include requested-file include-dirs)
      (resolve-relative-include requested-file requesting-file)))

(defparameter *default-include-dirs* nil
  "A list of include directories for included shader sources used by DEFAULT-INCLUDE-RESOLVE-CALLBACK.")

(defun default-include-resolver (user-data requested-source include-type requesting-source include-depth)
  "Resolves an include request issued by a shader source.
This is the default behaviour for include requests when using COMPILE-OPTIONS-SET.

*DEFAULT-INCLUDE-DIRS* is used to resolve absolute includes.
The client context (USER-DATA) and INCLUDE-DEPTH are ignored.

See RESOLVE-INCLUDE.
"
  (let ((included-file (resolve-include requested-source
                                        include-type
                                        requesting-source
                                        *default-include-dirs*)))
    (let ((ptr (cffi:foreign-alloc
                '(:struct %shaderc:include-result)
                :initial-element
                (if included-file
                    (make-instance 'include-result
                                   :source-name (namestring included-file)
                                   :content (alexandria:read-file-into-string included-file)
                                   :user-data user-data)
                    (make-instance 'include-result
                                   :content (format nil "Could not resolve ~(~a~) include request issued by source <~a> at depth ~a"
                                                    include-type
                                                    requesting-source
                                                    include-depth)
                                   :user-data user-data)))))
      ptr)))

(cffi:defcallback default-include-resolve-callback
    :pointer
    ((user-data :pointer)
     (requested-source :string)
     (include-type %shaderc:include-type)
     (requesting-source :string)
     (include-depth %shaderc:size-t))
  (default-include-resolver user-data requested-source include-type requesting-source include-depth))

(cffi:defcallback default-include-result-release-callback
    :void
    ((user-data :pointer)
     (include-result :pointer))
  (cffi:foreign-free include-result))

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
   (include-resolve-callback
    :initarg :include-resolve-callback
    :accessor include-resolve-callback
    :initform (cffi:get-callback 'default-include-resolve-callback))
   (include-result-release-callback
    :initarg :include-result-release-callback
    :accessor include-result-release-callback
    :initform (cffi:get-callback 'default-include-result-release-callback))
   (user-data
    :initarg :user-data
    :accessor user-data
    :initform (cffi:null-pointer))
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

INCLUDE-RESOLVE-CALLBACK - An includer callback type for mapping an #include request to an include result.
  Must have the signature:
    (cffi:defcallback <name> (:pointer (:struct %shaderc:include-result)) ((user-data :pointer)
                                                                           (requested-source :string)
                                                                           (include-type %shaderc:include-type)
                                                                           (requesting-source :string)
                                                                           (include-depth %shaderc:size-t)))
  The user_data parameter specifies the client context.
  The requested_source parameter specifies the name of the source being requested.
  The type parameter specifies the kind of inclusion request being made.
  The requesting_source parameter specifies the name of the source containing the #include request.
  The includer owns the result object and its contents, and both must remain valid until the release callback is called on the result object.
  Defaults to: DEFAULT-INCLUDE-RESOLVE-CALLBACK

INCLUDE-RESULT-RELEASE-CALLBACK - An includer callback type for destroying an include result.
  Must have the signature:
    (cffi:defcallback <name> :void ((user-data :pointer)
                                    (include-result (:pointer (:struct %shaderc:include-result)))))
  Defaults to: DEFAULT-INCLUDE-RESULT-RELEASE-CALLBACK

USER-DATA - A pointer specifying the client context for INCLUDE-RESOLVE-CALLBACK and INCLUDE-RESULT-RELEASE-CALLBACK.
  Defaults to: CFFI:NULL-POINTER

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
  "Sets all options specified in an COMPILE-OPTIONS-SET on a %SHADERC:COMPILE-OPTIONS handle."
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
  (when (and (include-resolve-callback options)
             (include-result-release-callback options))
    (%shaderc:compile-options-set-include-callbacks compile-options
                                                    (include-resolve-callback options)
                                                    (include-result-release-callback options)
                                                    (user-data options)))
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
            (when options
              (set-compile-options-from-set ,compile-options ,options))
            ,@body)
       (%shaderc:compile-options-release ,compile-options))))

(defun string-to-spv (source stage entry-point tag &optional (options nil))
  "Compiles a shader source string into a SPIR-V binary and returns it as a vector of 32-bit integers.

SOURCE - the source code of the shader in GLSL or HLSL as a string.
STAGE - the stage of the shader, must be a keyword from %SHADERC:SHADER-KIND.
ENTRY-POINT - a string naming the entry point of the shader.
TAG - a unique tag to identify the shader.
  Note: if you want to resolve relative includes, TAG must be the (real or virtual) file path of the shader.
OPTIONS - a COMPILE-OPTIONS-SET

See COMPILE-OPTIONS-SET
See RESOLVE-INCLUDE

Note: the returned SPIR-V code can be used directly as the CODE slot in a VK:SHADER-MODULE-CREATE-INFO.
"
  (with-compile-options (compile-options options)
    (with-compiler (compiler)
      (let ((result (%shaderc:compile-into-spv compiler
                                               source
                                               stage
                                               tag
                                               entry-point
                                               compile-options)))
        (unwind-protect
             (if (eq :success (%shaderc:result-get-compilation-status result))
                 (let ((spv (make-array (/ (%shaderc:result-get-length result) 4)
                                        :element-type '(unsigned-byte 32)
                                        :fill-pointer 0)))
                   (loop for i from 0 below (/ (%shaderc:result-get-length result) 4)
                         do (vector-push (cffi:mem-aref (%shaderc:result-get-bytes result) :uint32 i) spv))
                   spv)
                 (error "Compilation failed with status <~a>.~%Caught ~a warnings and ~a errors.~%~%~a~%"
                        (%shaderc:result-get-compilation-status result)
                        (%shaderc:result-get-num-warnings result)
                        (%shaderc:result-get-num-errors result)
                        (%shaderc:result-get-error-message result)))
          (%shaderc:result-release result))))))
