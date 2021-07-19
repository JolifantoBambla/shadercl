# shadercl
Common Lisp/CFFI bindings for [shaderc](https://github.com/google/shaderc).

`shaderc` allows you to compile GLSL and HLSL shaders to SPIR-V at runtime from your REPL.

## Requirements

* [shaderc](https://github.com/google/shaderc): target version is `v2021.1`. `shaderc` is included in the [Vulkan SDK](https://vulkan.lunarg.com/sdk/home).

## Usage

The following compiles a simple vertex shader to a SPIR-V binary.
The result can be used directly to bind the `code` slot of a `vk:shader-module` (see [vk](https://github.com/JolifantoBambla/vk)).

```cl
(defparameter vertex-shader "
#version 400

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (std140, binding = 0) uniform buffer {
       mat4 mvpc;
} uniformBuffer;

layout (location = 0) in vec4 pos;
layout (location = 1) in vec4 inColor;

layout (location = 0) out vec4 outColor;

void main() {
   outColor = inColor;
   gl_Position = uniformBuffer.mvpc * pos;
}")

(string-to-spv vertex-shader :vertex-shader "main" "some-tag")
```

### Compilation Options

`shaderc` provides lots of compilation options using an opaque `shaderc_compile_options` handle which can be created using `%shaderc:compile-options-initialize`.
Individual options can then be set by calling the respective functions (e.g. `%shaderc:compile-options-set-source-language compile-options`).

On a higher level you can simply create a `compile-options-set` instance and use it to set all options at once by calling `set-compile-options-from-set`.
E.g.:

```cl
(defparameter compile-opts
  (make-instance 'compile-options-set
                 :optimization-level :size))

(let ((opts-handle (%shaderc:compile-options-initialize)))
  (set-compile-options-from-set opts-handle compile-opts)
  ;; compile some shaders ...
  (%shaderc:compile-options-release ,compile-options))
```

You can also pass such an options object to `with-compile-options` or `string-to-spv` which internally call `set-compile-options-from-set`.

You can set the following options using the class `compile-options-set`:

```cl
"MACROS - A hash map of predefined macros and their values which should be added to the compilation options.
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
  Defaults to: NIL"
```


### Using includes

You can include other files from within your shaders using `#include` preprocessor directives.
These can either be standard includes (e.g. `#include <somefile>`) or relative includes (e.g. `#include "../some/file.glsl`).

By providing a callback for include requests, you can resolve such includes yourself and provide `shaderc` with the source code of the included files.
In the callback you'll be provided with the requested resouce, the tag (this is whatever you passed the compiler as `tag` or `file-name`) and the include type (i.e. `:standard` or `:relative`).
Furthermore, you'll get the client context as a `cffi:foreign-pointer` and the current depth of inclusions as an integer.
With this information you should be able to resolve all include requests that come your way.

To tell `shaderc` whether or not an include request was successful, you'll have to construct an instance of the `include-result` class and return it from your callback.
This class has only three slots: `source-name`, `content` and `user-data`.
If you could successfully resolve the include request `souce-name` should be bound to the absolute file path of the included source file.
Otherwise it must be empty to signal `shaderc` that you could not find the requested file!
`content` must either hold the source code read from the requested file or an error message if the include request could not be resolved.
`user-data` can be used to pass a client context.

Since the includer (i.e. you) owns the include result, `shaderc` will tell you that it is done with the included resource via a second callback.
You can use this callback to free any resources you allocated when resolving the include request.

Both callbacks as well as `user-data` can be set via the `compile-options-set` using the slots `include-resolve-callback`, `include-result-release-callback` and `user-data).
For each of these, we provide default callbacks, which completely ignore the client context and instead relies on a global list of include directories (i.e. `*default-include-dirs*`) to resolve standard includes. 


## TODO

* document usage
* test includes

