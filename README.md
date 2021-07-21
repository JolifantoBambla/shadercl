# shadercl
Common Lisp/CFFI bindings for [shaderc](https://github.com/google/shaderc).

`shaderc` allows you to compile GLSL and HLSL shaders to SPIR-V at runtime from your REPL.

## Requirements

### External Requirements
* [shaderc](https://github.com/google/shaderc): the target version is `v2021.1` but the API is not likely to change much, so other versions will probably work as well. `shaderc` is included in the [Vulkan SDK](https://vulkan.lunarg.com/sdk/home) (beginning with version `1.2.135`, so you might want to follow the installation instructions there.

### Common Lisp Dependencies
All CL dependencies are installable via `quicklisp`:

* alexandria
* cffi

### Supported CL implementations & Operating Systems
`shaderc` has been tested on Linux (Ubuntu 20.04) and Windows 10 (currently only tested with SBCL).

The following implementations are known to work and tested via GitHub Actions:

* SBCL
* CCL
* ABCL
* ECL

## Installation
This project is not on `quicklisp` yet, but you can clone the repository to one of your `ql:*local-project-directories*` and load it via:

```cl
(ql:quickload :shaderc)
```

## Usage

The system consists of two packages:

* `%shaderc` contains the lower level `cffi` bindings
* `shaderc` contains some higher level functions using `%shaderc`

To compile a GLSL vertex shader to a SPIR-V binary, you can use the function `shaderc:compile-to-spv`.
The result of this function can be used directly to bind the `code` slot of a `vk:shader-module-create-info` (see [vk](https://github.com/JolifantoBambla/vk#vkshadermodulecreateinfo)).

E.g.:

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

(compile-to-spv vertex-shader :vertex-shader)
```

### Compilation Options

The various compilation options the `shaderc` compiler offers, can be set via an instance of `shaderc:compile-options-set`.
Each option is represented as a slot of this class. The following is a list of the supported options:

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

Internally the `shaderc` library uses an opaque `shaderc_compile_options` handle and exposes a function for each of the options.
The function `shaderc:set-compile-options-from-set` provides an easy way of setting all options at once.
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

Note that `shaderc:compile-to-spv` takes a `shaderc:compile-options-set` as a key argument (`:options`, which defaults to `nil`) and uses `shaderc:set-compile-options-from-set` to apply all options.

If you'd rather use the `shaderc_compile_options` handle directly, you can use the low level bindings exposed by `%shaderc`.
`%shaderc:compile-options-initialize` creates an options handle and `shaderc:compile-options-release` must be used to release it again.
`shaderc:with-compile-options` provides a shorthand for this.
Individual options can then be set by calling the respective functions (e.g. `%shaderc:compile-options-set-source-language compile-options`).


### Using includes

The coolest thing about `shaderc` is that it lets you include other files using `#include` preprocessor directives.
These can either be standard includes (e.g. `#include <somefile.glsl>`) or relative includes (e.g. `#include "../some/file.glsl`).

When the `shaderc` compiler encounters an `#include` directive, it triggers a callback which must resolve the include request.
The `shaderc:compile-options-set` uses predefined default callbacks for this purpose.
**If you want to use them, make sure to explicitly pass an instance of `shaderc:compile-options-set` to `shaderc:compile-to-spv` since `:options` defaults to `nil`!**

To resolve standard includes (`#include <somefile.glsl>`) the default callbacks use the parameter `shaderc:*default-include-dirs*` which will be searched for the requested source file.

To resolve relative includes (`#include "some/file.glsl"`) the default callbacks try to resolve the file path based on the file path of the requesting source.
This can either be the `:tag` given to `shaderc:compile-to-spv` or the file name of a previously included files when resolving a nested include.
So, if you want to use relative includes in the shader source you pass to `shaderc:compile-to-spv`, make sure to also pass the file path as its `:tag` which corresponds to the "root" directory for your include.
The default callbacks also allow absolute paths when resolving relative includes (e.g. `#include "/home/shadercl-user1337/somefile.glsl"`) in which case the `:tag` (or file path of the requesting source) is ignored.

Note that enabling the `GL_GOOGLE_include_directive` in the shader is not required for includes to work.

#### Custom callbacks
Since resolving includes is left to the client (i.e. you), you can also provide custom callbacks and develop your own strategy for mapping included file paths to source files.

For this you need to provide two callbacks: one for resolving the include directive and one for releasing allocated memory when the `shaderc` compiler is done with the included sources.

The callback for resolving an include directive must return a pointer to a `%shaderc:include-result` struct, which must hold the identifier of the included source (this should be unique in the client context), the actual included source code as well as a pointer which can be used to pass some context information to the client.
If the inclusion failed the identifier (`source-name`) must be empty!
Also, in that case `content` should hold an error message instead of the included source code.
Since the C struct also has members for the size of both strings, `%shaderc` also provides a wrapper class of the same name (i.e. `include-result`) as well as translators, which allow you to create such a pointer more easily.
You can simply call:

```cl
(cffi:foreign-alloc '(:struct %shaderc:include-result)
                    (make-instance 'include-result
                                   :source-name "unique-identifier" ;; defaults to ""
                                   :content "void someFunc() {}"    ;; defaults to ""
                                   :user-data some-pointer))        ;; defaults to (cffi:null-pointer)
```

Since the client (i.e. you) owns the `include-result` and you most certainly allocated some memory when you resolved the include request, you should free this in a second callback.

Check out the documentation of the `include-resolve-callback`, `include-result-release-callback` and `user-data` options in `shaderc:compile-options-set` for information on how the signatures are supposed to look like.
