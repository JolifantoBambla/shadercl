# shadercl
Common Lisp/CFFI bindings for [shaderc](https://github.com/google/shaderc).

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

## TODO

* document usage
* test includes

