;;;; early.lisp

(in-package #:%shaderc)

(if (= 8 (foreign-type-size :pointer))
    (defctype size-t :uint64)
    (defctype size-t :uint32))
