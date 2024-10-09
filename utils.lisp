(defpackage #:slither/utils
  (:use #:cl)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :continuable))

(in-package #:slither/utils)

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))
