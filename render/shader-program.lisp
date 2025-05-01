(uiop:define-package #:slither/render/shader-program
  (:use #:cl)
  (:import-from :slither/render/uniform
                #:uniform
                #:uniform-symbol
                #:uniform-value
                #:uniform-location)
  (:import-from :slither/utils
                #:symbol->camel-case)
  (:import-from :slither/render/shader
                #:shader
                #:shader-id
                #:vertex-shader
                #:fragment-shader
                #:compile-shader)
  (:export #:shader-program-id
           #:get-uniform
           #:use-program
           #:shader-program
           #:shader-program-link
           #:shader-program-recompile
           #:make-shader-program
           #:set-uniform-value
           #:with-bound-shader-program))

(in-package :slither/render/shader-program)

(defclass shader-program ()
  ((id
    :initarg :id
    :accessor shader-program-id
    :type integer)
   (vertex-shader
    :initarg :vertex-shader
    :accessor vertex-shader
    :type shader)
   (fragment-shader
    :initarg :fragment-shader
    :accessor fragment-shader
    :type shader)
   (uniforms
    :initarg :uniforms
    :accessor uniforms
    :initform '()
    :type list)))

(defmethod program-linked-successfully-p ((program shader-program))
  (let ((id (shader-program-id program))
        (status (cffi:foreign-alloc :int :initial-element 0)))
    (%gl:get-program-iv id :link-status status)
    (let ((successfully-compiled (= 1 (cffi:mem-ref status :int))))
      (cffi:foreign-free status)
      successfully-compiled)))

(defmethod initialize-instance :after ((program shader-program) &key)
  (setf (shader-program-id program) (gl:create-program))
  (with-slots (id fragment-shader vertex-shader) program
    (gl:attach-shader id (shader-id vertex-shader))
    (gl:attach-shader id (shader-id fragment-shader))
    (shader-program-link program)))

(defmethod bind-program ((program shader-program))
  (gl:use-program (shader-program-id program)))

(defun unbind-program ()
  (gl:use-program 0))

(defmacro with-bound-shader-program (shader-program &body body)
  `(progn
     (bind-program ,shader-program)
     ,@body
     (unbind-program)))

(defgeneric set-uniform-value (program uniform value))
(defmethod set-uniform-value ((program shader-program) (uniform-symbol symbol) value)
  (set-uniform-value program (get-uniform program uniform-symbol) value))

(defmethod set-uniform-value ((program shader-program) (uniform-location integer) value)
  (with-bound-shader-program program
    (setf (uniform-value uniform-location) value)))

(defmethod get-uniform ((program shader-program) (uniform-symbol symbol))
  (find uniform-symbol (uniforms program) :key #'uniform-symbol))

(defmethod shader-program-recompile ((program shader-program))
  (with-slots (vertex-shader fragment-shader) program
    (compile-shader vertex-shader)
    (compile-shader fragment-shader))
  (shader-program-link program))

(defmethod shader-program-link ((program shader-program))
  (with-slots (id) program
    (gl:link-program id)
    #+dev (unless (program-linked-successfully-p program)
            (error "Shader program failed to link:~%~a"
                   (gl:get-program-info-log id)))
    (loop for uniform in (uniforms program)
          do (setf (uniform-location uniform)
                   (shader-program-uniform-location program
                                                    (uniform-symbol uniform))))))

(defmethod shader-program-uniform-location ((program shader-program) (uniform-symbol symbol))
  (gl:get-uniform-location (shader-program-id program)
                           (symbol->camel-case uniform-symbol)))

(defun make-uniform (program uniform-symbol)
  (make-instance 'uniform
                 :symbol uniform-symbol
                 :location (shader-program-uniform-location program uniform-symbol)))

(defun make-shader-program (&key vertex-shader fragment-shader uniform-symbols)
  (let ((shader-program (make-instance 'shader-program
                                       :vertex-shader vertex-shader
                                       :fragment-shader fragment-shader)))
    (setf (uniforms shader-program)
          (mapcar
           (lambda (uniform-symbol)
             (make-uniform shader-program uniform-symbol))
           uniform-symbols))
    shader-program))