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
           #:shader-program
           #:shader-program-link
           #:shader-program-recompile
           #:make-shader-program
           #:set-uniform-value
           #:with-bound-shader-program
           #:find-shader-program
           #:program-bind
           #:program-render))

(in-package :slither/render/shader-program)

(defclass shader-program ()
  ((id
    :initarg :id
    :accessor shader-program-id
    :type integer)
   (on-bind
    :initarg :on-bind
    :accessor shader-program-on-bind
    :type (function (shader-program)))
   (on-render
    :initarg :on-render
    :accessor shader-program-on-render
    :type (function (shader-program drawcall-data)))
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

(defvar *shader-programs* (make-hash-table))
(defun find-shader-program (id)
  (gethash id *shader-programs*))

(defmethod program-linked-successfully-p ((program shader-program))
  (let ((id (shader-program-id program))
        (status (cffi:foreign-alloc :int :initial-element 0)))
    (%gl:get-program-iv id :link-status status)
    (let ((successfully-compiled (= 1 (cffi:mem-ref status :int))))
      (cffi:foreign-free status)
      successfully-compiled)))

(defmethod initialize-instance :after ((program shader-program) &key)
  (setf (shader-program-id program) (gl:create-program))
  (setf (gethash (shader-program-id program) *shader-programs*) program)
  (with-slots (id fragment-shader vertex-shader) program
    (gl:attach-shader id (shader-id vertex-shader))
    (gl:attach-shader id (shader-id fragment-shader))
    (shader-program-link program)))

(defgeneric program-bind (program))

(defmethod program-bind ((program integer))
  (alexandria:when-let ((program (find-shader-program program)))
    (program-bind program)))

(defmethod program-bind ((program shader-program))
  (gl:use-program (shader-program-id program))
  (alexandria:when-let ((on-bind (shader-program-on-bind program)))
    (funcall on-bind program)))

(defgeneric program-render (program drawcall-data))

(defmethod program-render ((program integer) drawcall-data)
  (program-render (find-shader-program program) drawcall-data))

(defmethod program-render ((program shader-program) drawcall-data)
  (alexandria:when-let ((on-render (shader-program-on-render program)))
    (funcall on-render program drawcall-data))
  (%gl:draw-elements :triangles 6 :unsigned-int 0))

(defun program-unbind ()
  (gl:use-program 0))

(defmacro with-bound-shader-program (shader-program &body body)
  `(progn
     (program-bind ,shader-program)
     ,@body
     (program-unbind)))

(defmethod get-uniform ((program shader-program) (uniform-symbol symbol))
  (find uniform-symbol (uniforms program)
        :key #'uniform-symbol
        :test #'string=))

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

(defmethod shader-program-uniform-block-location ((program shader-program) (uniform-symbol symbol))
  (gl:get-uniform-block-index (shader-program-id program)
                              (symbol->camel-case uniform-symbol)))

(defmethod shader-program-bind-uniform-block ((program shader-program)
                                              (uniform-block-index integer)
                                              (buffer-id integer))
  (%gl:uniform-block-binding (shader-program-id program)
                             uniform-block-index
                             buffer-id))

(defmethod shader-program-bind-uniform-buffer ((program shader-program)
                                              (uniform-block-index integer)
                                              (buffer-id integer))
  (%gl:bind-buffer-base (shader-program-id program)
                        uniform-block-index
                        buffer-id))

(defun make-uniform (program uniform-symbol)
  (make-instance 'uniform
                 :symbol uniform-symbol
                 :location (shader-program-uniform-location program uniform-symbol)))

(defun make-shader-program (&key vertex-shader fragment-shader uniform-symbols on-bind on-render)
  (let ((shader-program (make-instance 'shader-program
                                       :vertex-shader vertex-shader
                                       :fragment-shader fragment-shader
                                       :on-bind on-bind
                                       :on-render on-render)))
    (setf (uniforms shader-program)
          (mapcar
           (lambda (uniform-symbol)
             (make-uniform shader-program uniform-symbol))
           uniform-symbols))
    shader-program))