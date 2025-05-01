(uiop:define-package #:slither/render/shader
  (:use #:cl)
  (:export #:shader-id
           #:shader
           #:vertex-shader
           #:fragment-shader
           #:compile-shader)
  (:import-from :slither/utils
                #:defmemo)
  (:import-from :slither/assets
                #:defasset
                #:asset-data))

(in-package #:slither/render/shader)

(defclass shader ()
  ((name
    :initarg :name
    :accessor shader-name)
   (id
    :initarg :id
    :type integer
    :initform 0
    :accessor shader-id)
   (type
    :initform nil
    :initarg :type
    :accessor shader-type)))

(defclass vertex-shader (shader) ()
  (:default-initargs :type :vertex-shader))

(defclass fragment-shader (shader) ()
  (:default-initargs :type :fragment-shader))

(defmethod shader-compiled-successfully-p ((shader shader))
  (let ((id (shader-id shader))
        (status (cffi:foreign-alloc :int :initial-element 0)))
    (%gl:get-shader-iv id :compile-status status)
    (let ((successfully-compiled (= 1 (cffi:mem-ref status :int))))
      (cffi:foreign-free status)
      successfully-compiled)))

(defmethod compile-shader ((shader shader))
  (let ((source (asset-data (shader-name shader)))
        (id (shader-id shader)))
    (gl:shader-source id source)
    (gl:compile-shader id)
    #+dev (unless (shader-compiled-successfully-p shader)
            (error "Shader \"~a\" compiled unsuccessfully:~%~a"
                   (shader-name shader)
                   (gl:get-shader-info-log id)))))

(defmethod initialize-instance :after ((shader shader) &key)
  (setf (shader-id shader) (gl:create-shader (shader-type shader)))
  (compile-shader shader))
