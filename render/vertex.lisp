(uiop:define-package :slither/render/vertex
  (:use :cl)
  (:import-from :alexandria
                #:with-gensyms
                #:flatten)
  (:import-from :serapeum
                #:eval-always)
  (:export :make-texture-vertex-array-object
           :make-quad-vertex-array-object
           :with-bound-vertex-array))

(in-package :slither/render/vertex)

(defmacro with-vertex-array (var &body body)
  `(let ((,var (gl:gen-vertex-array)))
     (gl:bind-vertex-array ,var)
     ,@body
     (gl:bind-vertex-array 0)
     ,var))

(defmacro with-array-buffer (var &body body)
  `(let ((,var (gl:gen-buffer)))
     (gl:bind-buffer :array-buffer ,var)
     ,@body
     (gl:bind-buffer :array-buffer 0)
     ,var))

(defmacro with-element-array-buffer (var &body body)
  `(let ((,var (gl:gen-buffer)))
     (gl:bind-buffer :element-array-buffer ,var)
     ,@body
     ; element-array-buffer should not be unbound
     ,var))

(defmacro with-bound-vertex-array (vao &body body)
  `(progn
     (gl:bind-vertex-array ,vao)
     ,@body
     (gl:bind-vertex-array 0)))

(defun cffi-type-of-vector (vector)
  (lisp-type->cffi-type (type-of (elt vector 0))))

(defun lisp-type->cffi-type (lisp-type)
  (case (if (symbolp lisp-type)
            lisp-type
            (first lisp-type))
    (single-float :float)
    (double-float :double)
    (string :string)
    (fixnum :int)
    (bit :int) ; 1 is interpreted as BIT
    (integer :int)
    (boolean :bool)
    (otherwise (error "No C conversion available for type: ~a" lisp-type))))

(defun vector->static-vector (vector)
  (static-vectors:make-static-vector (length vector)
                                     :element-type 
                                     (let ((type (type-of (elt vector 0))))
                                       (if (eql type 'bit)
                                           '(unsigned-byte 32)
                                           type))
                                     :initial-contents vector))

(defun cffi-memory-size-of-vector (vector)
  (* (length vector)
     (cffi:foreign-type-size (cffi-type-of-vector vector))))

(defun send-buffer-data (vector &key (buffer-type :array-buffer)
                                     (usage-type :static-draw))
  (%gl:buffer-data buffer-type
                   (cffi-memory-size-of-vector vector)
                   (static-vectors:static-vector-pointer 
                    (vector->static-vector vector))
                   usage-type))

(eval-always
  (defun recursive-length (list)
    (if (null list)
        0
        (destructuring-bind (&optional first . rest) list
          (+ (if (listp first)
                 (recursive-length first)
                 1)
             (recursive-length rest))))))

(defmacro define-vertex-array-object-generator (name &key vertices
                                                indices
                                                (usage-type :static-draw))
  `(defun ,name ()
     ,(with-gensyms (vao vbo ebo)
        `(with-vertex-array ,vao
           (with-array-buffer ,vbo
             (send-buffer-data ',(flatten vertices)
                               :usage-type ,usage-type)
             (with-element-array-buffer ,ebo
               (send-buffer-data ',indices
                                 :buffer-type :element-array-buffer
                                 :usage-type ,usage-type))
             ,@(loop with vertex = (first vertices)
                     with vertex-size = (* (cffi:foreign-type-size :float) 
                                           (recursive-length vertex))
                     for i from 0
                     for vertex-section in vertex
                     for vertex-section-size = (* (length vertex-section)
                                                  (cffi:foreign-type-size :float))
                     collect `(gl:vertex-attrib-pointer ,i ; INDEX 
                                                        ,(length vertex-section) ; SIZE 
                                                        :float ; TYPE 
                                                        nil ; NORMALIZED
                                                        ,vertex-size ; STRIDE: Size of all data for a vertex
                                                        ,current-pointer) ; POINTER: Stride minus 
                     collect `(gl:enable-vertex-attrib-array ,i)
                     sum vertex-section-size into current-pointer))))))

(define-vertex-array-object-generator make-quad-vertex-array-object
  :vertices (((1.0 1.0))
             ((1.0 -1.0))
             ((-1.0 -1.0))
             ((-1.0 1.0)))
  :indices (0 1 3 1 2 3))

(define-vertex-array-object-generator make-texture-vertex-array-object
  :vertices (((1.0 1.0)
              (1.0 1.0))
             ((1.0 -1.0)
              (1.0 0.0))
             ((-1.0 -1.0)
              (0.0 0.0))
             ((-1.0 1.0)
              (0.0 1.0)))
  :indices (0 1 3 1 2 3))
