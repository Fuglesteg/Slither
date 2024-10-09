(defpackage #:slither/render
  (:use #:cl))

(in-package #:slither/render)

(defun gen-quad ()
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (let ((sv (get-gl-quad 0.0 0.0 2.0 2.0)))
      (%gl:buffer-data :array-buffer 
                       (* 8 (cffi:foreign-type-size :float))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw)
      (static-vectors:free-static-vector sv))
    (gl:bind-buffer :element-array-buffer ebo)
    (let ((sv (get-quad-indices-array)))
      (%gl:buffer-data :element-array-buffer 
                       (* 6 (cffi:foreign-type-size :unsigned-int))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw)
      (static-vectors:free-static-vector sv))
    (gl:vertex-attrib-pointer 0 2 :float nil (* 2 (cffi:foreign-type-size :float)) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    vao))

(defun get-gl-quad (x y width height)
  (static-vectors:make-static-vector 8 
                                     :element-type 'single-float
                                     :initial-contents (get-quad-positions x y width height)))

(defun get-quad-positions (x y width height)
  (let ((left (- x (/ width 2)))
        (right (+ x (/ width 2)))
        (down (+ y (/ height 2)))
        (up (- y (/ height 2))))
    (list 
     right up
     right down
     left down
     left up)))

(defun get-quad-indices-array ()
  (static-vectors:make-static-vector 6 
                                     :element-type '(unsigned-byte 32)
                                     :initial-contents '(0 1 3 1 2 3)))
