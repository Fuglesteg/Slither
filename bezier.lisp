(defpackage #:bezier
  (:use #:cl))

(in-package #:bezier)
(require :cl-opengl)
(require :cl-glut)

(defun generate-bezier-line (p1 p2 p3 &key (quality 2000.0))
  (loop for i from 0 to quality
        for fraction = (/ i quality)
        collect (generate-bezier-vec p1 p2 p3 fraction)))

(defun generate-bezier-vec (p1 p2 p3 i)
  `(,(generate-bezier-num (first p1) (first p2) (first p3) i)
    ,(generate-bezier-num (second p1) (second p2) (second p3) i)))

(defun generate-bezier-num (p1 p2 p3 i)
  (let* ((u (- 1.0 i))
         (uu (expt u 2))
         (ii (expt i 2)))
    (+ (* uu p1) (* 2.0 uu i p2) (* ii p3))))

(defun flatten (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
          (append (flatten (car lst)) (flatten (cdr lst)))
          (cons (car lst) (flatten (cdr lst))))))

(defclass game-window (glut:window) ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 250 :height 250
   :mode '(:single :rgb :multisample) :title "Bezier"))

(defmethod glut:display-window :before ((w game-window))
  (init-triangle)
  (setf *triangle-vao* (get-triangle))
  (link-program)
  (init-color-uniform)
  (gl:clear-color 0 0 0 0)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:display ((w game-window))
  (gl:clear :color-buffer)
  (gl:use-program *program*)
  (use-color-uniform)
  (gl:bind-buffer :array-buffer *triangle-buffer*)
  (gl:buffer-sub-data :array-buffer *triangle*)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array *triangle-vao*)
  (gl:line-width 20)
  (gl:point-size 10)
  (gl:draw-arrays :line-strip 0 (length *vec*))
  (gl:bind-vertex-array 0)
  (tick-time)
  (gl:flush)
  (setf *delta-time* (- (get-time) *frame-start*)))

(defvar *delta-time* 0)

(defvar *frame-start* 0)

(defvar *timer-start* 0)
(defvar *frames* 0)
(defvar *last-frames* 0)

(defun tick-time ()
  (if (< internal-time-units-per-second (- (get-internal-real-time) *timer-start*))
      (setf *last-frames* *frames*
            *frames* 0
            *timer-start* (get-internal-real-time))
      (incf *frames*)))

(defvar *color-uniform* nil)

(defun init-color-uniform ()
  (setf *color-uniform* (gl:get-uniform-location *program* "transformColor")))

(defun use-color-uniform ()
  (let ((time (get-time)))
    (%gl:uniform-4f *color-uniform* (sin time) (cos time) (cos (sin time)) 1.0)))

(defun get-time ()
  (/ (float (get-internal-real-time)) (float internal-time-units-per-second)))

(defmethod glut:idle ((window game-window))
  (setf *frame-start* (get-time))
  (glut:post-redisplay))

(defvar *program* nil)

(defvar *vec* nil)
(defvar *triangle* nil)

(defvar *mouse-x* 0.5)
(defvar *mouse-y* 0.5)

(defun init-triangle ()
  (setf *vec* (let ((bezier-points (flatten (generate-bezier-line '(-0.9 0.0) (list *mouse-x* *mouse-y*) '(0.9 0.0)))))
              (make-array (length bezier-points) :initial-contents bezier-points)))
  (setf *triangle* (loop with gl-array = (gl:alloc-gl-array :float (length *vec*))
      for i from 0 below (length *vec*)
      do (setf (gl:glaref gl-array i)
               (elt *vec* i))
      finally (return gl-array))))

(defvar *triangle-vao* nil)

(defun link-program ()
  (let ((program (get-shader-program)))
    (gl:link-program program)
    (setf *program* program)))

(defun get-shader-program ()
  (let ((program (gl:create-program)))
    (gl:attach-shader program (get-vertex-shader))
    (gl:attach-shader program (get-fragment-shader))
    program))

(defun get-vertex-shader ()
  (let ((vertex-shader (gl:create-shader :vertex-shader)))
    (gl:shader-source vertex-shader (uiop:read-file-string #P"./vertex.glsl"))
    (gl:compile-shader vertex-shader)
    vertex-shader))

(defun get-fragment-shader ()
  (let ((fragment-shader (gl:create-shader :fragment-shader)))
    (gl:shader-source fragment-shader (uiop:read-file-string #P"./fragment.glsl"))
    (gl:compile-shader fragment-shader)
    fragment-shader))

(defvar *triangle-buffer* nil)

(defun get-triangle ()
  (let ((vao (gl:gen-vertex-array))
        (buffer (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer buffer)
    (gl:buffer-data :array-buffer :dynamic-draw *triangle*)
    (gl:vertex-attrib-pointer 0 2 :float 0 (* 2 (cffi:foreign-type-size :float)) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    (setf *triangle-buffer* buffer)
    vao))

(defconstant %game-window% (make-instance 'game-window))

(defmethod glut:keyboard ((w game-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defmethod glut:passive-motion ((w game-window) x y)
  (declare (ignore state))
  (init-triangle)
  (setf *mouse-x* (/ x (glut:get :screen-width))
        *mouse-y* (/ y (glut:get :screen-height))))

(defun start-game ()
  (sb-thread:make-thread (lambda () (glut:display-window %game-window%))))
