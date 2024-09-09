(defpackage #:slither
  (:use #:cl #:org.shirakumo.fraf.math.vectors #:org.shirakumo.fraf.math.matrices))

(in-package #:slither)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defclass shader ()
  ((path
    :initarg :path
    :accessor path
    :type pathname)
   (id
    :initarg :id
    :type integer
    :initform 0
    :accessor id)
   (shader-type
    :initform nil
    :initarg :shader-type
    :accessor shader-type)))

(defclass vertex-shader (shader) ()
  (:default-initargs :shader-type :vertex-shader))

(defclass fragment-shader (shader) ()
  (:default-initargs :shader-type :fragment-shader))

(defun shader-compiled-successfully-p (shader-id)
  (let ((status (cffi:foreign-alloc :int)))
    (%gl:get-shader-iv shader-id :compile-status status)
    (let ((successfully-compiled (= 1 (cffi:mem-ref status :int))))
      (cffi:foreign-free status)
      successfully-compiled)))

(defmethod initialize-instance :after ((shader shader) &key)
  (let ((shader-id (gl:create-shader (shader-type shader))))
    (gl:shader-source shader-id (uiop:read-file-string (path shader)))
    (gl:compile-shader shader-id)
    (let ((shader-successfully-compiled (shader-compiled-successfully-p shader-id)))
      (if (not shader-successfully-compiled)
        (format t "~a:~%~a" (path shader) (gl:get-shader-info-log shader-id))
        (setf (id shader) shader-id)))))

(defvar *game-objects* '())

(defvar *shader-program* nil)

(defvar *bg* nil)

(defun init ()
  (setf *shader-program* (make-instance 'shader-program 
                 :vertex-shader (make-instance 'vertex-shader :path #P"./vertex.glsl")
                 :fragment-shader (make-instance 'fragment-shader :path #P"./circle.glsl")
                 :uniforms (list
                            (make-instance 'uniform :name "position")
                            (make-instance 'uniform :name "screenSize")
                            (make-instance 'uniform :name "zoom"))))
  (setf *bg* (gen-quad))
  (gl:use-program (id *shader-program*))
  (%gl:uniform-2f (id (get-uniform *shader-program* "screenSize")) (screen-width) (screen-height))
  (set-circles))

(defvar *circles* '())

(defun random-float (&optional (range 1) (precision 10))
  (float (* (/ (random precision) precision) range)))

(defun wrap-in-uniform (value uniform-location &key (type 'uniform-wrapper))
  (make-instance type
                 :value value
                 :id uniform-location))

(defun make-random-circle (uniform-location)
  (make-instance 'circle 
                 :location (wrap-in-uniform
                            (vec2 (random-float 2)
                                  (random-float 2))
                            uniform-location)
                 :rotation (random 360)
                 :radius (wrap-in-uniform
                          (/ (- 1.2 (random-float)) 2)
                          (+ uniform-location 1)
                          :type 'radius)
                 :color (wrap-in-uniform
                         (random-color)
                         (+ uniform-location 2))))

(defun set-circles ()
  (let* ((circles-uniform-location 2)
        (circles (loop repeat 20
                       for i from circles-uniform-location by 3
                       collect (make-random-circle i))))
    (setf *circles* circles)))

(defun screen-width ()
  (float (glut:get :screen-width)))

(defun screen-height ()
  (float (glut:get :screen-height)))

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

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

#+micros
(defun read-repl ()
  (continuable
    (let ((connection (or micros/swank-api:*emacs-connection* (micros::default-connection))))
      (when connection
        (micros::handle-requests connection t)))))

(defun update ()
  (sleep 0.01)
  (update-dt)
  (loop for circle in *circles*
        do (tick circle))
  #+micros (read-repl))

(defun render ()
  (gl:clear :color-buffer)
  (gl:use-program (id *shader-program*))
  (gl:bind-vertex-array *bg*)
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  (gl:bind-vertex-array 0)
  (gl:flush))

(defclass game-window (glut:window) ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 250 :height 250
   :mode '(:single :rgb :multisample) :title "Snake game"))

(defparameter *window* (make-instance 'game-window))

(defun start-game ()
  (glut:display-window (make-instance 'game-window)))

(defmethod glut:reshape ((w game-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (if (<= width height)
      (gl:ortho -5 5 (/ (* -5 height) width)
                (/ (* 5 height) width) -5 5)
      (gl:ortho (/ (* -5 width) height) (/ (* 5 width) height)
                -5 5 -5 5))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:display-window :before ((w game-window))
  (init)
  (setf *zoom* (make-instance 'uniform-wrapper 
                              :value 0.1
                              :name "zoom"
                              :id (id (get-uniform *shader-program* "zoom"))))
  (gl:clear-color 0 0 0 0)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:idle ((window game-window))
  (glut:post-redisplay))

(defmethod glut:display ((window game-window))
  (update)
  (render))

(defclass transform ()
  ((location
    :initform (make-instance 'vec2)
    :accessor location
    :initarg :location)
   (rotation
    :initform 0
    :accessor rotation
    :initarg :rotation)))

(defclass game-object (transform)
   ((color
    :initform (make-instance 'vec4)
    :initarg :color
    :accessor color)))

(defclass circle (game-object)
  ((radius
    :initform 0.0
    :initarg :radius)))

(defmethod location ((circle circle))
  (value (slot-value circle 'location)))

(defmethod (setf location) ((location vec2) (circle circle))
  (setf (slot-value (slot-value circle 'location) 'value) location))

(defmethod (setf location) :after ((location vec2) (circle circle))
  (setf (uniform-value (slot-value circle 'location))
        (m* (nmtranslate
             (nmscale (meye 3) (vec2 (value *zoom*)))
             *camera-position*)
            *model-matrix*
            location)))

(defmethod radius ((circle circle))
  (value (slot-value circle 'radius)))

(defmethod (setf radius) ((radius float) (circle circle))
  (setf (slot-value (slot-value circle 'radius) 'value) radius))

(defmethod (setf radius) :after ((radius float) (circle circle))
  (setf (uniform-value (slot-value circle 'radius))
        (* radius (value *zoom*))))

(defun rotation->vec2 (rotation)
  (vscale (vec2 (cos rotation)
                (sin rotation))
          1))

(defun random-point-in-bounds ()
  (vec2 (random-float 2) (random-float 2)))

(defun randomly-negate (number)
  (case (random 2)
    (0 (- number))
    (1 number)))

(defun radians->degrees (radians)
  (* radians (/ 180 pi)))

(defmethod angle-towards ((from vec2) (to vec2))
  (let ((direction (vscale (v- to from) 1)))
    (radians->degrees (atan (vx direction) (vy direction)))))

(defun random-point-outside-bounds (&optional (distance 2))
  (vec2 (randomly-negate (+ distance (random-float distance)))
        (randomly-negate (+ distance (random-float distance)))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun random-color ()
  (vec3 (random-float) (random-float) (random-float)))

(defmethod tick ((circle circle))
  (with-slots (rotation radius) circle
    (if (out-of-bounds-p circle :forgiveness (+ 2 (value radius)))
        (let ((point-outside (random-point-outside-bounds))
              (point-inside (random-point-in-bounds)))
          (setf (location circle) point-outside
                (rotation circle) (angle-towards point-outside point-inside)))
        (let ((direction (rotation->vec2 rotation)))
          (setf (location circle) (v+ (location circle) (v/ direction 100)))))))

(defgeneric out-of-bounds-p (circle &key forgiveness))

(defmethod out-of-bounds-p ((circle circle) &key (forgiveness 0))
  (let ((location (location circle))
        (radius
          (radius circle)))
    (out-of-bounds-p location :forgiveness (+ forgiveness radius))))

(defmethod out-of-bounds-p ((point vec2) &key (forgiveness 0))
  (with-vec (x y) point
     (or (< (+ x forgiveness) 0)
         (> (- x forgiveness) 2)
         (< (+ y forgiveness) 0)
         (> (- y forgiveness) 2))))

(defclass uniform () 
  ((id
    :type integer
    :accessor id
    :initform 0
    :initarg :id)
   (name
    :type string
    :accessor name
    :initarg :name)))

(defvar *model-matrix* (meye 3))
(defvar *camera-position* (vec2 1.0))
(defvar *view-matrix* (nmtranslate (nmscale (meye 3) (vec2 0.5 0.5)) *camera-position*))
(defvar *zoom* nil)

(defmethod (setf uniform-value) :after ((value float) (uniform (eql *zoom*)))
  (dolist (circle *circles*)
    (uniform-sync (slot-value circle 'radius))))

(defmethod (setf uniform-value) ((value float) (uniform uniform))
  (%gl:uniform-1f (id uniform) value))

(defmethod (setf uniform-value) ((vec vec2) (uniform uniform))
  (with-vec (x y) vec
    (%gl:uniform-2f (id uniform) x y)))

(defmethod (setf uniform-value) ((vec vec3) (uniform uniform))
  (with-vec (x y z) vec
    (%gl:uniform-3f (id uniform) x y z))) 

(defclass uniform-wrapper (uniform)
  ((value
    :reader value
    :initarg :value)))

(defgeneric uniform-sync (uniform-wrapper))
(defmethod uniform-sync ((uniform-wrapper uniform-wrapper))
  (setf (uniform-value uniform-wrapper) (value uniform-wrapper)))

(defclass radius (uniform-wrapper) ())

(defmethod uniform-sync ((radius radius))
  (setf (uniform-value radius) (* (value radius) (value *zoom*))))

(defmethod initialize-instance :after ((uniform-wrapper uniform-wrapper) &key)
  (uniform-sync uniform-wrapper))

(defmethod (setf value) (value (uniform-wrapper uniform-wrapper))
  (setf (slot-value uniform-wrapper 'value) value))

(defmethod (setf value) :after (value (uniform-wrapper uniform-wrapper))
  (uniform-sync uniform-wrapper))

(defclass shader-program ()
  ((id
    :initarg :id
    :accessor id
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

(defmethod initialize-instance :after ((program shader-program) &key)
  (let ((program-id (gl:create-program)))
    (with-slots (vertex-shader fragment-shader) program
      (gl:attach-shader program-id (id vertex-shader))
      (gl:attach-shader program-id (id fragment-shader))
      (gl:link-program program-id)
      ;; Init uniforms
      (when (uniforms program)
        (mapcar
         (lambda (uniform)
           (setf (id uniform) (gl:get-uniform-location program-id (name uniform))))
         (uniforms program)))
      (setf (id program) program-id))))

(defmethod get-uniform ((program shader-program) (name string))
  (find-if (lambda (uniform) (string= name (name uniform))) (uniforms program)))

(defmethod glut:keyboard ((w game-window) key x y)
  (declare (ignore x y))
  (case key
    (#\i (init))
    (#\w (decf (vy *camera-position*) 0.1))
    (#\s (incf (vy *camera-position*) 0.1))
    (#\d (decf (vx *camera-position*) 0.1))
    (#\a (incf (vx *camera-position*) 0.1))
    (#\+ (setf (value *zoom*) (+ (value *zoom*) 0.1)))
    (#\- (setf (value *zoom*) (- (value *zoom*) 0.1)))
    (#\Esc (glut:destroy-current-window))))

(defvar *last-time* 0)
(defvar *dt* 0)

(defun get-time ()
  (glut:get :elapsed-time))

(defun update-dt ()
  (let ((time (get-time)))
    (setf *dt* (- time *last-time*)
          *last-time* time)))

(defmethod glut:passive-motion ((window game-window) x y)
  (let ((height (float (screen-height)))
        (width (float (screen-width))))
  (%gl:uniform-2f (id (get-uniform *shader-program* "position"))
                  (float (* 2 (/ x width)))
                  (- 1.5 (float (* 1.5 (/ y height)))))))

(defun get-fps ()
  (float (/ 1000 *dt*)))
