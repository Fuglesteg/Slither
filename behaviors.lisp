(uiop:define-package #:slither/behaviors
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/utils
        #:slither/core
        #:slither/render
        #:slither/networking
        #:slither/networking/networked)
  (:import-from #:slither/input
                #:key-held-p)
  (:import-from #:slither/audio
                #:sound-play
                #:sound-stop
                #:listener-position)
  (:export #:speaker-play
           #:speaker-stop
           #:listener
           #:speaker
           #:camera
           #:rectangle
           #:directional-move
           #:sprite
           #:move
           #:rotate
           #:transform
           #:transform-position
           #:transform-size
           #:transform-rotation
           #:transform-distance
           #:render-transform
           #:render-transform-position
           #:render-transform-size
           #:render-transform-rotation))

(in-package #:slither/behaviors)

(defbehavior transform
  ((position :init (vec2 0.0)
             :networked :lag-compensation)
   (size :init (vec2 1.0 1.0)
         :networked t)
   (rotation :init 0
             :writer (lambda (new-value)
                       (coerce (- new-value (* 360 (floor (/ new-value 360))))
                               'single-float))
             :networked t))
  (:networked t))

(defmethod transform-distance ((transform1 transform) (transform2 transform))
  (vdistance (transform-position transform1)
             (transform-position transform2)))

(defun move (offset)
  (nv+ (transform-position) offset))

(defun rotate (degrees)
  (incf (transform-rotation) degrees))

(defbehavior render-transform
    ((smoothed-position :init (vec2))
     (previous-smoothed-position :init (vec2))
     (position :init (vec2))
     (smoothed-size :init (vec2))
     (previous-smoothed-size :init (vec2))
     (size :init (vec2))
     (smoothed-rotation :init 0.0)
     (previous-smoothed-rotation :init 0.0)
     (rotation :init 0.0))
  (:required-behaviors transform)
  (:post-start
   (setf (render-transform-smoothed-position) (transform-position))
   (setf (render-transform-previous-smoothed-position) (transform-position))
   (setf (render-transform-position) (transform-position))
   (setf (render-transform-smoothed-size) (transform-size))
   (setf (render-transform-previous-smoothed-size) (transform-size))
   (setf (render-transform-size) (transform-size))
   (setf (render-transform-smoothed-rotation) (transform-rotation))
   (setf (render-transform-previous-smoothed-rotation) (transform-rotation))
   (setf (render-transform-rotation) (transform-rotation)))
  (:fixed-tick
   (when (clientp)
     (setf (render-transform-previous-smoothed-position) (render-transform-smoothed-position))
     (setf (render-transform-previous-smoothed-size) (render-transform-smoothed-size))
     (setf (render-transform-previous-smoothed-rotation) (render-transform-smoothed-rotation))
     (setf (render-transform-smoothed-position)
           (vlerp (render-transform-smoothed-position)
                  (transform-position)
                  0.3))
     (setf (render-transform-smoothed-rotation)
           (rotation-lerp (render-transform-smoothed-rotation)
                          (transform-rotation)
                          0.3))
     (setf (render-transform-smoothed-size)
           (vlerp (render-transform-smoothed-size)
                  (transform-size)
                  0.3))))
  (:tick
   (when (clientp)
       (setf (render-transform-position)
             (vlerp (render-transform-previous-smoothed-position)
                    (render-transform-smoothed-position)
                    (interpolation-alpha)))
       (setf (render-transform-rotation)
             (rotation-lerp (render-transform-previous-smoothed-rotation)
                            (render-transform-smoothed-rotation)
                            (interpolation-alpha)))
       (setf (render-transform-size)
             (vlerp (render-transform-previous-smoothed-size)
                    (render-transform-smoothed-size)
                    (interpolation-alpha))))))

(defbehavior rectangle
    ((color :init (vec4 255 0 0 255))
     (depth :init 0))
  (:required-behaviors transform)
  (:tick
   (draw-rectangle (if (entity-find-behavior *entity* 'render-transform)
                       (render-transform-position)
                       (transform-position))
                   (transform-size)
                   (rectangle-color *behavior*)
                   :depth (rectangle-depth *behavior*))))

(defbehavior sprite
    (texture
     (depth :init 0)
     (layer :init 1))
  (:required-behaviors transform)
  (:tick
   (let ((render-transform (entity-find-behavior *entity* 'render-transform)))
     (draw-texture (if render-transform
                       (render-transform-position)
                       (transform-position))
                   ;; Scale transform-size by the dimensions of the texture
                   (let* ((width (texture-width (sprite-texture)))
                          (height (texture-height (sprite-texture)))
                          (sum (+ height width)))
                     (v* (transform-size)
                         (vec2 (/ sum height 2)
                               (/ sum width 2))))
                   (sprite-texture)
                   :rotation (if render-transform
                                 (render-transform-rotation)
                                 (transform-rotation))
                   :depth (sprite-depth)
                   :layer (sprite-layer)))))

(defbehavior move
  ((dx :init 0.0)
   (dy :init 0.0)
   (speed :init 1.0))
  (:required-behaviors transform)
  (:tick
   (when (networked-simulate-p)
     (with-slots (dx dy speed) *behavior*
       (incf dx (* dx 5.0 (coerce (delta-time) 'single-float) -1))
       (incf dy (* dy 5.0 (coerce (delta-time) 'single-float) -1))
       (incf dx (+ (if (key-held-p :d)
                       speed
                       0)
                   (if (key-held-p :a)
                       (* speed -1)
                       0)))
       (incf dy (+ (if (key-held-p :w)
                       speed
                       0)
                   (if (key-held-p :s)
                       (* speed -1)
                       0)))
         (nv+ (transform-position)
              (v* (vec2 dx dy) (delta-time)))))))

(defbehavior camera
  ((zoom :init 1.0))
  (:tick
   (when (and (not (slither/networking:serverp))
              (networked-simulate-p))
     (with-accessors ((zoom camera-zoom)) *behavior*
       (when (key-held-p :i)
         (incf zoom
               (delta-time)))
       (when (and (key-held-p :o)
                  (> zoom 0.01))
         (decf zoom
               (delta-time)))
       (set-camera-position (transform-position)
                            :zoom zoom
                            :rotation (transform-rotation))))))

(defbehavior follow
    (target)
  (:tick
   (with-slots (target) *behavior*
     (with-accessors ((position transform-position)) *entity*
       (setf position (transform-position target))))))

(defbehavior speaker
    (sound)
  (:speaker-play ()
   (unless (slither/networking:serverp)
     (sound-play (speaker-sound)
                 :position (transform-position))))
  (:speaker-stop ()
   (unless (slither/networking:serverp)
     (sound-stop (speaker-sound)))))

(defbehavior listener
    ()
  (:tick
   (setf (listener-position) (transform-position))))
