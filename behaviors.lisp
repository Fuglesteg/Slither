(uiop:define-package #:slither/behaviors
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/utils
        #:slither/core
        #:slither/render)
  (:import-from #:slither/input
                #:key-held-p)
  (:import-from #:slither/window
                #:*dt*)
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
           #:transform-distance))

(in-package #:slither/behaviors)

(defbehavior transform
  ((position :init (vec2 0.0)
             :networked t)
   (size :init (vec2 1.0 1.0))
   (rotation :init 0
             :writer (lambda (new-value)
                       (- new-value (* 360 (floor (/ new-value 360)))))))
  (:networked t))

(defmethod transform-distance ((transform1 transform) (transform2 transform))
  (vdistance (transform-position transform1)
             (transform-position transform2)))

(defun move (offset)
  (nv+ (transform-position *entity*) offset))

(defun rotate (degrees)
  (incf (transform-rotation *entity*) degrees))

(defbehavior rectangle
    ((color :init (vec4 255 0 0 255))
     (depth :init 0))
  (:required-behaviors transform)
  (:tick
   (let* ((transform (entity-find-behavior *entity* 'transform))
          (position (transform-position transform))
          (size (transform-size transform)))
       *entity*
     (draw-rectangle position size (rectangle-color *behavior*)
                     :depth (rectangle-depth *behavior*)))))

(defbehavior sprite
    (texture
     (depth :init 0))
  (:required-behaviors transform)
  (:tick
     (draw-texture (transform-position *entity*)
                   (transform-size *entity*)
                   (sprite-texture *behavior*)
                   :rotation (transform-rotation *entity*)
                   :depth (sprite-depth *behavior*))))

(defbehavior move
    ((dx :init 0.0)
     (dy :init 0.0)
     (speed :init 1.0))
  (:required-behaviors transform)
  (:tick
   (with-slots (dx dy speed) *behavior*
     (incf dx (* dx 5.0 (coerce *dt* 'single-float) -1))
     (incf dy (* dy 5.0 (coerce *dt* 'single-float) -1))
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
     (let ((transform-position (transform-position (entity-find-behavior *entity* 'transform))))
       (nv+ transform-position
            (v* (vec2 dx dy) *dt*))))))

(defbehavior camera
    ((zoom :init 1.0))
  (:tick
   (with-accessors ((zoom camera-zoom)) *behavior*
     (when (key-held-p :i)
       (incf zoom
             *dt*))
     (when (and (key-held-p :o)
                (> zoom 0.01))
       (decf zoom
             *dt*))
     (set-camera-position (transform-position *entity*)
                          :zoom zoom
                          :rotation (transform-rotation *entity*)))))

(defbehavior follow
    (target)
  (:tick
   (with-slots (target) *behavior*
     (with-accessors ((position transform-position)) *entity*
       (setf position (transform-position target))))))

(defbehavior speaker
    (sound)
  (:speaker-play ()
   (sound-play (speaker-sound *behavior*)
               :position (transform-position *entity*)))
  (:speaker-stop ()
   (sound-stop (speaker-sound *behavior*))))

(defbehavior listener
    ()
  (:tick
   (setf (listener-position) (transform-position *entity*))))
