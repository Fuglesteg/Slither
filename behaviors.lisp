(uiop:define-package #:slither/behaviors
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/render)
  (:import-from #:slither/entities
                #:entity
                #:transform-size
                #:transform-position
                #:transform-rotation
                #:start
                #:tick
                #:*entity*)
  (:import-from #:slither/input
                #:key-held-p)
  (:import-from #:slither/window
                #:*dt*)
  (:import-from #:slither/audio
                #:sound-play
                #:sound-stop
                #:listener-position)
  (:export #:defbehavior
           #:speaker-play
           #:speaker-stop
           #:listener
           #:speaker
           #:camera
           #:rectangle
           #:move
           #:sprite))

(in-package #:slither/behaviors)

(defclass behavior ()
  ((entity
    :accessor behavior-entity
    :initarg :entity
    :initform (error "Entity is required")
    :type entity)))

(defmethod tick ((behavior behavior)))

(defmethod start ((behavior behavior)))

(defmacro defbehavior (name slots &body sections)
  `(progn
     (defclass ,name (behavior) ,slots)
     ,@(loop for (keyword-or-symbol . arguments) in sections
             collect
                (cond 
                  ((string= keyword-or-symbol :tick)
                   (destructuring-bind ((&optional behavior entity) . tick-body) arguments
                     `(defmethod tick ((,behavior ,name))
                        (let ((,entity *entity*))
                          ,@tick-body))))
                  ((string= keyword-or-symbol :start)
                   (destructuring-bind ((&optional behavior entity) . start-body) arguments
                     `(defmethod start ((,behavior ,name))
                        (let ((,entity *entity*))
                        ,@start-body))))
                  (t (destructuring-bind ((&optional behavior entity . method-arguments) . body) arguments
                       `(defgeneric ,keyword-or-symbol ((,behavior) ,@method-arguments)
                          (:method :around ((,behavior ,name) ,@method-arguments)
                            (let ((*entity* (behavior-entity ,behavior)))
                              (call-next-method)))
                          (:method ((,behavior ,name) ,@method-arguments)
                            (let ((,entity *entity*))
                              ,@body)))))))))

(defbehavior rectangle
    ((color
      :accessor rectangle-color
      :initform (vec4 255 0 0 255)
      :initarg :color)
     (depth
      :accessor rectangle-depth
      :initform 0
      :initarg :depth))
  (:tick (rectangle entity)
   (with-accessors ((position transform-position)
                    (size transform-size))
       entity
     (draw-rectangle position size (rectangle-color rectangle)
                     :depth (rectangle-depth rectangle)))))

(defbehavior sprite
    ((texture
      :accessor sprite-texture
      :initarg :texture)
     (depth
      :accessor sprite-depth
      :initform 0
      :initarg :depth))
  (:tick (sprite entity)
   (with-accessors ((position transform-position)
                    (size transform-size))
       entity
     (draw-texture position size (sprite-texture sprite)
                   :depth (sprite-depth sprite)))))

(defbehavior move
    ((dx
      :accessor move-dx
      :initarg :dx
      :initform 0.0
      :type single-float)
     (dy
      :accessor move-dy
      :initarg :dy
      :initform 0.0
      :type single-float)
     (speed
      :accessor move-speed
      :initarg :speed
      :initform 1.0
      :type single-float))
  (:tick (move entity)
   (with-slots (dx dy speed) move
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
     (with-accessors ((position transform-position)) entity
       (incf (vx position) (* dx (coerce *dt* 'single-float)))
       (incf (vy position) (* dy (coerce *dt* 'single-float)))))))

(defbehavior camera
    ((zoom
      :initform 1.0
      :accessor camera-zoom
      :initarg :zoom))
  (:tick (camera entity)
   (with-accessors ((zoom camera-zoom)) camera
     (when (key-held-p :i)
       (incf zoom
             *dt*))
     (when (and (key-held-p :o)
                (> zoom 0.01))
       (decf zoom
             *dt*))
     (set-camera-position (transform-position entity) zoom))))

(defbehavior follow
    ((target
      :initarg :target))
  (:tick (follow entity)
   (with-slots (target) follow
     (with-accessors ((position transform-position)) entity
       (setf position (transform-position target))))))

(defbehavior speaker
    ((sound
      :initarg :sound
      :accessor speaker-sound)))

(defbehavior listener
    ()
  (:tick (listener entity)
   (setf (listener-position) (transform-position entity))))

(defmethod speaker-play ((speaker speaker) (entity entity))
  (sound-play (speaker-sound speaker)
              :position (transform-position entity)))

(defmethod speaker-stop ((speaker speaker))
  (sound-stop (speaker-sound speaker)))
