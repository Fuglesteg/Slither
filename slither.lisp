(uiop:define-package #:slither
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/utils
        #:slither/render
        #:slither/window
        #:slither/input)
  (:import-from :slither/render/uniform
                #:uniform-value
                #:uniform-location)
  (:import-from :slither/render/shader-program
                #:shader-program
                #:make-shader-program
                #:set-uniform-value
                #:get-uniform)
  (:export #:start-game
           #:defentity
           #:add-entity
           #:base-uniform-location
           #:uniform-size))

(in-package #:slither)

(defvar *entities* '())

(defun start-game (&optional start-procedure)
  (with-window
    (when start-procedure
      (funcall start-procedure))
    (renderer-init)
    (with-event-loop
      (update-entities))))

(defun update-entities ()
  (gl:clear :color-buffer)
  (loop for entity in *entities*
        do (tick entity))
  (gl:flush))

(defun render-entities ()
  )

;;; Entities

(defun add-entity (&rest entities)
  (dolist (entity entities)
    (push entity entities)
    (start entity)))

(defun remove-entity (entity)
  (setf *entities* (remove entity *entities*)))

(defclass transform ()
  ((position
    :initform (vec2 0.0)
    :accessor transform-position
    :initarg :position)
   (size
    :initform (vec2 100.0 100.0)
    :accessor transform-size
    :initarg :size)
   (rotation
    :initform 0
    :accessor transform-rotation
    :initarg :rotation)))

(defclass entity (transform) 
  ((behaviors
    :accessor entity-behaviors
    :initarg :behaviors)))

(defgeneric tick (entity))

(defmethod tick ((entity entity))
  (loop for behavior in (entity-behaviors entity)
        do (behavior-tick behavior entity)))

(defgeneric start (entity))

(defmethod start ((entity entity))
  (loop for behavior in (entity-behaviors entity)
        do (behavior-start behavior entity)))

(defmacro defentity (name slots &body sections)
  `(progn
     ,@(loop for (keyword . arguments) in sections
             with renderer = nil
             when (string= keyword :tick)
             collect (destructuring-bind (&optional entity-symbol . forms) arguments
                       `(defmethod tick ((,entity-symbol ,name))
                        ,@forms)) into methods
             when (string= keyword :start)
             collect (destructuring-bind (&optional entity-symbol . forms) arguments
                       `(defmethod start ((,entity-symbol ,name))
                          ,@forms)) into methods
             when (string= keyword :behaviors)
             collect arguments into behaviors
             when (string= keyword :uniforms)
             append (loop for uniform in arguments
                           for i from 0
                           collect `(defmethod (setf ,uniform) :after (value (,name ,name))
                                      (with-slots (shader-program) ,name
                                        (set-uniform-value shader-program ,i value))))
                 into methods
             and
             collect `(defmethod uniform-size ((,name ,name))
                       ,(length arguments))
                 into methods
             and
             collect '(base-uniform-location
                      :initarg :base-uniform-location
                      :initform 0
                      :accessor base-uniform-location)
                 into extra-slots
             finally (return
                       `((defclass ,name (entity)
                           (,@slots ,@extra-slots)
                           (:default-initargs
                            :behaviors ,behaviors))
                         ,@methods)))))

;;; Behaviors

(defclass behavior () ())

(defgeneric behavior-tick (behavior entity))
(defgeneric behavior-start (behavior entity))

(defmacro defbehavior (name slots &body sections)
  `(progn
     (defclass ,name (behavior) ,slots)
     ,@(loop for (keyword . arguments) in sections
             when (string= keyword :tick)
             collect (destructuring-bind ((&optional behavior entity) . tick-body) arguments
                       `(defmethod behavior-tick ((,name ,behavior) (entity ,entity))
                         ,@tick-body))
             when (string= keyword :start)
             collect (destructuring-bind ((&optional behavior entity) . start-body) arguments
                       `(defmethod behavior-start ((,name ,behavior) (,entity entity))
                         ,@start-body)))))

(defbehavior rectangle
    ((color
      :accessor rectangle-color
      :initform (vec4 255 255 255 255)
      :initarg :color))
  (:tick (rectangle entity)
   (with-accessors ((position transform-position)
                    (size transform-size))
       entity
     (draw-rectangle position size (rectangle-color rectangle)))))

(defbehavior move
    ((dx
      :accessor move-dx
      :initarg :dx
      :initform 0)
     (dy
      :accessor move-dy
      :initarg :dy
      :initform 0))
  (:tick (move entity)
   (with-slots (dx dy) move
     (incf dx (* dx 5 *dt* -1))
     (incf dy (* dy 5 *dt* -1))
     (incf dx (+ (if (key-held-p :d)
                     1
                     0)
                 (if (key-held-p :a)
                     -1
                     0)))
     (incf dy (+ (if (key-held-p :w)
                     1
                     0)
                 (if (key-held-p :s)
                     -1
                     0)))
     (with-accessors ((position transform-position)) entity
       (incf (vx position) (* dx *dt*))
       (incf (vy position) (* dy *dt*))))))

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

#+nil(let ((player (make-instance 'entity
                             :size (vec2 1.0 1.0)
                             :behaviors (list 
                                         (make-instance 'rectangle
                                                        :color (vec4 255 0 0 255))
                                         (make-instance 'move)))))
(setf *entities*
      (list
       (make-instance 'entity
                      :size (vec2 0.2 0.2)
                      :behaviors (list 
                                  (make-instance 'camera)))
       (make-instance 'entity
                      :position (vec2 2 3)
                      :size (vec2 1.0 1.0)
                      :behaviors (list 
                                  (make-instance 'rectangle
                                                 :color (vec4 255 255 255 255))))
       (make-instance 'entity
                      :size (vec2 1.0 1.0)
                      :behaviors (list 
                                  (make-instance 'rectangle
                                                 :color (vec4 255 255 255 255))))
       player)))