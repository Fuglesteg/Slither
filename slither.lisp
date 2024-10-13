(defpackage #:slither
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/render
        #:slither/window
        #:slither/input)
  (:export :start-game
           :define-game-object))

(in-package #:slither)

(defvar *entities* '())

(defun start-game ()
  (with-game-loop 
    (update-entities) 
    (render-entities)))

(defmacro defentity (name slots &body sections)
  `(progn
     ,@(loop for (keyword entity-symbol . forms) in sections
             with renderer = nil
             when (eql keyword 'tick)
             collect `(defmethod tick ((,entity-symbol ,name))
                        ,@forms) into methods
             when (eql keyword 'start)
             collect `(defmethod start ((,entity-symbol ,name))
                        ,@forms) into methods
             when (eql keyword 'behaviors)
             collect (cons entity-symbol forms) into behaviors
             when (eql keyword 'renderer)
             do (setf renderer entity-symbol)
             finally (return
                `((defclass ,name '(entity)
                   (,@slots)
                    (:default-initargs
                    :behaviors ,behaviors
                    :renderer ,(if (symbolp renderer)
                                   `(make-instance ,renderer)
                                   renderer)))
                  ,@methods)))))

(defentity player
  ((speed
    :initform (vec2 0.0 0.0)))
  (renderer quad)
  (tick player
        (with-slots (location speed) player
          (let ((speed-up-vector 
                  (vec2 (+ (if (key-held-p #\d) 1.0 0.0)
                           (if (key-held-p #\a) -1.0 0.0))
                        (+ (if (key-held-p #\w) 1.0 0.0)
                           (if (key-held-p #\s) -1.0 0.0)))))
            (setf speed (v+ speed speed-up-vector))
            (setf location (v+ location speed))))))

(defun update-entities ()
  (loop for entity in *entities*
        do (tick entity)))

(defun render-entities ()
  (gl:clear :color-buffer)
  (loop for entity in *entities*
        do (render entity))
  (gl:flush))

(defclass transform ()
  ((location
    :initform (make-instance 'vec2)
    :accessor location
    :initarg :location)
   (rotation
    :initform 0
    :accessor rotation
    :initarg :rotation)))

(defclass entity (transform) 
  ((renderer
    :initarg :renderer)
   (behaviors
    :accessor behaviors
    :initarg :behaviors)))

(defgeneric tick (entity))
(defgeneric start (entity))
(defmethod render ((entity entity))
  (with-slots (renderer) entity
    (when renderer 
      (render renderer))))

(defvar *model-matrix* (meye 3))
(defvar *camera-position* (vec2 1.0))
(defvar *view-matrix* (nmtranslate (nmscale (meye 3) (vec2 0.5 0.5)) *camera-position*))