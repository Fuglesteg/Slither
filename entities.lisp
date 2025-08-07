(uiop:define-package #:slither/entities
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/render)
  (:export #:start
           #:tick
           #:defentity
           #:transform-position
           #:transform-size
           #:transform-rotation
           #:entity-find-behavior
           #:add-entity
           #:append-entity
           #:remove-entity
           #:entities-find-entity
           #:entities-find-entities
           #:transform-distance
           #:update-entities))

(in-package #:slither/entities)

(defvar *entities* '())
(defvar *entity* nil)

(defun entities-find-entity (entity-type)
  (find entity-type *entities*
        :key #'type-of))

(defun entities-find-entities (entity-type)
  (remove-if-not (lambda (entity)
                   (typep entity entity-type))
                 *entities*))

(defun update-entities ()
  (loop for entity in *entities*
        do (tick entity)))

(defun add-entity (&rest entities)
  (dolist (entity entities)
    (push entity *entities*)
    (start entity)))

(defun append-entity (&rest entities)
  (setf *entities* (append *entities* entities))
  (dolist (entity entities)
    (start entity)))

(defun remove-entity (&rest entities)
  (dolist (entity entities)
    (setf *entities* (remove entity *entities*))))

(defclass transform ()
  ((position
    :initform (vec2 0.0)
    :accessor transform-position
    :initarg :position)
   (size
    :initform (vec2 1.0 1.0)
    :accessor transform-size
    :initarg :size)
   (rotation
    :initform 0
    :accessor transform-rotation
    :initarg :rotation)))

(defmethod transform-distance ((transform1 transform) (transform2 transform))
  (vdistance (transform-position transform1)
             (transform-position transform2)))

(defclass entity (transform) 
  ((behaviors
    :accessor entity-behaviors
    :initarg :behaviors)))

(defgeneric tick (entity))
(defmethod tick ((entity entity)))

(defmethod tick :around ((entity entity))
  (let ((*entity* entity))
    (call-next-method)))

(defmethod tick :before ((entity entity))
  (loop for behavior in (entity-behaviors entity)
        do (tick behavior)))

(defgeneric start (entity))

(defmethod start :around ((entity entity))
  (let ((*entity* entity))
    (call-next-method)))

(defmethod start :before ((entity entity))
  (loop for behavior in (entity-behaviors entity)
        do (start behavior)))

(defmethod start ((entity entity)))

(defmethod entity-find-behavior ((entity entity) (behavior symbol))
  (find behavior (entity-behaviors entity)
        :key #'type-of)) 

(defmacro defentity (name slots &body sections)
  `(progn
     ,@(loop for (keyword . arguments) in sections
             when (string= keyword :behaviors)
             append arguments into behaviors
             else ; Methods
             collect (cond
                       ((string= keyword :tick)
                        (destructuring-bind (&optional entity-symbol . forms) arguments
                          `(defmethod tick ((,entity-symbol ,name))
                             ,@forms)))
                       ((string= keyword :start)
                        (destructuring-bind (&optional entity-symbol . forms) arguments
                          `(defmethod start ((,entity-symbol ,name))
                             ,@forms)))
                       (t
                        (destructuring-bind ((entity-symbol . method-arguments) . forms) arguments
                          `(defmethod ,keyword ((,entity-symbol ,name) ,@method-arguments)
                             ,@forms)))) into methods
             finally (return
                       `((defclass ,name (entity)
                           ,slots
                           (:default-initargs
                            :behaviors (list ,@behaviors)))
                         ,@methods)))))

