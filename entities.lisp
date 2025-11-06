(uiop:define-package #:slither/entities
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/utils
        #:slither/render)
  (:export #:start
           #:tick
           #:move
           #:rotate
           #:defentity
           #:transform-position
           #:transform-size
           #:transform-rotation
           #:entity-find-behavior
           #:add-entity
           #:spawn-entity
           #:append-entity
           #:remove-entity
           #:entities-find-entity
           #:entities-find-entities
           #:transform-distance
           #:update-entities
           #:behavior-required-behaviors
           #:with-behaviors
           #:*entity*
           #:behaviors-of-type))

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

(defun behaviors-of-type (type)
  (loop for entity in *entities*
        append (loop for behavior in (entity-behaviors entity)
                     when (typep behavior type)
                     collect behavior)))

(defun update-entities ()
  (loop for entity in *entities*
        do (tick entity)))

(defun add-entity (&rest entities)
  (dolist (entity entities)
    (push entity *entities*)
    (start entity)))

(defun spawn-entity (name &rest initargs)
  (add-entity (apply #'make-instance name initargs)))

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
    :reader transform-rotation
    :initarg :rotation)))

(defun (setf transform-rotation) (new-value transform)
  (setf (slot-value transform 'rotation)
        (- new-value (* 360 (floor (/ new-value 360))))))

(defmethod transform-distance ((transform1 transform) (transform2 transform))
  (vdistance (transform-position transform1)
             (transform-position transform2)))

(defun move (offset)
  (nv+ (transform-position *entity*) offset))

(defun rotate (degrees)
  (incf (transform-rotation *entity*) degrees))

(defclass entity (transform) 
  ((behaviors
    :accessor entity-behaviors
    :initarg :behaviors)))

(defgeneric entity-make-default-behaviors (entity))
(defgeneric entity-initialize-behaviors (entity))
(defmethod entity-initialize-behaviors ((entity entity))
  (setf (entity-behaviors entity) (entity-make-default-behaviors entity)))

(defmethod initialize-instance :after ((entity entity) &key)
  (entity-initialize-behaviors entity))

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

(defgeneric behavior-required-behaviors (behavior))
(defmethod behavior-required-behaviors ((behavior t)))

(defmacro defentity (name slots &body sections)
  `(progn
     ,@(loop for (keyword . arguments) in sections
             collect (cond
                       ((string= keyword :behaviors)
                        (let ((behavior-symbols (mapcar (lambda (behavior)
                                                          (etypecase behavior
                                                            (symbol behavior)
                                                            (cons (car behavior))))
                                                        arguments)))
                          (loop for behavior in behavior-symbols
                                do (loop for required-behavior in (behavior-required-behaviors behavior)
                                         do (unless (member required-behavior behavior-symbols)
                                              (error "Behavior ~a, required by ~a not found in behavior list" required-behavior behavior)))))
                        (let ((entity-symbol (gensym)))
                          `(defmethod entity-make-default-behaviors ((,entity-symbol ,name))
                             (list ,@(loop for behavior in arguments
                                           collect (etypecase behavior
                                                     (symbol `(make-instance ',behavior :entity ,entity-symbol))
                                                     (cons `(make-instance ,@(cons (list 'quote (car behavior)) (cdr behavior))
                                                                           :entity ,entity-symbol))))))))
                        ((string= keyword :tick)
                         (let ((entity-symbol (gensym)))
                           `(defmethod tick ((,entity-symbol ,name))
                             ,@arguments)))
                        ((string= keyword :start)
                         (let ((entity-symbol (gensym)))
                           `(defmethod start ((,entity-symbol ,name))
                            ,@arguments)))
                       (t
                        (let ((entity-symbol (gensym)))
                          (destructuring-bind (method-arguments . body) arguments
                            `(defmethod ,(ensure-non-keyword-symbol keyword) ((,entity-symbol ,name) ,@method-arguments)
                               (let ((*entity* ,entity-symbol))
                               ,@body)))))) into methods
             finally (return
                       `((defclass ,name (entity)
                           ,slots)
                         ,@methods)))))

(defmacro with-behaviors (behaviors entity &body body)
  (let (behavior-binds slot-binds)
    (loop for behavior in behaviors
          do (etypecase behavior
               (symbol (push `(,behavior (entity-find-behavior ,entity ',behavior)) behavior-binds))
               (cons (destructuring-bind (slots behavior) behavior
                       (etypecase behavior
                         (symbol (push `(,behavior (entity-find-behavior ,entity ',behavior)) behavior-binds))
                         (cons (destructuring-bind (behavior-binding behavior-symbol) behavior
                                 (push `(,behavior-binding (entity-find-behavior ,entity ',behavior-symbol)) behavior-binds))))
                       (let ((behavior (etypecase behavior
                                         (symbol behavior)
                                         (cons (car behavior)))))
                         (loop for slot in slots
                               do (etypecase slot
                                    (symbol (push `(,slot (slot-value ,behavior ',slot)) slot-binds))
                                    (cons (push (destructuring-bind (slot-binding slot-symbol) slot
                                                  `(,slot-binding (slot-value ,behavior ',slot-symbol))) slot-binds)))))))))
    `(let* (,@behavior-binds
           ,@slot-binds)
       ,@body)))
