(uiop:define-package #:slither/behaviors
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/utils
        #:slither/render)
  (:import-from #:slither/entities
                #:entity
                #:entity-find-behavior
                #:behavior-required-behaviors
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
           #:directional-move
           #:sprite
           #:behavior-entity
           #:*behavior*))

(in-package #:slither/behaviors)

(defvar *behavior* nil)

(defclass behavior ()
  ((entity
    :accessor behavior-entity
    :initarg :entity
    :initform (error "Entity is required")
    :type entity)))

(defgeneric tick (behavior)
  (:method ((behavior behavior)))
  (:method :around ((*behavior* behavior))
    (declare (special *behavior*))
    (call-next-method)))

(defgeneric start (behavior)
  (:method ((behavior behavior)))
  (:method :around ((*behavior* behavior))
    (declare (special *behavior*))
    (call-next-method)))

(defmacro defbehavior (name slots &body sections)
  `(progn
     (defclass ,name (behavior) ,slots)
     ,@(loop for (keyword-or-symbol . arguments) in sections
             collect
                (cond
                  ((string= keyword-or-symbol :tick)
                     `(defmethod tick ((,(gensym) ,name))
                          ,@arguments))
                  ((string= keyword-or-symbol :start)
                     `(defmethod start ((,(gensym) ,name))
                          ,@arguments))
                  ((string= keyword-or-symbol :required-behaviors)
                   `(defmethod behavior-required-behaviors ((behavior (eql ',name)))
                      (quote ,arguments)))
                  (t (destructuring-bind (method-arguments . body) arguments
                       `(defgeneric ,(ensure-non-keyword-symbol keyword-or-symbol) (,name ,@method-arguments)
                          (:method ((entity entity) ,@method-arguments)
                            (,(ensure-non-keyword-symbol keyword-or-symbol)
                             (entity-find-behavior entity ',name)
                             ,@(lambda-list-bindings method-arguments)))
                          (:method :around ((*behavior* ,name) ,@method-arguments)
                            (declare (special *behavior*)
                                     (ignore ,@(remove-if-not (alexandria:compose #'not #'keywordp)
                                                              (lambda-list-bindings method-arguments))))
                            (let ((*entity* (behavior-entity *behavior*)))
                              (call-next-method)))
                          (:method ((*behavior* ,name) ,@method-arguments)
                            ,@body))))))))

(defbehavior rectangle
    ((color
      :accessor rectangle-color
      :initform (vec4 255 0 0 255)
      :initarg :color)
     (depth
      :accessor rectangle-depth
      :initform 0
      :initarg :depth))
  (:tick
   (with-accessors ((position transform-position)
                    (size transform-size))
       *entity*
     (draw-rectangle position size (rectangle-color *behavior*)
                     :depth (rectangle-depth *behavior*)))))

(defbehavior sprite
    ((texture
      :accessor sprite-texture
      :initarg :texture)
     (depth
      :accessor sprite-depth
      :initform 0
      :initarg :depth))
  (:tick
     (draw-texture (transform-position *entity*)
                   (transform-size *entity*)
                   (sprite-texture *behavior*)
                   :rotation (transform-rotation *entity*)
                   :depth (sprite-depth *behavior*))))

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
     (with-accessors ((position transform-position)) *entity*
       (incf (vx position) (* dx (coerce *dt* 'single-float)))
       (incf (vy position) (* dy (coerce *dt* 'single-float)))))))

(defbehavior camera
    ((zoom
      :initform 1.0
      :accessor camera-zoom
      :initarg :zoom))
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
    ((target
      :initarg :target))
  (:tick
   (with-slots (target) *behavior*
     (with-accessors ((position transform-position)) *entity*
       (setf position (transform-position target))))))

(defbehavior speaker
    ((sound
      :initarg :sound
      :accessor speaker-sound)))

(defbehavior listener
    ()
  (:tick
   (setf (listener-position) (transform-position *entity*))))

(defmethod speaker-play ((speaker speaker) (entity entity))
  (sound-play (speaker-sound speaker)
              :position (transform-position entity)))

(defmethod speaker-stop ((speaker speaker))
  (sound-stop (speaker-sound speaker)))
