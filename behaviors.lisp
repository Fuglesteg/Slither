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
           #:*behavior*
           #:move
           #:rotate
           #:transform
           #:transform-position
           #:transform-size
           #:transform-rotation
           #:transform-distance))

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

(defmacro define-behavior-accessor (behavior slot-name)
  (let ((accessor-symbol (intern (format nil "~a-~a" (symbol-name behavior) (symbol-name slot-name)))))
    `(progn
       (defun ,accessor-symbol (&optional (behavior (or *behavior*
                                                     (entity-find-behavior *entity* ',behavior))))
         (slot-value behavior ',slot-name))
       (defun (setf ,accessor-symbol) (new-value &optional (behavior (or *behavior*
                                                                      (entity-find-behavior *entity* ',behavior))))
         (setf (slot-value behavior ',slot-name) new-value)))))

(defmacro define-behavior-method (behavior name method-arguments &body body)
  `(defun ,name ,method-arguments
       (let ((*behavior* (or *behavior*
                             (entity-find-behavior *entity* ',behavior))))
         ,@body)))

(defun behavior-invoke (behavior method &rest arguments)
  (let ((*behavior* behavior)
        (*entity* (behavior-entity behavior)))
    (apply method arguments)))

(defgeneric behavior-encode (behavior))
(defgeneric behavior-decode (behavior-symbol behavior-vector entity))

(defmacro defbehavior (name slots &body sections)
  (let (methods clos-slots slot-symbols)
    (loop for slot in slots
          do (if (symbolp slot)
                 (progn (push slot slot-symbols)
                        (push (list slot
                                    :initarg (intern (symbol-name slot) :keyword))
                              clos-slots))
                 (destructuring-bind (symbol &optional default-value) slot
                   (push symbol
                         slot-symbols)
                   (push (list symbol :initform default-value :initarg (intern (symbol-name symbol) :keyword))
                         clos-slots))))
    (setf slot-symbols (nreverse slot-symbols))
    (setf clos-slots (nreverse clos-slots))
    (loop for (keyword-or-symbol . arguments) in sections
          collect
             (cond
               ((string= keyword-or-symbol :tick)
                (push
                `(defmethod tick ((,(gensym) ,name))
                   ,@arguments)
                methods))
               ((string= keyword-or-symbol :start)
                (push
                `(defmethod start ((,(gensym) ,name))
                   ,@arguments)
                methods))
               ((string= keyword-or-symbol :required-behaviors)
                (push
                `(defmethod behavior-required-behaviors ((behavior (eql ',name)))
                   (quote ,arguments))
                methods))
               (t (destructuring-bind (method-arguments . body) arguments
                    (push
                     `(define-behavior-method ,name ,(ensure-non-keyword-symbol keyword-or-symbol) ,method-arguments
                        ,@body)
                     methods)))))
    `(progn
       (defclass ,name (behavior) ,clos-slots)
       (defmethod behavior-encode ((behavior ,name))
         (let ((behavior-data (apply #'concatenate
                                     '(vector (unsigned-byte 8))
                                     (mapcar
                                      #'slither/networking::encode-argument
                                      (list ,@(loop for slot in slot-symbols
                                                    collect `(slot-value behavior ',slot)))))))
           (concatenate
            '(vector (unsigned-byte 8))
            (let ((data-length (length behavior-data)))
              (vector (ldb (byte 8 8) data-length)
                      (ldb (byte 8 0) data-length)))
            behavior-data)))
       (defmethod behavior-decode ((behavior-symbol (eql ',name)) behavior-vector entity)
         (let* ((behavior-size (vector-read-integer behavior-vector :bytes 2))
                (arguments (slither/networking::decode-arguments (subseq behavior-vector 2 behavior-size))))
           (values
            (make-instance ',name
                           :entity entity
                           ,@(loop for slot-symbol in slot-symbols
                                   for i from 0
                                   append (list (intern (symbol-name slot-symbol) :keyword)
                                                `(aref arguments ,i))))
            (+ 2 behavior-size))))
       ,@methods)))

(defbehavior transform
  ((position (vec2 0.0))
   (size (vec2 1.0 1.0))
   (rotation 0)))

(defmethod (setf transform-rotation) (new-value (entity entity))
  (setf (transform-rotation (entity-find-behavior entity 'transform))
        new-value))

(defmethod (setf transform-rotation) (new-value (transform transform))
  (setf (slot-value transform 'rotation)
        (- new-value (* 360 (floor (/ new-value 360))))))

(defmethod transform-distance ((transform1 transform) (transform2 transform))
  (vdistance (transform-position transform1)
             (transform-position transform2)))

(defun move (offset)
  (nv+ (transform-position *entity*) offset))

(defun rotate (degrees)
  (incf (transform-rotation *entity*) degrees))

(defbehavior rectangle
    ((color
      :accessor rectangle-color
      :initform (vec4 255 0 0 255)
      :initarg :color)
     (depth
      :accessor rectangle-depth
      :initform 0
      :initarg :depth))
  (:required-behaviors transform)
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
  (:required-behaviors transform)
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
      :accessor speaker-sound))
  (:speaker-play ()
   (sound-play (speaker-sound *behavior*)
               :position (transform-position *entity*)))
  (:speaker-stop ()
   (sound-stop (speaker-sound *behavior*))))

(defbehavior listener
    ()
  (:tick
   (setf (listener-position) (transform-position *entity*))))
