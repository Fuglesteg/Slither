(uiop:define-package #:slither/entities
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/utils
        #:slither/render)
  (:export #:start
           #:tick
           #:defentity
           #:entity-find-behavior
           #:entity-behaviors
           #:behavior-required-behaviors
           #:with-behaviors
           #:*entity*))

(in-package #:slither/entities)

(defvar *entity* nil)

(defclass entity ()
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
