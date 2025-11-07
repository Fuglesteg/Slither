(defpackage #:slither/scenes
  (:use #:cl
        #:slither/utils
        #:slither/entities)
  (:export #:defscene
           #:scene-reset
           #:entities-find-entity
           #:entities-find-entities
           #:behaviors-of-type
           #:add-entity
           #:spawn-entity
           #:remove-entity
           #:update-entities
           #:update-scene
           #:current-scene))

(in-package #:slither/scenes)

(defvar *scene* nil)

(defun current-scene ()
  *scene*)

(defun update-scene ()
  (when (current-scene)
    (tick (current-scene))))

(defun (setf current-scene) (new-scene)
  (setf *scene* new-scene)
  (scene-switch *scene*))

(defun entities-find-entity (entity-type)
  (find entity-type (scene-entities *scene*)
        :key #'type-of))

(defun entities-find-entities (entity-type)
  (remove-if-not (lambda (entity)
                   (typep entity entity-type))
                 (scene-entities *scene*)))

(defun behaviors-of-type (type)
  (loop for entity in (scene-entities *scene*)
        append (loop for behavior in (entity-behaviors entity)
                     when (typep behavior type)
                     collect behavior)))

(defun add-entity (&rest entities)
  (dolist (entity entities)
    (push entity (scene-entities *scene*))
    (start entity)))

(defun spawn-entity (name &rest initargs)
  (add-entity (apply #'make-instance name initargs)))

(defun remove-entity (&rest entities)
  (dolist (entity entities)
    (setf (scene-entities *scene*)
          (remove entity (scene-entities *scene*)))))

(defun update-entities ()
  (loop for entity in (scene-entities *scene*)
        do (tick entity)))

(defclass scene ()
  ((entities
    :initarg :entities
    :initform nil
    :accessor scene-entities
    :type list)))

(defgeneric scene-reset (scene)
  (:method ((scene scene))
    (setf (scene-entities scene) (scene-make-initial-entities scene))
    (start scene)))

(defgeneric scene-switch (scene)
  (:method ((scene scene))
    (unless (scene-entities scene)
      (scene-reset scene))
    (scene-on-switch scene))
  (:method ((scene symbol))
    (scene-switch (make-instance scene))))

(defgeneric scene-on-switch (scene)
  (:method ((scene scene))))

(defgeneric scene-make-initial-entities (scene)
  (:method ((scene scene)) nil))

(defmethod tick :before ((scene scene))
  (update-entities))

(defmethod tick ((scene scene)))

(defmethod start :before ((scene scene))
  (loop for entity in (scene-entities scene)
        do (start entity)))

(defmethod start ((scene scene)))

(defmacro defscene (name slots &body sections)
  `(progn
     (defclass ,name (scene)
       ,slots)
     ,@(loop for section in sections
              collect (destructuring-bind (keyword . body) section
                        (flet ((scene-method (method-name)
                                 (let ((instance-sym (gensym)))
                                   `(defmethod ,method-name ((,instance-sym ,name))
                                      ,@body))))
                          (cond ((string= keyword :entities)
                                 (let ((instance-sym (gensym)))
                                 `(defmethod scene-make-initial-entities ((,instance-sym ,name))
                                    (list ,@(loop for entity in body
                                                  collect (etypecase entity
                                                            (symbol `(make-instance ',entity))
                                                            (cons `(make-instance ,@(cons (list 'quote (car entity))
                                                                                          (cdr entity))))))))))
                                ((string= keyword :start)
                                 (scene-method 'start))
                                ((string= keyword :tick)
                                 (scene-method 'tick))
                                ((string= keyword :on-switch)
                                 (scene-method 'scene-on-switch))))))))
