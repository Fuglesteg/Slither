(defpackage #:slither/scenes
  (:use #:cl 
        #:slither/utils)
  (:import-from #:slither/entities
                #:start
                #:tick)
  (:export #:defscene))

(in-package #:slither/scenes)

(defvar *scene* nil)

(defclass scene ()
  ((entities
    :initarg :entities
    :initform nil
    :accessor scene-entities
    :type list)))

(defgeneric scene-reset (scene)
  (:method ((scene scene))
    (setf (scene-entities scene) (scene-make-initial-entities scene))))

(defgeneric scene-switch (scene)
  (:method ((scene scene))
    (when (scene-entities scene)
      (scene-reset scene))
    (setf slither/entities::*entities* (scene-entities scene))
    (scene-on-switch scene))
  (:method ((scene symbol))
    (scene-switch (make-instance scene))))

(defgeneric scene-on-switch (scene)
  (:method ((scene scene))))

(defgeneric scene-make-initial-entities (scene)
  (:method ((scene scene)) nil))

(defmethod tick ((scene scene)))
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
                                                            (cons `(make-instance ,@(cons (list 'quote (car entity)) (cdr entity))))))))))
                                ((string= keyword :start)
                                 (scene-method 'start))
                                ((string= keyword :tick)
                                 (scene-method 'tick))
                                ((string= keyword :on-switch)
                                 (scene-method 'scene-on-switch))))))))
