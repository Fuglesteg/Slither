(in-package :slither/core)

(defvar *systems* nil)

(defclass system () ())

(defgeneric system-tick (system)
  (:method ((system system))))

(defgeneric system-fixed-tick (system)
  (:method ((system system))))

(defgeneric system-start (system)
  (:method ((system system))))

(defmacro defsystem (name &body clauses)
  `(progn
     (defclass ,name (system) ())
     (pushnew (make-instance ',name)
              *systems*
              :key #'type-of)
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (clause-identifier . arguments) clause
            (cond
              ((string= clause-identifier :tick)
               `(defmethod system-tick ((,name ,name))
                 ,@arguments))
              ((string= clause-identifier :fixed-tick)
               `(defmethod system-fixed-tick ((,name ,name))
                 ,@arguments))
              ((string= clause-identifier :start)
               `(defmethod system-start ((,name ,name))
                 ,@arguments)))))
        clauses)))

(defun systems-tick ()
  (dolist (system *systems*)
    (system-tick system)))

(defun systems-fixed-tick ()
  (dolist (system *systems*)
    (system-fixed-tick system)))

(defun systems-start ()
  (dolist (system *systems*)
    (system-start system)))
