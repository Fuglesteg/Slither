(uiop:define-package :slither/networking/networked
  (:use :cl
        :slither/core
        :slither/utils
        :slither/serialization
        :slither/scenes
        :slither/networking/protocol
        :slither/networking/connection)
  (:export :networked-objects
           :find-networked
           :scene-make-networked
           :networking-environment
           :networked
           :networked-id
           :networked-apply-update
           :networked-apply-action))

(in-package :slither/networking/networked)

(defvar *networking-environment* nil)

(defun networking-environment ()
  *networking-environment*)

(defun (setf networking-environment) (new-value)
  (setf *networking-environment* new-value))

(defun networked-objects ()
  (scene-value (current-scene) 'networked))

(defun find-networked (id)
  (gethash id (networked-objects)))

(defmethod scene-make-networked ((scene scene))
  (setf (scene-value scene 'networked)
        (make-hash-table :test 'eq)))

(defvar *networked-object-id-count* 0)

(defbehavior networked
    ((id :init (prog1 *networked-object-id-count*
                 (incf *networked-object-id-count*))
         :networked t)
     (actions :init (make-hash-table :test 'eq))
     (update-places :init (make-array 100
                                      :fill-pointer 0)))
  (:networked t)
  (:start
   (setf (gethash (networked-id) (networked-objects))
         *behavior*)
   (let ((entity *entity*))
     (flet ((slot-accessor (slot behavior)
              (if behavior
                  (lambda (new-value)
                    (setf (slot-value (entity-find-behavior entity behavior)
                                      slot)
                          new-value))
                  (lambda (new-value)
                           (setf (slot-value entity slot) new-value)))))
       (loop for (networked-slot slot-behavior) in (entity-networked-slots-with-behaviors entity)
             do (vector-push (slot-accessor networked-slot
                                            slot-behavior)
                             (networked-update-places))))))

  (:networked-find-action (networked action-id)
   (gethash action-id (networked-actions networked)))

  (:networked-apply-action (networked action-id action-arguments)
   (apply (networked-find-action networked action-id) action-arguments))

  #+nil(:networked-find-update-place (networked place-id)
   (aref (networked-update-places networked) place-id))

  #+nil(:networked-apply-update (networked place-id new-value)
   (funcall (networked-find-update-place networked place-id) new-value))

  #+nil(:networked-send-update (networked place-id new-value)
        (send-subpacket :update (networked-id networked) place-id new-value)))

(defun networked-apply-update (networked place-id new-value)
  (funcall (networked-find-update-place networked place-id) new-value))

(defun networked-find-update-place (networked place-id)
 (aref (networked-update-places networked) place-id))
