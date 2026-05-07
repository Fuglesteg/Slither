(uiop:define-package :slither/networking/networked
  (:use :cl
        :slither/core
        :slither/utils
        :slither/serialization
        :slither/scenes
        :slither/input
        :slither/networking/protocol
        :slither/networking/connection)
  (:export :networked-objects
           :find-networked
           :scene-make-networked
           :networking-environment
           :networked
           :networked-id
           :networked-apply-update
           :networked-apply-action
           :networked-apply-input
           :networked-simulate-p
           :networked-static-p
           :networked-client-predicted-p
           :networked-owned-p
           :networked-mode
           :networked-last-tick-update
           :networked-get-updated-places
           :on-add-networked
           :on-remove-networked))

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

(defvar *on-remove-networked* (lambda (networked) (declare (ignore networked))))

(defun on-remove-networked (networked)
  (funcall *on-remove-networked* networked))

(defun (setf on-remove-networked) (new-value)
  (setf *on-remove-networked* new-value))

(defun remove-networked (networked)
  (on-remove-networked networked)
  (remhash (networked-id networked) (networked-objects)))

(defvar *on-add-networked* (lambda (networked) (declare (ignore networked))))

(defun on-add-networked (networked)
  (funcall *on-add-networked* networked))

(defun (setf on-add-networked) (new-value)
  (setf *on-add-networked* new-value))

(defun add-networked (networked)
  (on-add-networked networked)
  (setf (gethash (networked-id networked)
                 (networked-objects))
        networked))

(defmethod scene-make-networked ((scene scene))
  (setf (scene-value scene 'networked)
        (make-hash-table :test 'eq)))

(defvar *networked-object-id-count* 0)

(deftype networked-mode ()
  '(member :owned :client-predicted :static))

(defbehavior networked
    ((id :init (prog1 *networked-object-id-count*
                 (incf *networked-object-id-count*))
         :networked t)
     (mode :init (if (eq (networking-environment) :client)
                     :static
                     :owned))
     (last-tick-update :init 0)
     (updated-places :init '()))
  (:networked t)
  (:start
   (when (networked-objects)
     (add-networked *behavior*)))
  ;; TODO: Should this happen pre tick?
  (:tick
   (case (networked-mode)
     (:client-predicted
      (let ((ticks-to-predict (- (current-tick) (networked-last-tick-update))))
        (when (< 0 ticks-to-predict)
          (copy-inputs slither/input::*inputs*)
          (setf (networked-last-tick-update) (current-tick)))))
      (:static)
      (:owned)))
  (:destroy
   (when (networked-objects)
     (remove-networked *behavior*))))

(defun networked-apply-update (networked place-id new-value)
  (let ((entity (behavior-entity networked)))
    (multiple-value-bind (slot-symbol behavior) (entity-find-networked-slot-symbol entity place-id)
      (if behavior
          (setf (slot-value (entity-find-behavior entity behavior) slot-symbol) new-value)
          (setf (slot-value entity slot-symbol) new-value)))))

(defun networked-register-place-change (networked place-symbol &optional behavior-symbol)
  (when (eq (networking-environment) :server)
    (pushnew (entity-find-networked-slot-id (behavior-entity networked) place-symbol behavior-symbol)
             (networked-updated-places networked))))

(defun networked-get-updated-places (networked)
  (prog1
      (loop for updated-place in (networked-updated-places networked)
            collect (multiple-value-bind (place-symbol behavior-symbol)
                        (entity-find-networked-slot-symbol (behavior-entity networked) updated-place)
                      (cons place-symbol behavior-symbol)))
    (setf (networked-updated-places networked) nil)))

(defun networked-owned-p (networked)
  (eq (networked-mode networked)
      :owned))

(defun networked-client-predicted-p (networked)
  (eq (networked-mode networked)
      :client-predicted))

(defun networked-static-p (networked)
  (eq (networked-mode networked)
      :static))

(defun networked-simulate-p ()
  (if (not (networking-environment))
      t
      (let ((networked (entity-find-behavior *entity* 'networked)))
        (if networked
            (member (networked-mode networked)
                    (list :owned :client-predicted))
            t))))
