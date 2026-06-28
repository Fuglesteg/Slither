(uiop:define-package :slither/networking/networked
  (:use :cl
        :slither/core
        :slither/utils
        :slither/scenes
        :slither/input
        :slither/networking/protocol
        :slither/networking/client-prediction
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
           :networked-connection
           :networked-rewind-to-tick
           :networked-simulated-inputs
           :networked-needs-simulation
           :on-add-networked
           :on-remove-networked
           :serverp
           :clientp))

(in-package :slither/networking/networked)

(defvar *networking-environment* nil)

(defun networking-environment ()
  *networking-environment*)

(defun (setf networking-environment) (new-value)
  (setf *networking-environment* new-value))

(defun networked-objects ()
  (scene-value (current-scene) 'networked))

(defun serverp ()
  (eq (networking-environment)
      :server))

(defun clientp ()
  (eq (networking-environment)
      :client))

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
  (unless (gethash (networked-id networked)
                   (networked-objects))
    (setf (gethash (networked-id networked)
                   (networked-objects))
          networked)))

(defmethod scene-make-networked ((scene scene))
  (setf (scene-value scene 'networked)
        (make-hash-table :test 'eq)))

(defvar *networked-object-id-count* 0)

(deftype networked-mode ()
  '(member :owned :client-predicted :static))

(defconstant +lag-compensation-history-size+ 60)

(defbehavior networked
    ((id :init (prog1 *networked-object-id-count*
                 (incf *networked-object-id-count*))
         :networked t)
     (mode :init (if (clientp)
                     :static
                     :owned))
     (connection :init (progn nil))
     ;; Server lag compensation
     (lag-compensated-slots)
     ;; Client prediction
     (last-tick-update :init 0)
     (needs-simulation :init (progn nil))
     (simulated-inputs :init (copy-inputs slither/input::*inputs*))
     (updated-places :init (progn nil)))
  (:networked t)
  (:create
   (when (networked-objects)
     (add-networked *behavior*)))
  (:start
   (on-add-networked *behavior*)
   (when (and (eq (networked-mode) :owned)
              (serverp))
     (setf (networked-lag-compensated-slots)
           (make-hash-table :test 'equalp))
     (loop for lag-compensated-slot in (entity-lag-compensated-slots-with-behaviors *entity*)
           do (let* ((initial-value
                       (etypecase lag-compensated-slot
                         (cons (destructuring-bind (behavior . slot) lag-compensated-slot
                                 (copy-value (slot-value (entity-find-behavior *entity* behavior)
                                                         slot))))
                         (symbol (copy-value (slot-value *entity* lag-compensated-slot)))))
                     (buffer (make-array +lag-compensation-history-size+
                                         :initial-element nil)))
                ;; Fill the buffer with initial state so
                ;; rewinding to ticks before the
                ;; entity existed returns its spawn state.
                (dotimes (i +lag-compensation-history-size+)
                  (setf (aref buffer i) (copy-value initial-value)))
                (setf (gethash lag-compensated-slot (networked-lag-compensated-slots))
                      buffer)))))
  (:pre-fixed-tick)
  (:post-fixed-tick
   (when (serverp)
     (do-hash-table (slot history-buffer (networked-lag-compensated-slots))
       (replace history-buffer history-buffer
                :start1 1)
       (setf (aref history-buffer 0)
             (etypecase slot
               (list (destructuring-bind (behavior . slot) slot
                       (copy-value (slot-value (entity-find-behavior *entity* behavior)
                                   slot))))
               (symbol (copy-value (slot-value *entity* slot))))))))
  (:destroy
   (when (networked-objects)
     (remove-networked *behavior*))))

(defun networked-rewind-to-tick (networked tick)
  (let ((index (min +lag-compensation-history-size+ (- (current-tick) tick))))
    (do-hash-table (slot history-buffer (networked-lag-compensated-slots networked))
      (etypecase slot
        (cons (destructuring-bind (behavior . slot) slot
                (setf (slot-value (entity-find-behavior (behavior-entity networked) behavior)
                                  slot)
                      (aref history-buffer index))))
        (symbol (setf (slot-value (behavior-entity networked) slot)
                      (aref history-buffer index))))
      (replace history-buffer history-buffer
               :start1 index))))

(defun networked-apply-update (networked place-id new-value)
  (setf (networked-needs-simulation networked) t)
  (let ((entity (behavior-entity networked)))
    (multiple-value-bind (slot-symbol behavior) (entity-find-networked-slot-symbol entity place-id)
      (if behavior
          (setf (slot-value (entity-find-behavior entity behavior) slot-symbol) new-value)
          (setf (slot-value entity slot-symbol) new-value)))))

(defun networked-register-place-change (networked place-symbol &optional behavior-symbol)
  (when (serverp)
    (pushnew (entity-find-networked-slot-id (behavior-entity networked) place-symbol behavior-symbol)
             (networked-updated-places networked))))

(defun networked-get-updated-places (networked)
  (prog1
      (loop for updated-place in (networked-updated-places networked)
            unless (null updated-place)
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
