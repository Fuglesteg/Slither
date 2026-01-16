(uiop:define-package :slither/networking/server
  (:use :cl
        :slither/utils
        :slither/core
        :slither/scenes
        :slither/networking/protocol
        :slither/networking/networked
        :slither/networking/socket
        :slither/networking/connection)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export
   :send-update
   :init-server
   :flush-server
   :send-entity
   :on-new-connection
   :user
   :user-name
   :run-server))

(in-package :slither/networking/server)

(defvar *client-connections*
  (make-hash-table :test 'equalp)) ; Must use equalp to compare the address vectors

(defun find-client-connection (remote)
  (gethash remote *client-connections*))

(defclass user ()
  ((name
    :initform (error "Name is required")
    :initarg :name
    :accessor user-name
    :type string)))

(defconstant +tick-buffer-size+ 10)

(defclass client-connection (connection)
  ((user
    :initform (error "User is required")
    :initarg :user
    :accessor client-connection-user
    :type user)
   (entities
    :initform nil
    :initarg :entities
    :reader client-connection-entities)
   (inputs
    :initform (make-array +tick-buffer-size+
                          :fill-pointer 0
                          :initial-element nil)
    :accessor client-connection-inputs)))

(defun (setf client-connection-entities) (new-entities client-connection)
  (setf (slot-value client-connection 'entities)
        new-entities)
  (connection-add-subpacket
   client-connection
   (make-subpacket :owner
                   (mapcar
                    (lambda (entity)
                      (networked-id (entity-find-behavior entity 'networked)))
                    new-entities))))

#+nil(defvar *client-inputs*
  (make-hash-table +tick-buffer-size+
              :element-type 'hash-table
              :initial-contents (loop repeat +tick-buffer-size+
                                      collect (make-array 32
                                                          :fill-pointer 0))))

(defun client-inputs-add (client inputs)
  (vector-push inputs (client-connection-inputs client)))

(defun client-inputs-reset ()
  (serapeum:do-hash-table (remote client *client-connections*)
    (declare (ignore remote))
    (setf (fill-pointer (client-connection-inputs client))
          0)))

#+server-reconciliation
(defvar *client-inputs*
  (make-array +tick-buffer-size+
              :element-type 'hash-table
              :initial-contents (loop repeat +tick-buffer-size+
                                      collect (make-hash-table :test 'eq))))

#+server-reconciliation
(defun client-inputs-add (client tick inputs)
  (let ((index (- (current-tick) tick)))
    (when (< index +tick-buffer-size+)
      (setf (gethash client (aref *client-inputs* index))
            inputs))))

#+server-reconciliation
(defun client-inputs (client tick)
  (let ((index (- (current-tick) tick)))
    (when (< index +tick-buffer-size+)
      (gethash client (aref *client-inputs* index)))))

#+server-reconciliation
(defun client-inputs-shift ()
  (setf (aref *client-inputs* 0)
        (clrhash (aref *client-inputs* (1- +tick-buffer-size+))))
  (replace *client-inputs* *client-inputs*
           :start1 1))

(defclass inbound-packet ()
  ((origin
    :initarg :origin
    :accessor inbound-packet-origin
    :type list)
   (data
    :initarg :data
    :accessor inbound-packet-data
    :type vector)))

(defparameter *inbound-packet-buffer* (make-array 1000
                                                  :fill-pointer 0
                                                  :initial-element (make-instance 'inbound-packet)
                                                  :element-type 'inbound-packet))

(defun run-listener ()
  (socket-open)
  (socket-listen)
  (unwind-protect
       (loop (multiple-value-bind (data remote)
                 (socket-receive)
               (when (and data
                          (> (length data) 0))
                 (vector-push (make-instance 'inbound-packet
                                             :origin remote
                                             :data data)
                              *inbound-packet-buffer*))))
    (socket-close)
    (setf *client-connections* nil)))

(defun init-listener ()
  (sb-thread:make-thread
   #'run-server
   :name "packet-listener"))

(defvar *on-new-connection* nil)

(defun on-new-connection (connection)
  (funcall *on-new-connection* connection))

(defun (setf on-new-connection) (new-value)
  (setf *on-new-connection* new-value))

(defun client-connections-add-subpacket (subpacket)
  (loop for key being the hash-keys of *client-connections*
        using (hash-value connection)
        do (connection-add-subpacket connection subpacket)))

(defun send-update (networked-object-id update-place-id new-value)
  (let ((subpacket (make-subpacket :update networked-object-id update-place-id new-value)))
    (client-connections-add-subpacket subpacket)))

(defun send-entity (entity)
  (let ((subpacket (make-subpacket :entity
                                   (networked-id (entity-find-behavior entity 'networked))
                                   entity)))
    (client-connections-add-subpacket subpacket)))

(defun find-connection (origin)
  (gethash origin *client-connections*))

(defun entity-client-connection (entity)
  (loop for key being the hash-keys of *client-connections*
        using (hash-value client-connection)
        do (loop for client-entity in (client-connection-entities client-connection)
                 when (eq client-entity
                          entity)
                 do (return-from entity-client-connection client-connection))))

(defun run-server ()
  (glfw:init)
  (init-listener)
  (let ((tick-delta (/ 1.0 60.0))
        (accumulator 0.0)
        (last-time (glfw:time))
        (tick 0))
    (loop
      (let* ((current-time (glfw:time))
             (frame-time (- current-time last-time)))
        (setf last-time current-time)
        (incf accumulator frame-time)
        ;; Process all accumulated ticks
        (loop while (>= accumulator tick-delta)
              do (incf tick)
                 (decf accumulator tick-delta)
                 (let ((slither/core::*tick* tick)
                       (slither/window:*dt* tick-delta))
                   #+micros (slither/window::read-repl)
                   (flush-server)
                   (loop for entity in (scene-entities (current-scene))
                         do (let ((client-connection
                                    (entity-client-connection entity)))
                              (if (and client-connection
                                       (client-connection-inputs client-connection)
                                       (< 0 (length (client-connection-inputs client-connection))))
                                  (loop for input across (client-connection-inputs client-connection)
                                        do (let ((slither/input::*inputs* input))
                                             (tick entity)))
                                  (tick entity))))
                   #+server-reconciliation (client-inputs-shift)
                   (client-inputs-reset)))))))

(defun init-server ()
  (sb-thread:make-thread #'run-server
                         :name "slither-server"))

(defun flush-server ()
  ;; Get updates from networked objects
  (loop for key being the hash-keys of (networked-objects)
        using (hash-value networked-object)
        do (loop for (networked-slot slot-behavior) in (entity-networked-slots-with-behaviors
                                                        (behavior-entity networked-object))
                 do (send-update key
                                 (entity-find-networked-slot-id (behavior-entity networked-object)
                                                                networked-slot
                                                                slot-behavior)
                                 (if slot-behavior
                                     (slot-value (entity-find-behavior (behavior-entity networked-object)
                                                                       slot-behavior)
                                                 networked-slot)
                                     (slot-value (behavior-entity networked-object)
                                                 networked-slot)))))
  ;; Send outbound packets to clients
  (loop for key being the hash-keys of *client-connections*
        using (hash-value client-connection)
        do (connection-flush client-connection))
  ;; Process inbound packets
  (loop for inbound-packet across *inbound-packet-buffer*
        do (multiple-value-bind (protocol
                                 tick
                                 packet-id
                                 acknowledging-packet-id
                                 last-acknowledged-packets
                                 subpackets) (parse-packet (inbound-packet-data inbound-packet))
             (declare (ignore protocol)
                      (ignorable tick))
             (let ((connection (find-connection (inbound-packet-origin inbound-packet))))
               (loop for (subpacket-type . subpacket) in subpackets
                     do (restart-case
                          (ecase subpacket-type
                            (:connect (destructuring-bind (username) subpacket
                                        (when connection
                                          (error "Connection already exists"))
                                        (let ((user (make-instance 'user :name username)))
                                          (setf (gethash (inbound-packet-origin inbound-packet)
                                                         *client-connections*)
                                                (make-instance 'client-connection
                                                               :remote (inbound-packet-origin inbound-packet)
                                                               :user user))
                                          (setf connection (find-connection (inbound-packet-origin inbound-packet)))
                                          ;; Send scene to new connection
                                          (setf (connection-outbound-subpackets connection)
                                                (loop for key being the hash-keys of (networked-objects)
                                                      using (hash-value networked-object)
                                                      collect (make-subpacket :entity
                                                                              key
                                                                              (behavior-entity networked-object))))
                                          (when *on-new-connection*
                                            (on-new-connection connection)))))
                            (:disconnect (remhash (inbound-packet-origin inbound-packet) *client-connections*))
                            (:update t #+nil(destructuring-bind (networked-object-id place-id new-value) subpacket
                                              (networked-apply-update (find-networked networked-object-id)
                                                                      place-id
                                                                      new-value)))
                            (:action #+nil(destructuring-bind (networked-object-id action-id arguments) subpacket
                                            (apply #'networked-apply-action
                                                   (find-networked networked-object-id)
                                                   action-id
                                                   arguments)))
                            (:entity nil)
                            (:input (when connection
                                      (destructuring-bind (inputs) subpacket
                                        (when inputs
                                          (client-inputs-add connection
                                                             (loop for input in inputs
                                                                   collect (cons input :held)))))
                                      #+server-reconciliation
                                      (client-inputs-add connection
                                                         (current-tick)
                                                         (destructuring-bind (inputs) subpacket
                                                           (loop for input in inputs
                                                                 collect (cons input :held)))))))
                          (skip-packet ()
                            :report "Skip the current packet"
                            t)))
               (when connection
                 (connection-acknowledge-received connection packet-id)
                 (connection-acknowledge-sent connection acknowledging-packet-id last-acknowledged-packets)))))
  (setf (fill-pointer *inbound-packet-buffer*) 0))
