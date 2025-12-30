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

(defclass client-connection (connection)
  ((user
    :initform (error "User is required")
    :initarg :user
    :accessor client-connection-user
    :type user)
   (entities
    :initform nil
    :initarg :entities
    :accessor client-connection-entities)))

(defconstant +tick-buffer-size+ 10)

(defvar *client-inputs*
  (make-array +tick-buffer-size+
              :element-type 'hash-table
              :initial-contents (loop repeat +tick-buffer-size+
                                      collect (make-hash-table :test 'eq))))

(defun client-inputs-add (client tick inputs)
  (let ((index (- (current-tick) tick)))
    (when (< index +tick-buffer-size+)
      (setf (gethash client (aref *client-inputs* index))
            inputs))))

(defun client-inputs (client tick)
  (let ((index (- (current-tick) tick)))
    (when (< index +tick-buffer-size+)
      (gethash client (aref *client-inputs* index)))))

(defun client-inputs-rotate ()
  (clrhash (aref *client-inputs* (1- +tick-buffer-size+)))
  (macrolet ((rotate-vector-range ()
               `(rotatef
                 ,@(loop for i from 1 below +tick-buffer-size+
                       collect `(aref *client-inputs*
                                      (- +tick-buffer-size+ ,i))))))
    (rotate-vector-range)))

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
  (let ((subpacket (make-subpacket :spawn
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
  (init-listener)
  (let ((tick-delta (/ 1 60))
        (last-time (glfw:time))
        (tick 0))
  (loop for time = (glfw:time)
        when (<= (+ last-time tick-delta)
                 time)
        do #+micros (slither/window::read-repl)
           (incf tick)
           (setf last-time time)
           (let ((slither/core::*tick* tick)
                 (slither/window:*dt* (- time last-time)))
             (flush-server)
             (loop for entity in (scene-entities (current-scene))
                   do (let ((client-connection
                              (entity-client-connection entity)))
                        (if client-connection
                            (let ((slither/input::*inputs*
                                  (client-inputs client-connection
                                                (current-tick))))
                              (tick entity))
                            (tick entity))))
             (client-inputs-rotate)))))

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
                                                      collect (make-subpacket :spawn
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
                            (:spawn nil)
                            (:input (when connection
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
