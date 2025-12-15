(uiop:define-package :slither/networking/server
  (:use :cl
        :slither/utils
        :slither/core
        :slither/scenes
        :slither/networking/protocol
        :slither/networking/networked
        :slither/networking/socket
        :slither/networking/connection)
  (:export
   :send-update
   :init-server
   :flush-server))

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
    :accessor connection-user
    :type user)))

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

(defun run-server ()
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

(defun init-server ()
  (sb-thread:make-thread
   #'run-server
   :name "server"))

(defun send-update (networked-object-id update-place-id new-value)
  (let ((subpacket (make-subpacket :update networked-object-id update-place-id new-value)))
    (loop for key being the hash-keys of *client-connections*
          using (hash-value connection)
          do (connection-add-subpacket connection subpacket))))

(defun find-connection (origin)
  (gethash origin *client-connections*))

(defun flush-server ()
  ;; Get updates from networked objects
  (loop for key being the hash-keys of (networked-objects)
        using (hash-value networked-object)
        do (loop for (networked-slot slot-behavior) in (entity-networked-slots-with-behaviors (behavior-entity networked-object))
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
             (declare (ignore protocol tick))
             (let ((connection (find-connection (inbound-packet-origin inbound-packet))))
               (loop for (subpacket-type . subpacket) in subpackets
                     do (ecase subpacket-type
                          (:connect (destructuring-bind (username) subpacket
                                      (when connection
                                        (error "Connection already exists"))
                                      (setf (gethash (inbound-packet-origin inbound-packet)
                                                     *client-connections*)
                                            (make-instance 'client-connection
                                                           :remote (inbound-packet-origin inbound-packet)
                                                           :user (make-instance 'user :name username)))
                                      (setf connection (find-connection (inbound-packet-origin inbound-packet)))
                                      ;; Send scene to new connection
                                      (setf (connection-outbound-subpackets connection)
                                            (loop for key being the hash-keys of (networked-objects)
                                                  using (hash-value networked-object)
                                                  collect (make-subpacket :spawn
                                                                          key
                                                                          (behavior-entity networked-object))))))
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
                          (:spawn #+nil(destructuring-bind (entity-type-id arguments) subpacket
                                         (add-entity (decode-entity entity-type-id arguments))))
                          (:input t)))
               (when connection
                 (connection-acknowledge-received connection packet-id)
                 (connection-acknowledge-sent connection acknowledging-packet-id last-acknowledged-packets)))))
  (setf (fill-pointer *inbound-packet-buffer*) 0))
