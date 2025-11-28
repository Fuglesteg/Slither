;;;; PROTOCOL:
;;;;
;;;; ======HEADER======
;;;; First byte determines protocol:
;;;; 0 for unsynced normal UDP
;;;; 1 for Reliable UDP (RUDP)
;;;;
;;;; 4 bytes for tick number
;;;;
;;;; 4 bytes for packet id of this packet
;;;;
;;;; RUDP specific:
;;;;
;;;; 4 bytes for "aknowledging packet-id"
;;;; 4 bytes for last-aknowledged packets bitfield
;;;;
;;;; =======BODY=======
;;;;
;;;; Contains multiple subpackets up until the packet size limit 1200
;;;;
;;;; Packet types:
;;;; 0 = connect: Request server to open connection
;;;; 1 = disconnect: Request server to close connection
;;;; 2 = update: Request client/server to update a value
;;;; 3 = action: Request server to perform an action
;;;; 4 = spawn: Tell client to spawn an object
;;;; 5 = input: Send inputs
;;;;
;;;; 0: Connect:
;;;;   - String - Username
;;;;
;;;; 1: Disconnect:
;;;;
;;;; 2: Update:
;;;;  - networked-object-id (uint32)
;;;;  - place-id (uint32) - Id of update-place in networked-object
;;;;  - new-value (t)
;;;;
;;;; 3: Action:
;;;;  - networked-object-id (uint32)
;;;;  - action-id (uint32) - Id of action in networked-object
;;;;  - ...arguments (vector t) - Arguments to action
;;;;
;;;; 4: Spawn:
;;;;  - networked-object-id (uint32)
;;;;  - entity-type-id (uint32) - Id of entity type
;;;;  - ...arguments (vector t) - Arguments to spawn-entity
;;;;
;;;; 5: Input:
;;;;  - input bit flags
;;;;
;;;; ==TYPED ARGUMENTS==
;;;;
;;;; First byte determines type:
;;;; 0 = uint32
;;;; 1 = single-float
;;;; 2 = vec2
;;;; 3 = vec3
;;;; 4 = vec4
;;;; 5 = string (2 bytes for length, ...chars)

(uiop:define-package #:slither/networking
  (:use :cl
        :slither/core
        :slither/utils
        :slither/serialization
        :slither/scenes
        :org.shirakumo.fraf.math.vectors
        :ieee-floats))

(in-package :slither/networking)

(defvar *socket* nil)

(defvar *server-address*)

;;; Plan:
;;; - Behavior for networking
;;;   - Monitors changing values on entities/behaviors and generates packets for update requests
;;;   - Applies game-state updates from server
;;;   - Applies various techniques of client side prediction, set by user
;;; - Clients run limited simulation for client prediction
;;; - Clients apply game-state packets from server and applies them
;;; - Server runs full simulation, receives game-state update requests from clients, runs tick and sends game-state updates
;;; - Server checks client ownership of objects before applying requests (Authorization)

;;; TODO: Implement headless mode for server
;;; TODO: Implement hertz tickrate for server

(defvar *request-buffer* nil)
(defvar *update-buffer* nil)

(defun flush-client-networking ()
  (loop for request in *request-buffer*
        do (request-send request))
  (setf *request-buffer* nil)
  (loop for update in *update-buffer*
        do (update-apply *update-buffer*))
  (setf *update-buffer* nil))

(defun flush-server-networking ()
  (loop for request in *request-buffer*
        do (request-apply request))
  (setf *request-buffer* nil)
  (loop for update in *update-buffer*
        do (update-send update)))

(defun networked-objects ()
  (scene-value (current-scene) 'networked))

(defun find-networked (id)
  (gethash id (networked-objects)))

(defmethod scene-make-networked ((scene scene))
  (setf (scene-value scene 'networked)
        (make-hash-table :test 'eq)))

(defvar *connections*
  (make-hash-table :test 'equalp)) ; Must use equalp to compare the address vectors

(defclass user ()
  ((name
    :initform (error "Name is required")
    :initarg :name
    :accessor user-name
    :type string)))

(defclass connection ()
  ((address
    :initform (error "Address is required")
    :initarg :address
    :accessor connection-address)
   (user
    :initform (error "User is required")
    :initarg :user
    :accessor connection-user
    :type user)))

(defun connection-send-data (connection data)
  (with-accessors ((address connection-address)) connection
    (send-data data
               (aref address 0)
               (aref address 1))))

(defun parse-request-data-header (request)
  (let ((packet-type (case (aref request 0)
                       (0 :connect)
                       (1 :disconnect)
                       (2 :action)
                       (3 :update)))
        (packet-id (vector-read-integer request :bytes 4)))
    (values packet-type packet-id)))

(defclass request ()
  ((origin
    :initarg :origin
    :accessor request-origin
    :type vector)
   (data
    :initarg :data
    :accessor request-data
    :type vector)))

(defun request-data-body (request)
  (subseq (request-data request) 5))

(defun parse-connection-request (request)
  (let ((request-body (request-data-body request)))
    (map 'string #'code-char request-body)))

(defun encode-request (request-type request-id &key user-name
                                                    networked-object-id
                                                    action-id
                                                    update-place-id
                                                    arguments)
  (case request-type
    (:connect (concatenate '(vector (unsigned-byte 8))
                           (vector 0)
                           (integer->byte-array request-id)
                           (map '(vector (unsigned-byte 8))
                                #'char-code
                                user-name)))
    (:disconnect t)
    (:action (concatenate '(vector (unsigned-byte 8))
                          (vector 2)
                          (integer->byte-array request-id)
                          (integer->byte-array networked-object-id)
                          (integer->byte-array action-id)
                          (apply #'concatenate '(vector (unsigned-byte 8))
                                 (map 'list #'encode-argument arguments))))
    (:update (concatenate '(vector (unsigned-byte 8))
                          (vector 3)
                          (integer->byte-array request-id)
                          (integer->byte-array networked-object-id)
                          (integer->byte-array update-place-id)
                          (apply #'concatenate '(vector (unsigned-byte 8))
                                 (map 'list #'encode-argument arguments))))))

(defun apply-action-request (request)
  (let* ((request-body (request-data-body request))
         (networked-object-id (vector-read-integer request-body :bytes 4))
         (action-id (vector-read-integer (subseq request-body 4) :bytes 4)))
    (networked-apply-action (find-networked networked-object-id)
                            action-id
                            (decode-arguments (subseq request-body 8)))))

(defun apply-update-request (request)
  (let* ((request-body (request-data-body request))
         (networked-object-id (vector-read-integer request-body :bytes 4))
         (place-id (vector-read-integer (subseq request-body 4) :bytes 4)))
    (networked-apply-update (find-networked networked-object-id)
                            place-id
                            (decode-arguments (subseq request-body 8)))))

;; Received from client
(defun request-apply (request)
  (multiple-value-bind (packet-type packet-id) (parse-request-data-header (request-data request))
    (declare (ignore packet-id))
    (#+dev ecase
     #-dev case
     packet-type
     (:connect
      (let ((user-name (parse-connection-request request))
            (origin (request-origin request)))
        (setf (gethash origin *connections*)
              (make-instance 'connection
                             :address origin
                             :user (make-instance 'user :name user-name)))))
     (:disconnect
      (remhash (request-origin request) *connections*))
     (:action
      ;(when (gethash (request-origin request) *connections*) ; When connection exists
        (apply-action-request request)
      ;)
     )
     (:update
      (when (gethash (request-origin request) *connections*) ; When connection exists
        (apply-update-request request))))))

;; Sent from client
(defun request-send (request-data)
  (send-data-to-server request-data))

(deftype update ()
  '(vector (unsigned-byte 8)))

(declaim (ftype (function (update) (values keyword integer))
                parse-update-header))
(defun parse-update-header (update)
  (declare (type update update))
  (let ((update-type (case (vector-read-integer update)
                       (0 :spawn)
                       (1 :value)))
        (packet-id (vector-read-integer update :bytes 4)))
    (values update-type
            packet-id)))

;; Received from server
(declaim (ftype (function (update) (values)) update-apply))
(defun update-apply (update)
  (declare (type update update))
  (multiple-value-bind (packet-type packet-id) (parse-update-header update)
    (declare (ignore packet-id))
    (case packet-type
      (:spawn
       t
       #+nil(multiple-value-bind (entity-type-id fields) (parse-update-spawn-arguments update)
         (apply #'spawn-entity (find-entity-type-by-id entity-type-id)
                fields)))
      (:value
       (multiple-value-bind (networked-id place-id new-value) (decode-arguments update)
         (networked-apply-update (find-networked networked-id)
                                 place-id
                                 new-value))))))

(defun encode-update (update-type packet-id &key networked-object-id
                                                 update-place-id
                                                 new-value)
  (case update-type
    (:value (concatenate '(vector (unsigned-byte 8))
                    (vector 1)
                    (integer->byte-array packet-id)
                    (integer->byte-array networked-object-id)
                    (integer->byte-array update-place-id)
                    (integer->byte-array new-value)))))

;; Sent from server
(defun update-send (update)
  (loop for connection in *connections*
        do (connection-send-data connection update)))

(defvar *networked-object-id-count* 0)

(defbehavior networked
    ((id :init (prog1 *networked-object-id-count*
                  (incf *networked-object-id-count*)))
     (actions :init (make-hash-table :test 'eq))
     (update-places :init (make-hash-table :test 'eq)))
  (:start
   (setf (gethash (networked-id *behavior*) (networked-objects))
         *behavior*))
  (:networked-find-action (action-id)
   (gethash action-id (networked-actions *behavior*)))
  (:networked-apply-action (action-id action-arguments)
   (apply (networked-find-action *behavior* action-id) action-arguments))
  (:networked-find-update-place (place-id)
   (gethash place-id (networked-update-places *behavior*)))
  (:networked-apply-update (place-id new-value)
   (funcall (networked-find-update-place *behavior* place-id) new-value))
  (:networked-send-request-update (place-id new-value)
   (when (boundp *server-address*)
   (push (encode-request :update
                         0
                         :networked-object-id (networked-id *behavior*)
                         :update-place-id place-id
                         :arguments (list new-value))
         *request-buffer*)))
  (:networked-send-request-action (action-id &rest arguments)
   (when (boundp '*server-address*)
     (push (encode-request :action
                           0
                           :networked-object-id (networked-id *behavior*)
                           :action-id action-id
                           :arguments arguments)
           *request-buffer*)))
  (:networked-send-update-value (place-id new-value)
   (push (encode-update :value
                        0
                        :networked-object-id (networked-id *behavior*)
                        :update-place-id place-id
                        :new-value new-value)
         *update-buffer*)))

(defun start-server (&optional (port 7777))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :datagram
                               :protocol :udp)))
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (setf *socket* socket)))

(defun stop-server ()
  (when *socket*
    (sb-bsd-sockets:socket-close *socket*)
    (setf *socket* nil)))

(defun send-data-to-server (data)
  (send-data data (aref *server-address* 0) (aref *server-address* 1)))

(defun send-data (data &rest address)
  (sb-bsd-sockets:socket-send *socket*
                              data
                              nil
                              :address address))

(defvar *receiving-data-buffer*
  (make-array 1500 ; 1500 is the usual page size limit of UDP
              :element-type '(unsigned-byte 8)))

(defun receive-data ()
  (multiple-value-bind (data length remote-address remote-port)
      (sb-bsd-sockets:socket-receive *socket*
                                     *receiving-data-buffer*
                                     nil)
    (values (subseq data 0 length) remote-address remote-port)))

(defun init-server-connection (address)
  (setf *server-address* address)
  (setf *socket* (make-instance 'sb-bsd-sockets:inet-socket
                                :type :datagram
                                :protocol :udp)))

(defun run-server ()
  (unless *socket*
    (start-server))
  (unwind-protect
       (loop (multiple-value-bind (data remote-address remote-port)
                 (receive-data)
               (when (and data
                          (> (length data) 0))
                 (push (make-instance 'request
                                      :origin (vector remote-address remote-port)
                                      :data data)
                       *request-buffer*))))
    (stop-server)))

(defun init-server ()
  (sb-thread:make-thread
   #'run-server
   :name "server"))
