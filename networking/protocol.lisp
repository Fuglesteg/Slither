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
;;;; 4 = entity: Entity data to be spawned or updated
;;;; 5 = input: Send inputs
;;;; 6 = owner: Send ownership to client
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

(uiop:define-package :slither/networking/protocol
  (:use :cl
        :ieee-floats
        :slither/utils
        :slither/input
        :slither/core
        :slither/serialization)
  (:import-from :serapeum
                :octet-vector
                :make-octet-vector)
  (:export :packet-parse-header
           :make-packet-header
           :make-subpacket
           :parse-subpacket
           :make-packet
           :parse-packet))

(in-package :slither/networking/protocol)

(defconstant +packet-max-size+ 1200)

(defun packet-parse-header (packet)
  (with-vector-reader packet (:read-integer read-packet-bytes)
    (let ((protocol (case (read-packet-bytes 1)
                      (0 :udp)
                      (1 :rudp)))
          (tick (read-packet-bytes 4))
          (packet-id (read-packet-bytes 4)))
      (let ((acknowledging-packet-id (read-packet-bytes 4))
            (last-acknowledged-packets (read-packet-bytes 4)))
        (values protocol
                tick
                packet-id
                acknowledging-packet-id
                last-acknowledged-packets)))))

(defun make-packet-header (&key (protocol :udp)
                                tick
                                packet-id
                                (acknowledging-packet-id 0)
                                (last-acknowledged-packets 0))
  (with-vector-writer (make-array 17 :element-type '(unsigned-byte 8)) (:write-integer write-packet-bytes)
    (write-packet-bytes (case protocol
                          (:udp 0)
                          (:rudp 1))
                        :bytes 1)
    (write-packet-bytes tick :bytes 4)
    (write-packet-bytes packet-id :bytes 4)
    (write-packet-bytes acknowledging-packet-id :bytes 4)
    (write-packet-bytes last-acknowledged-packets :bytes 4)))

(defun make-subpacket (packet-type &rest arguments)
  (case packet-type
    (:connect
     (destructuring-bind (username) arguments
       (with-vector-writer (make-octet-vector (+ 3 (length username))) (:write-integer packet-write-byte)
         (packet-write-byte 0 :bytes 1)
         (packet-write-byte (length username) :bytes 2)
         (loop for char across username
               do (packet-write-byte (char-code char) :bytes 1)))))
    (:disconnect
     (make-array 1
                 :element-type '(unsigned-byte 8)
                 :initial-contents #(1)))
    (:update
     (destructuring-bind (networked-object-id place-id new-value) arguments
       (let* ((encoded-argument (encode-argument new-value))
              (packet-length (+ (length encoded-argument) 2 2)))
         (with-vector-writer (make-octet-vector (+ packet-length 3)) (:write-integer packet-write-byte
                                                                      :write-sequence packet-write-sequence)
           (packet-write-byte 2 :bytes 1)
           (packet-write-byte packet-length :bytes 2)
           (packet-write-byte networked-object-id :bytes 2)
           (packet-write-byte place-id :bytes 2)
           (packet-write-sequence encoded-argument)))))
    (:action
     (destructuring-bind (networked-object-id action-id . arguments) arguments
       (let* ((encoded-arguments (encode-arguments arguments))
              (packet-length (+ (length encoded-arguments) 2 2)))
         (with-vector-writer (make-octet-vector (+ packet-length 3)) (:write-integer packet-write-byte
                                                                      :write-sequence packet-write-sequence)
           (packet-write-byte 3 :bytes 1)
           (packet-write-byte packet-length :bytes 2)
           (packet-write-byte networked-object-id :bytes 2)
           (packet-write-byte action-id :bytes 2)
           (packet-write-sequence encoded-arguments)))))
    (:entity
     (destructuring-bind (networked-object-id entity) arguments
       (let* ((encoded-entity (entity-encode entity))
              (packet-length (+ (length encoded-entity) 2 2))
              (entity-id (entity-type-id entity)))
         (with-vector-writer (make-octet-vector (+ packet-length 3)) (:write-integer packet-write-byte
                                                                      :write-sequence packet-write-sequence)
           (packet-write-byte 4 :bytes 1)
           (packet-write-byte packet-length :bytes 2)
           (packet-write-byte networked-object-id :bytes 2)
           (packet-write-byte entity-id :bytes 2)
           (packet-write-sequence encoded-entity)))))
    (:input
     (destructuring-bind (inputs) arguments
       (let ((encoded-inputs (encode-inputs inputs)))
         (with-vector-writer (make-octet-vector 3) (:write-integer packet-write-byte)
           (packet-write-byte 5 :bytes 1)
           (packet-write-byte encoded-inputs :bytes 2)))))
    (:owner
     (destructuring-bind (entity-ids) arguments
       (let ((packet-length (* (length entity-ids) 2)))
       (with-vector-writer
           (make-octet-vector (+ 3 packet-length))
           (:write-integer packet-write-byte)
         (packet-write-byte 6 :bytes 1)
         (packet-write-byte packet-length :bytes 2)
         (dolist (entity-id entity-ids)
           (packet-write-byte entity-id :bytes 2))))))))

(defun parse-subpacket (subpacket)
  (with-vector-reader subpacket (:read-integer packet-read-bytes
                                 :read-sequence packet-read-sequence)
    (case (packet-read-bytes 1)
      (0
       (let ((username-length (packet-read-bytes 2)))
         (values
          (list :connect
                (map 'string #'code-char (packet-read-sequence username-length)))
          (+ username-length 3))))
      (1
       (values (list :disconnect)
               1))
      (2
       (let* ((packet-length (packet-read-bytes 2))
              (networked-object-id (packet-read-bytes 2))
              (place-id (packet-read-bytes 2))
              (new-value (decode-argument (packet-read-sequence (- packet-length 4)))))
         (values (list
                  :update
                  networked-object-id
                  place-id
                  new-value)
                 (+ 3 packet-length))))
      (3
       (let* ((packet-length (packet-read-bytes 2))
              (networked-object-id (packet-read-bytes 2))
              (action-id (packet-read-bytes 2))
              (arguments (decode-arguments (packet-read-sequence (- packet-length 4)))))
         (values
          (list
           :action
           networked-object-id
           action-id
           arguments)
          (+ packet-length 3))))
      (4
       (let* ((packet-length (packet-read-bytes 2))
              (networked-object-id (packet-read-bytes 2))
              (entity-type-id (packet-read-bytes 2))
              (entity (entity-decode entity-type-id (packet-read-sequence (- packet-length 4)))))
         (declare (ignore networked-object-id))
         (values
          (list
           :entity
           entity-type-id
           entity)
          (+ packet-length 3))))
      (5
       (let* ((input-integer (packet-read-bytes 2))
              (inputs (decode-inputs input-integer)))
         (values (list
                  :input
                  inputs)
                 3)))
      (6
       (let* ((packet-length (packet-read-bytes 2))
              (entity-ids (loop repeat (the integer (/ packet-length 2))
                                collect (packet-read-bytes 2))))
         (values (list
                  :owner
                  entity-ids)
                 (+ packet-length 3)))))))

(defconstant +packet-max-size+ 1200)

(defun make-packet (&key header subpackets)
  (let ((subpackets-used 0))
    (values
     (apply #'concatenate
            'octet-vector
            header
            (loop for subpacket in subpackets
                  for i from 1
                  for total-length = (length subpacket) then (+ total-length (length subpacket))
                  until (< total-length +packet-max-size+)
                  finally (setf subpackets-used i)
                          (return (subseq subpackets 0 i))))
     subpackets-used)))

(defun parse-packet (packet)
  (multiple-value-bind (protocol
                        tick
                        packet-id
                        acknowledging-packet-id
                        last-acknowledged-packets)
      (packet-parse-header (subseq packet 0 17))
    (let ((subpackets-vector (subseq packet 17))
          (parsed-subpackets))
      (loop until (= 0 (length subpackets-vector))
            do (multiple-value-bind (subpacket length)
                   (parse-subpacket subpackets-vector)
                 (push subpacket parsed-subpackets)
                 (setf subpackets-vector
                       (subseq subpackets-vector length))))
      (values protocol
              tick
              packet-id
              acknowledging-packet-id
              last-acknowledged-packets
              parsed-subpackets))))
