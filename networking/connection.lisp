(uiop:define-package :slither/networking/connection
  (:use :cl
        :slither/utils
        :slither/core
        :slither/networking/socket
        :slither/networking/protocol)
  (:export
   :connection
   :connection-add-subpacket
   :connection-acknowledge-received
   :connection-acknowledge-sent
   :connection-outbound-subpackets
   :connection-outbound-packet-id
   :connection-flush
   :connection-send-packet
   :connection-last-acknowledged-sent-packet-id))

(in-package :slither/networking/connection)

(defclass rudp-context ()
  ((acknowledged-packets
    :initarg :acknowledged-packets
    :accessor rudp-context-acknowledged-packets
    :initform 0)
   (last-acknowledged-sent-packet-id
    :accessor rudp-context-last-acknowledged-sent-packet-id
    :initform 0)
   (sent-packets
    :initform (make-hash-table :test 'eq)
    :accessor rudp-context-sent-packets)))

(defclass connection ()
  ((remote
    :initarg :remote
    :accessor connection-remote)
   (outbound-packet-id-counter
    :reader connection-outbound-packet-id
    :initform 0)
   (last-received-packet-id
    :accessor connection-last-received-packet-id
    :initform 0)
   (rudp-context
    :accessor connection-rudp-context
    :initform (make-instance 'rudp-context))
   (should-rudp
    :accessor connection-should-rudp
    :initform nil)
   (outbound-subpackets
    :accessor connection-outbound-subpackets
    :initform nil)))

(defmethod connection-last-acknowledged-sent-packet-id ((connection connection))
  (rudp-context-last-acknowledged-sent-packet-id (connection-rudp-context connection)))

(defmethod connection-new-packet-id ((connection connection))
  (incf (slot-value connection 'outbound-packet-id-counter)))

(defmethod connection-acknowledge-received ((connection connection)
                                            packet-id)
  (with-accessors ((rudp-context connection-rudp-context)) connection
    (let ((packet-id-offset
            (- packet-id (connection-last-received-packet-id connection))))
      (unless (= packet-id-offset 0)
        (if (< packet-id-offset 0)
            ;; Packet is in the past, update bitfield
            (let ((packet-position (coerce (abs packet-id-offset)
                                           '(unsigned-byte 32))))
              (setf (rudp-context-acknowledged-packets rudp-context)
                    (dpb 1
                         (byte 1 packet-position)
                         (rudp-context-acknowledged-packets rudp-context))))
            ;; Packet is in the future, shift bitfield
            (let ((acknowledged-packets-bitfield
                    (rudp-context-acknowledged-packets rudp-context)))
              (setf acknowledged-packets-bitfield
                    (ash acknowledged-packets-bitfield packet-id-offset))
              (setf acknowledged-packets-bitfield
                    (dpb 1 (byte 1 0) acknowledged-packets-bitfield))
              (setf (rudp-context-acknowledged-packets rudp-context)
                    acknowledged-packets-bitfield)
              (setf (connection-last-received-packet-id connection)
                    packet-id)))))))

(defmethod connection-acknowledge-sent ((connection connection)
                                        packet-id
                                        packets-bitfield)
  (rudp-context-acknowledge-sent (connection-rudp-context connection)
                                 packet-id
                                 packets-bitfield))

(defmethod rudp-context-acknowledge-sent ((rudp-context rudp-context)
                                          packet-id
                                          packets-bitfield)
  (setf (rudp-context-last-acknowledged-sent-packet-id rudp-context)
        packet-id)
  (let ((acknowledged-packet-ids (list packet-id)))
    (loop for i from 1 to 32
          when (= 1 (ldb (byte 1 i) packets-bitfield))
          do (push (- packet-id i)
                   acknowledged-packet-ids))
    (loop for packet-id in acknowledged-packet-ids
          do (remhash packet-id (rudp-context-sent-packets rudp-context)))))

(defmethod connection-flush ((connection connection))
  (let ((subpackets (connection-outbound-subpackets connection)))
    (loop while (> (length subpackets) 0)
          do (multiple-value-bind (packet subpackets-used)
                 (make-packet :header (connection-make-packet-header connection)
                              :subpackets subpackets)
               (connection-send-packet connection packet)
               (setf subpackets (subseq subpackets subpackets-used))))
    (setf (connection-outbound-subpackets connection) nil)
    (setf (connection-should-rudp connection) nil)))

(defmethod connection-send-packet ((connection connection) packet)
  (socket-send (connection-remote connection) packet))

(defmethod connection-add-subpacket ((connection connection) subpacket)
  (push subpacket (connection-outbound-subpackets connection)))

(defmethod connection-make-packet-header ((connection connection))
  (with-slots (rudp-context) connection
    (make-packet-header :protocol :udp
                        :tick (current-tick)
                        :packet-id (connection-new-packet-id connection)
                        :acknowledging-packet-id (connection-last-received-packet-id connection)
                        :last-acknowledged-packets (rudp-context-acknowledged-packets rudp-context))))
