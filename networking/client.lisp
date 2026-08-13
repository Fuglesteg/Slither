(uiop:define-package :slither/networking/client
  (:use :cl
        :slither/utils
        :slither/core
        :slither/input
        :slither/scenes
        :slither/networking/protocol
        :slither/networking/client-prediction
        :slither/networking/networked
        :slither/networking/socket
        :slither/networking/connection)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:import-from :serapeum
                :octet-vector)
  (:export
   :flush-server-connection
   :init-server-connection
   :process-inbound-prediction
   :send-inputs
   :unprocessed-inputs
   :disconnect
   :on-disconnected-by-server))

(in-package :slither/networking/client)

(defclass server-connection (connection) ())

(defvar *server-connection* nil)

(defconstant +default-port+ 7878)

(defun host-address (host)
  (let* ((port-separator-position (position #\: host))
         (domain (if port-separator-position
                     (subseq host 0 port-separator-position)
                     host))
         (port (if port-separator-position
                   (parse-integer (subseq host port-separator-position))
                   +default-port+)))
         (multiple-value-bind (ipv4-entry ipv6-entry)
             (sb-bsd-sockets:get-host-by-name domain)
           (list
            (or (sb-bsd-sockets:host-ent-address ipv4-entry)
                (sb-bsd-sockets:host-ent-address ipv6-entry))
            port))))

(defun connect-to-server (host &optional (username "Solan Gundersen"))
  (reset-client-prediction)
  (socket-open)
  (setf *server-connection*
        (make-instance 'server-connection
                       :remote (host-address host)))
  (connection-add-subpacket *server-connection* (make-subpacket :connect username)))

(defvar *inbound-packet-buffer-lock* (sb-thread:make-mutex :name "inbound-packet-buffer"))
(defvar *inbound-packet-buffer* (make-array 1000
                                            :fill-pointer 0
                                            :initial-element (make-octet-vector 0)
                                            :element-type 'octet-vector))

(defun disconnect ()
  (setf (networking-environment) nil)
  (when *server-connection*
    (setf (connection-outbound-subpackets *server-connection*)
          (list (make-subpacket :disconnect)))
    (connection-flush *server-connection*)
    (socket-close)
    (setf (fill-pointer *inbound-packet-buffer*) 0)
    (setf *server-connection* nil)))

(defun run-server-connection (address &optional username)
  (declare (ignore address username))
  (ignore-errors
    (unwind-protect
         (loop
           (let ((packet (socket-receive)))
             (sb-thread:with-mutex (*inbound-packet-buffer-lock*)
               (vector-push packet
                            *inbound-packet-buffer*))))
      (socket-close)
      (setf (fill-pointer *inbound-packet-buffer*) 0)
      (setf *server-connection* nil))))

(defvar *server-connection-thread* nil)

(defun init-server-connection (address &optional username)
  (connect-to-server address username)
  (setf *server-connection-thread*
        (sb-thread:make-thread
         (lambda ()
               (run-server-connection address username))
         :name "slither-server-connection")))

(defvar *input-packets* nil)

(defun send-inputs (inputs)
  (connection-add-subpacket *server-connection*
                            (make-subpacket :input
                                            (current-tick)
                                            inputs)))

(defun send-timestamp ()
  (connection-add-subpacket *server-connection*
                            (make-subpacket :echo
                                            (glfw:time))))

(declaim (type (or null (function (connection))) *on-disconnected-by-server*))
(defvar *on-disconnected-by-server* nil)

(defun on-disconnected-by-server ()
  (disconnect)
  (when *on-disconnected-by-server*
    (funcall *on-disconnected-by-server* *server-connection*)))

(defun (setf on-disconnected-by-server) (new-value)
  (setf *on-disconnected-by-server* new-value))

(defun process-inbound-packets ()
  (when *server-connection*
    ; Timed out
    (when (< (+ 1028 (connection-outbound-packet-id *server-connection*))
             (connection-last-received-packet-id *server-connection*))
      #+micros(micros:watch (connection-outbound-packet-id *server-connection*))
      #+micros(micros:watch (connection-last-received-packet-id *server-connection*))
      (on-disconnected-by-server))
    (let ((inbound-packet-buffer
            (sb-thread:with-mutex (*inbound-packet-buffer-lock*)
              (prog1 (subseq *inbound-packet-buffer* 0 (length *inbound-packet-buffer*))
                (setf (fill-pointer *inbound-packet-buffer*) 0)))))
      (loop for packet across inbound-packet-buffer
            when packet
            do (multiple-value-bind (protocol
                                     tick
                                     packet-id
                                     acknowledging-packet-id
                                     last-acknowledged-packets
                                     subpackets) (parse-packet packet)
                 (declare (ignore protocol))
                 (connection-acknowledge-received *server-connection*
                                                  packet-id)
                 (connection-acknowledge-sent *server-connection*
                                              acknowledging-packet-id
                                              last-acknowledged-packets)
                 (when (< (server-tick) tick)
                   (setf (server-tick) tick))
                 (loop for (subpacket-type . subpacket) in subpackets
                       do (case subpacket-type
                            (:connect
                             (slither/input::input-history-reset-to-tick (current-tick)))
                            (:echo
                             (destructuring-bind (time) subpacket
                               (register-round-trip-time (- (glfw:time) time))))
                            (:entity
                             (destructuring-bind (entity-type-id entity) subpacket
                               (declare (ignore entity-type-id))
                               (add-entity entity)))))
                 (loop for (subpacket-type . subpacket) in subpackets
                       do (case subpacket-type
                            (:update
                             (destructuring-bind (networked-object-id place-id new-value) subpacket
                               (when-let ((networked-object (find-networked networked-object-id)))
                                 (when (<= (networked-last-tick-update networked-object) tick)
                                   (networked-apply-update networked-object
                                                           place-id
                                                           new-value)
                                   (setf (networked-last-tick-update networked-object) tick)))))
                            (:action)
                            (:owner
                             (destructuring-bind (entity-ids) subpacket
                               (loop for entity-id in entity-ids
                                     do (alexandria:if-let ((networked (find-networked entity-id)))
                                          (setf (networked-mode networked)
                                                :client-predicted)))))
                            (:destroy
                             (destructuring-bind (networked-object-id) subpacket
                               (when-let ((networked (find-networked networked-object-id)))
                                 (remove-entity (behavior-entity networked)))))
                            (:disconnect
                             (on-disconnected-by-server))))
                   (let ((networked-to-predict nil)
                         (earliest-tick nil))
                     (do-hash-table (networked-id networked (networked-objects))
                       (declare (ignore networked-id))
                       (when (and (eq (networked-mode networked)
                                      :client-predicted)
                                  (networked-needs-simulation networked))
                         (cond
                           ((not earliest-tick) (setf earliest-tick (networked-last-tick-update networked)))
                           ((< (networked-last-tick-update networked)
                               earliest-tick)
                            (setf earliest-tick (networked-last-tick-update networked))))
                         (push networked networked-to-predict)))
                     (when earliest-tick
                       (let ((ticks-to-resimulate (- (current-tick)
                                                     earliest-tick
                                                     1))) ; -1 because current tick is handled by normal fixed-tick update
                         (dotimes (tick-count (max 0 ticks-to-resimulate))
                           (let ((tick (+ earliest-tick tick-count)))
                             (dolist (networked networked-to-predict)
                               (when (>= tick (networked-last-tick-update networked))
                                 (slither/input::input-history-apply (networked-simulated-inputs networked) tick)
                                 (let ((slither/input::*inputs* (networked-simulated-inputs networked))
                                       (slither/core::*delta-time* (tick-delta)))
                                   (fixed-tick (behavior-entity networked))))
                               (setf (networked-needs-simulation networked) nil))))))))))))

(-> process-inbound-prediction () ())
(defun process-inbound-prediction ()
  (when *server-connection*
    (process-inbound-packets))
  (when *server-connection*
    (send-inputs slither/input::*inputs*)
    (send-timestamp)))

(-> flush-server-connection () ())
(defun flush-server-connection ()
  (when *server-connection*
    (connection-flush *server-connection*)
    (client-prediction-tick-rate-flush)))
