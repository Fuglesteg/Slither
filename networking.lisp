(uiop:define-package #:slither/networking
  (:use :cl))

(in-package :slither/networking)

(defvar *socket* nil)

(defun start-server ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :datagram
                               :protocol :udp)))
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) 8080)
    (setf *socket* socket)))

(defun stop-server ()
  (when *socket*
    (sb-bsd-sockets:socket-close *socket*)
    (setf *socket* nil)))

(defun send-data (data &rest address)
  (sb-bsd-sockets:socket-send *socket*
                              data
                              nil
                              :address address))

(defun receive-data ()
  (let ((buffer (make-array 1000
                            :element-type 'character
                            :fill-pointer t)))
    (multiple-value-bind (data length remote-address remote-port)
        (sb-bsd-sockets:socket-receive *socket* buffer nil)
      (declare (ignore length))
      (values data remote-address remote-port))))

(defun run-server ()
  (unless *socket*
    (start-server))
  (unwind-protect
       (loop (multiple-value-bind (data remote-address remote-port)
                 (receive-data)
               (when (and data
                          (> (length data) 0))
                 (send-data data remote-address remote-port))))
    (stop-server)))

(defun init-server ()
  (sb-thread:make-thread
   #'run-server
   :name "udp-server"))
