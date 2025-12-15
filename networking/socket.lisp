(uiop:define-package :slither/networking/socket
  (:use :cl
        :slither/utils)
  (:export :socket-open
           :socket-listen
           :socket-close
           :socket-receive
           :socket-send))

(in-package :slither/networking/socket)

(defvar *socket*)

(defun socket-open ()
  (setf *socket*
        (make-instance 'sb-bsd-sockets:inet-socket
                        :type :datagram
                        :protocol :udp)))

(defun socket-listen (&optional (port 7777))
  (sb-bsd-sockets:socket-bind *socket* #(0 0 0 0) port))

(defun socket-close ()
  (sb-bsd-sockets:socket-close *socket*)
  (setf *socket* nil))

(defvar *receiving-data-buffer*
  (make-array 1500 ; 1500 is the usual page size limit of UDP
              :element-type '(unsigned-byte 8)))

(defun socket-receive ()
  (multiple-value-bind (data length remote-address remote-port)
      (sb-bsd-sockets:socket-receive *socket*
                                     *receiving-data-buffer*
                                     nil
                                     :element-type '(unsigned-byte 8))
    (values (subseq data 0 length)
            (list remote-address
                  remote-port))))

(defun socket-send (address data)
  (sb-bsd-sockets:socket-send *socket*
                              data
                              nil
                              :address address))
