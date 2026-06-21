(uiop:define-package #:slither/networking
  (:use :cl
        :slither/core
        :slither/utils
        :slither/scenes)
  (:use-reexport :slither/networking/server
                 :slither/networking/client
                 :slither/networking/networked)
  (:export :start-server
           :start-network-client
           :client-prediction-tick-rate-flush
           :ticks-to-predict))

(in-package :slither/networking)

(defun start-server ()
  (init-server)
  (setf (networking-environment) :server))

(defun start-network-client (address &optional username)
  (init-server-connection address username)
  (setf (networking-environment) :client))
