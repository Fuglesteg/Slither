(uiop:define-package :slither/examples/multiplayer
  (:use :cl
        :slither))

(in-package :slither/examples/multiplayer)

(defentity player
  ()
  (:behaviors
   transform
   networked
   move
   rectangle))

(defscene multiplayer-server ()
  (:entities player)
  (:start
   (start-server))
  (:tick
   (flush-server)))

(defscene multiplayer-client ()
  (:start
   (start-network-client (list #(127 0 0 1) 7777)))
  (:tick
   (flush-server-connection)))

(defun start-game-server ()
  (let ((scene (make-instance 'multiplayer-server)))
    (scene-make-networked scene)
    (setf (current-scene) scene))
  (start-game))

(defun start-game-client ()
  (let ((scene (make-instance 'multiplayer-client)))
    (scene-make-networked scene)
    (setf (current-scene) scene))
  (start-game))
