(uiop:define-package #:slither/examples/sprites
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/sprites)

(define-texture player-icon #P"examples/player.png")

(defentity player ()
  (:behaviors
   (make-instance 'sprite
                  :texture player-icon)
   (make-instance 'move)
   (make-instance 'camera)))

(defun start-example ()
  (add-entity (make-instance 'player))
  (start-game))