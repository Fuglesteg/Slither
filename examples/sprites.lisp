(uiop:define-package #:slither/examples/sprites
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/sprites)

(define-texture player-icon #P"examples/player.png")

(defentity player ()
  (:behaviors
   transform
   (sprite :texture player-icon)
   move
   camera))

(defscene sprite-example ()
  (:entities player))

(defun start-example ()
  (setf (current-scene) (make-instance 'sprite-example))
  (start-game))
