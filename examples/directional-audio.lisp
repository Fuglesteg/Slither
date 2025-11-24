(uiop:define-package #:slither/examples/directional-audio
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/directional-audio)

(defentity player
    ()
  (:behaviors
   transform
   listener
   camera
   rectangle
   move))

(defsound woosh #P"woosh.mp3")

(defentity boombox
    ()
  (:behaviors
   transform
   (speaker :sound woosh)
   rectangle)
  (:tick
   (cond ((key-pressed-p :p) (speaker-play *entity*)))))

(defscene directional-audio-example ()
  (:entities boombox player))

(defun start-example ()
  (setf (current-scene) (make-instance 'directional-audio-example))
  (start-game))
