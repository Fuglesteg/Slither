(uiop:define-package #:slither
  (:use #:cl)
  (:use-reexport #:org.shirakumo.fraf.math.vectors
                 #:org.shirakumo.fraf.math.matrices
                 #:slither/utils
                 #:slither/core
                 #:slither/render
                 #:slither/ui
                 #:slither/window
                 #:slither/audio
                 #:slither/input
                 #:slither/behaviors
                 #:slither/scenes
                 #:slither/physics
                 #:slither/networking)
  (:export #:start-game))

(in-package #:slither)

(defun start-game (&key window)
  (with-window window
    (renderer-init)
    (audio-init)
      (with-event-loop
        (input-poll)
        (update-scene)
        (renderer-flush))))
