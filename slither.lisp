(uiop:define-package #:slither
  (:use #:cl)
  (:use-reexport #:org.shirakumo.fraf.math.vectors
                 #:org.shirakumo.fraf.math.matrices
                 #:slither/utils
                 #:slither/core
                 #:slither/render
                 #:slither/window
                 #:slither/audio
                 #:slither/input
                 #:slither/behaviors
                 #:slither/scenes
                 #:slither/physics
                 #:slither/networking)
  (:import-from #:slither/render/uniform
                #:uniform-value
                #:uniform-location)
  (:import-from #:slither/render/shader-program
                #:shader-program
                #:make-shader-program
                #:set-uniform-value
                #:get-uniform)
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
