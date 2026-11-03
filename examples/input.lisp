(defpackage #:slither/examples/input
  (:use :cl :slither))

(in-package #:slither/examples/input)

(define-vector-input aim
  :poll (world-mouse-position))

(define-vector-input move
  :up :w
  :left :a
  :down :s
  :right :d)

(definput fire
  :default-bindings (:left-click))

(define-texture player-texture #P"examples/player.png")

(defentity player
    ()
  (:behaviors
   render-transform
   transform
   (controllable :input-source :player1)
   (sprite :texture player-texture))
  (:tick
   (draw-quadratic-bezier (render-transform-position)
			  (vlerp (render-transform-position) (vector-value aim) 0.6)
			  (vector-value aim)
			  :thickness 0.1
			  :color (if (held-p fire)
				     (vec4 0.4 0.8 0.4 1.0)
				     (vec4 1.0))))
  (:fixed-tick
   (if (key-held-p :left-click)
       (entity-unbind-input-source *entity*)
       ; :player1 is the default input source
       (entity-bind-input-source *entity* (input-source :player1)))
   (nv+ (transform-position) (vector-value move))))

(defscene input-example ()
  (:entities player)
  (:tick
   (set-camera-position (vec2) :zoom 0.08)))

(defun start-example ()
  (setf (current-scene) (make-instance 'input-example))
  (start-game))
