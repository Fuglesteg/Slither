(uiop:define-package #:slither/collisions
  (:use #:cl
        #:slither/utils
        #:slither/behaviors
        #:slither/entities
        #:org.shirakumo.fraf.math.vectors)
  (:export :circle-collider
           :rigidbody
           :rigidbody-velocity
           :rigidbody-drag
           :rigidbody-velocity+))

(in-package #:slither/collisions)

(declaim (ftype (function (vec2 float vec2 float) boolean) circle-collision-p))
(defun circle-collision-p (circle1-position circle1-radius
                           circle2-position circle2-radius)
  (<= (vlength (v- circle1-position circle2-position))
      (+ circle1-radius circle2-radius)))

(defbehavior circle-collider
    ()
  (:circle-radius ()
     (vx (transform-size *entity*)))
  (:circle-collider-collision-p (circle)
   (with-behaviors ((() (circle1-collider circle-collider))
                    (((circle1-position position)) (circle1-transform transform))) circle
     (with-behaviors ((((circle2-position position)) (circle2-transform transform))) *entity*
       (circle-collision-p circle1-position (circle-radius circle1-collider)
                           circle2-position (circle-radius *behavior*))))))

(defbehavior rigidbody
    (colliders
     (mass
      :accessor rigidbody-mass
      :initarg :mass
      :initform 1.0
      :type float)
     (bounciness
      :accessor rigidbody-bounciness
      :initarg bounciness
      :initform 0.0
      :type float)
     (drag
      :accessor rigidbody-drag
      :initarg :drag
      :initform 0.0
      :type float)
     (velocity
      :accessor rigidbody-velocity
      :initarg :velocity
      :initform (vec2)
      :type vec2))
  (:start
   (alexandria:when-let ((collider (entity-find-behavior *entity* 'circle-collider)))
     (setf (slot-value *behavior* 'colliders)
           (list collider))))
  (:tick
   ; Drag
   (vdecf (rigidbody-velocity *behavior*)
          (v* (rigidbody-velocity *behavior*)
              (vabs (rigidbody-velocity *behavior*))
              (- (rigidbody-drag *behavior*))
              slither:*dt*))


   ; Collisions
   (loop for this-collider in (slot-value *behavior* 'colliders)
         do (loop for foreign-collider in
                     ; Make sure the collision check doesn't run on a collider on current entity
                     (remove-if-not
                      (lambda (behavior)
                        (not (eql (behavior-entity behavior) *entity*)))
                      (behaviors-of-type 'circle-collider))
                  ; TODO: Add etypecase for other colliders
                  do (let* ((offset (v- (transform-position (behavior-entity foreign-collider))
                                        (transform-position *entity*)))
                            (distance (vlength offset))
                            (normalized-offset (if (= (vlength offset) 0)
                                                   (vec2 0 1)
                                                   (v/ offset distance)))
                            (total-size (+ (circle-radius this-collider)
                                           (circle-radius foreign-collider))))

                       ; Collision detection
                       (when (< distance total-size)
                         ;; Collision resolution
                         ; Set position of object to edge
                         (setf (transform-position *entity*)
                               (v- (transform-position (behavior-entity foreign-collider))
                                   (v* normalized-offset
                                       total-size)))

                         (let* ((this-mass (rigidbody-mass *behavior*))
                                (this-inverse-mass (if (= this-mass 0.0)
                                                       0.0
                                                       this-mass))
                                (this-bounciness (rigidbody-bounciness *behavior*))
                                (foreign-rigidbody (entity-find-behavior (behavior-entity foreign-collider)
                                                                         'rigidbody))
                                (foreign-bounciness (if foreign-rigidbody
                                                        (rigidbody-bounciness foreign-rigidbody)
                                                        0.0))
                                (foreign-mass (if foreign-rigidbody
                                                  (rigidbody-mass foreign-rigidbody)
                                                  0.0))
                                (foreign-inverse-mass (if (= foreign-mass 0.0)
                                                          0.0
                                                          foreign-mass))
                                (foreign-velocity (if foreign-rigidbody
                                                      (rigidbody-velocity foreign-rigidbody)
                                                      (vec2)))
                                (static-friction 1)
                                (dynamic-friction 1)
                                (relative-velocity (v- foreign-velocity
                                                       (rigidbody-velocity *behavior*)))
                                (velocity-along-normal (v. relative-velocity
                                                           normalized-offset))
                                (restitution (min this-bounciness foreign-bounciness))
                                (normal-impulse (/ (* (- (+ 1 restitution)) velocity-along-normal)
                                                   (+ this-inverse-mass
                                                      foreign-inverse-mass))))
                           ; Collision impulse resolution
                           (unless (> velocity-along-normal 0)
                             (setf (rigidbody-velocity *behavior*)
                                   (v- (rigidbody-velocity *behavior*)
                                       (v* normalized-offset normal-impulse this-inverse-mass)))
                             (when foreign-rigidbody
                               (setf (rigidbody-velocity foreign-rigidbody)
                                     (v+ (rigidbody-velocity foreign-rigidbody)
                                         (v* normalized-offset normal-impulse foreign-inverse-mass))))
                             (let* ((relative-velocity (v- (vec2 0 0) #+nil(rigidbody-velocity (behavior-entity foreign-collider))
                                                           (rigidbody-velocity *behavior*)))
                                    (tangent (safe-vscale
                                              (v- relative-velocity
                                                  (v* normalized-offset
                                                      (v. relative-velocity normalized-offset)))
                                              1))
                                    (tangent-magnitude (/ (- (v. relative-velocity tangent))
                                                          (+ this-inverse-mass
                                                             foreign-inverse-mass)))
                                    (mu (sqrt (+ (expt static-friction 2) (expt static-friction 2))))
                                    (friction-impulse (if (< (abs tangent-magnitude) (* normal-impulse mu))
                                                          (v* tangent tangent-magnitude)
                                                          (let ((dynamic-friction (sqrt (+ (expt dynamic-friction 2)
                                                                                           (expt dynamic-friction 2)))))
                                                            (v* tangent (- normal-impulse) dynamic-friction)))))
                               ; Friction impulse resolution
                               (setf (rigidbody-velocity *behavior*)
                                     (v- (rigidbody-velocity *behavior*)
                                         (v* friction-impulse this-inverse-mass)))
                               (when foreign-rigidbody
                                 (setf (rigidbody-velocity foreign-rigidbody)
                                       (v+ (rigidbody-velocity foreign-rigidbody)
                                           (v* friction-impulse foreign-inverse-mass)))))))))))

   ; Update position
   (move (rigidbody-velocity *behavior*)))
  (:rigidbody-velocity+ (force)
   (setf (rigidbody-velocity *behavior*) (nv+ (rigidbody-velocity *behavior*) force))))
