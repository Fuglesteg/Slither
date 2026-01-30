(uiop:define-package #:slither/physics
  (:use #:cl
        #:slither/utils
        #:slither/behaviors
        #:slither/core
        #:slither/scenes
        #:org.shirakumo.fraf.math.vectors)
  (:export :circle-collider
           :circle-collision-p
           :rigidbody
           :rigidbody-velocity
           :rigidbody-drag
           :rigidbody-velocity+
           :point-collides-p
           :circle-overlaps-point-p
           :circle-collisions))

(in-package #:slither/physics)

(declaim (ftype (function (vec2 float vec2 float) boolean) circle-collision-p))
(defun circle-collision-p (circle1-position circle1-radius
                           circle2-position circle2-radius)
  (< (vlength (v- circle1-position circle2-position))
      (+ circle1-radius circle2-radius)))

(defun circle-overlaps-point-p (circle-position circle-radius point)
  (< (vlength (v- point circle-position))
     circle-radius))

(defun circle-collides-p (circle-position circle-radius)
  (loop for circle-collider in (behaviors-of-type 'circle-collider)
        do (when (circle-collision-p circle-position
                                     circle-radius
                                     (transform-position (behavior-entity circle-collider))
                                     (circle-radius circle-collider))
             (return circle-collider))))

(defun circle-collisions (circle-position circle-radius)
  (loop for circle-collider in (behaviors-of-type 'circle-collider)
        when (circle-collision-p circle-position
                                    circle-radius
                                    (transform-position (behavior-entity circle-collider))
                                    (circle-radius circle-collider))
        collect circle-collider))

(defun point-collides-p (point)
  (loop for circle-collider in (behaviors-of-type 'circle-collider)
        do (when (circle-overlaps-point-p (transform-position (behavior-entity circle-collider))
                                          (circle-radius circle-collider)
                                          point)
             (return circle-collider))))

(defbehavior circle-collider
    ()
  (:circle-radius (&optional (circle *behavior*))
     (vx (transform-size circle)))
  (:circle-collider-collision-p (circle)
   (with-behaviors ((() (circle1-collider circle-collider))
                    (((circle1-position position)) (circle1-transform transform))) circle
     (with-behaviors ((((circle2-position position)) (circle2-transform transform))) *entity*
       (circle-collision-p circle1-position (circle-radius circle1-collider)
                           circle2-position (circle-radius *behavior*))))))

(defbehavior rigidbody
    (colliders
     (mass :init 1.0)
     (bounciness :init 0.0)
     (drag :init 0.0)
     (velocity :init (vec2)
               :networked t))
  (:networked t)
  (:required-behaviors
   transform)
  (:start
   (when-let ((collider (entity-find-behavior *entity* 'circle-collider)))
     (setf (slot-value *behavior* 'colliders)
           (list collider))))
  (:tick
   ;; Drag
   (vdecf (rigidbody-velocity)
          (v* (rigidbody-velocity)
              (vabs (rigidbody-velocity))
              (- (rigidbody-drag))
              slither/window:*dt*))

   ;; Collisions
   (loop for this-collider in (rigidbody-colliders)
         do (loop for foreign-collider in
                     ;; Make sure the collision check doesn't run on a collider on current entity
                     (remove-if-not
                      (lambda (behavior)
                        (not (eql (behavior-entity behavior) *entity*)))
                      (behaviors-of-type 'circle-collider))
                  ;; TODO: Add etypecase for other colliders
                  do (let* ((offset (v- (transform-position (behavior-entity foreign-collider))
                                        (transform-position)))
                            (distance (vlength offset))
                            (total-size (+ (circle-radius this-collider)
                                           (circle-radius foreign-collider))))

                       ;; Collision detection
                       (when (< distance total-size)
                         (let ((normalized-offset (if (= (vlength offset) 0)
                                                      (vec2 0 1)
                                                      (v/ offset distance))))
                           ;;; Collision resolution
                           ;; Set position of object to edge
                           (setf (transform-position *entity*)
                                 (v- (transform-position (behavior-entity foreign-collider))
                                     (v* normalized-offset
                                         total-size)))

                           (let* ((this-mass (rigidbody-mass *behavior*))
                                  (foreign-rigidbody (entity-find-behavior (behavior-entity foreign-collider)
                                                                           'rigidbody))
                                  (foreign-mass (if foreign-rigidbody
                                                    (rigidbody-mass foreign-rigidbody)
                                                    0.0)))
                             (unless (= 0 (+ this-mass foreign-mass))
                             (let* ((this-inverse-mass (if (= this-mass 0.0)
                                                         0.0
                                                         (/ 1 this-mass)))
                                  (this-bounciness (rigidbody-bounciness *behavior*))
                                  (foreign-bounciness (if foreign-rigidbody
                                                          (rigidbody-bounciness foreign-rigidbody)
                                                          0.0))

                                  (foreign-inverse-mass (if (= foreign-mass 0.0)
                                                            0.0
                                                            (/ 1 foreign-mass)))
                                  (foreign-velocity (if foreign-rigidbody
                                                        (rigidbody-velocity foreign-rigidbody)
                                                        (vec2)))
                                  (static-friction 1.1)
                                  (dynamic-friction 2.0)
                                  (relative-velocity (v- foreign-velocity
                                                         (rigidbody-velocity *behavior*)))
                                  (velocity-along-normal (v. relative-velocity
                                                             normalized-offset))
                                  (restitution (min this-bounciness foreign-bounciness))
                                  (normal-impulse (/ (* (- (+ 1 restitution)) velocity-along-normal)
                                                     (+ this-inverse-mass
                                                        foreign-inverse-mass))))
                               ;; Don't resolve unless the colliders are moving towards each other
                               (unless (> velocity-along-normal 0)
                                 ;; Collision impulse resolution
                                 (setf (rigidbody-velocity *behavior*)
                                       (v- (rigidbody-velocity *behavior*)
                                           (v* normalized-offset
                                               normal-impulse
                                               this-inverse-mass)))
                                 (when foreign-rigidbody
                                   (setf (rigidbody-velocity foreign-rigidbody)
                                         (v+ (rigidbody-velocity foreign-rigidbody)
                                             (v* normalized-offset
                                                 normal-impulse
                                                 foreign-inverse-mass))))
                                 (let* ((relative-velocity (v- (if foreign-rigidbody
                                                                   (rigidbody-velocity foreign-rigidbody)
                                                                   (vec2))
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
                                   ;; Friction impulse resolution
                                   (setf (rigidbody-velocity *behavior*)
                                         (v- (rigidbody-velocity *behavior*)
                                             (v* friction-impulse this-inverse-mass)))
                                   (when foreign-rigidbody
                                     (setf (rigidbody-velocity foreign-rigidbody)
                                           (v+ (rigidbody-velocity foreign-rigidbody)
                                               (v* friction-impulse foreign-inverse-mass))))))))))))))

   ;; Update position
   (move (rigidbody-velocity *behavior*)))
  (:rigidbody-velocity+ (force)
   (setf (rigidbody-velocity *behavior*) (nv+ (rigidbody-velocity *behavior*) force))))
