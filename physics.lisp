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
           :rigidbody-position
           :rigidbody-mass
           :rigidbody-velocity
           :rigidbody-drag
           :rigidbody-velocity+
           :point-collides-p
           :circle-overlaps-point-p
           :circle-collisions
           :inverse-mass))

(in-package #:slither/physics)

(-> circle-collision-p (vec2 float vec2 float) boolean)
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
                                     (rigidbody-position (behavior-entity circle-collider))
                                     (circle-radius circle-collider))
             (return circle-collider))))

(defun circle-collisions (circle-position circle-radius)
  (loop for circle-collider in (behaviors-of-type 'circle-collider)
        when (circle-collision-p circle-position
                                    circle-radius
                                    (rigidbody-position (behavior-entity circle-collider))
                                    (circle-radius circle-collider))
        collect circle-collider))

(defun point-collides-p (point)
  (loop for circle-collider in (behaviors-of-type 'circle-collider)
        do (when (circle-overlaps-point-p (rigidbody-position (behavior-entity circle-collider))
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

(defun inverse-mass (mass)
  (if (= mass 0.0)
      0.0
      (/ 1 mass)))

(defbehavior rigidbody
    ((colliders :init (list))
     (mass :init 1.0)
     (bounciness :init 0.0)
     (drag :init 0.0)
     (velocity :init (vec2)
               :networked t)
     (position :init (vec2)
               :networked t
               :networked-overrides ((transform position)))
     (previous-position :init (vec2)
                        :networked t))
  (:networked t)
  (:required-behaviors
   transform)
  (:start
   (when-let ((collider (entity-find-behavior *entity* 'circle-collider)))
     (setf (rigidbody-colliders)
           (list collider)))
   (setf (rigidbody-position) (transform-position))
   (setf (rigidbody-previous-position) (transform-position)))
  (:tick
   (setf (transform-position)
         (vlerp (rigidbody-previous-position)
                (rigidbody-position)
                (interpolation-alpha))))
  (:fixed-tick
   (when (slither/networking:networked-simulate-p)
   (setf (rigidbody-previous-position)
         (rigidbody-position))
   ;; Drag
   (vdecf (rigidbody-velocity)
          (v* (rigidbody-velocity)
              (vabs (rigidbody-velocity))
              (- (rigidbody-drag))
              (delta-time)))

   ;; Collisions
   (loop for this-collider in (rigidbody-colliders)
         do (loop for foreign-collider in
                  ;; Make sure the collision check doesn't run on a collider on current entity
                     (remove-if-not
                      (lambda (behavior)
                        (not (eql (behavior-entity behavior) *entity*)))
                      (behaviors-of-type 'circle-collider))
                  ;; TODO: Add etypecase for other colliders
                  do (let* ((offset (v- (rigidbody-position (behavior-entity foreign-collider))
                                        (rigidbody-position)))
                            (distance (vlength offset))
                            (total-size (+ (circle-radius this-collider)
                                           (circle-radius foreign-collider))))

                       ;; Collision detection
                       (when (< distance total-size)
                         ;; Collision resolution
                         (let* ((penetration-depth (- total-size distance))
                                (normalized-offset (if (= (vlength offset) 0)
                                                       (vec2 0 1)
                                                       (v/ offset distance)))
                                (this-mass (rigidbody-mass))
                                (foreign-rigidbody (entity-find-behavior (behavior-entity foreign-collider)
                                                                         'rigidbody))
                                (foreign-mass (if foreign-rigidbody
                                                  (rigidbody-mass foreign-rigidbody)
                                                  0.0)))
                           (unless (= 0 (+ this-mass foreign-mass))
                             (let* ((this-inverse-mass (inverse-mass this-mass))
                                    (this-bounciness (rigidbody-bounciness))
                                    (foreign-bounciness (if foreign-rigidbody
                                                            (rigidbody-bounciness foreign-rigidbody)
                                                            0.0))
                                    (foreign-inverse-mass (inverse-mass foreign-mass))
                                    (foreign-velocity (if foreign-rigidbody
                                                          (rigidbody-velocity foreign-rigidbody)
                                                          (vec2)))
                                    (static-friction 0.1)
                                    (dynamic-friction 1.0)
                                    (relative-velocity (v- foreign-velocity
                                                           (rigidbody-velocity)))
                                    (velocity-along-normal (v. relative-velocity
                                                               normalized-offset))
                                    (restitution (min this-bounciness foreign-bounciness))
                                    (normal-impulse (/ (* (- (+ 1 restitution)) velocity-along-normal)
                                                       (+ this-inverse-mass
                                                          foreign-inverse-mass))))
                               ;; Positional correction
                               (let* ((percent 0.2)
                                      (slop 0.01)
                                      (correction
                                        (v* normalized-offset
                                            (* (/ (max (- penetration-depth slop) 0.0) (+ this-inverse-mass
                                                                                          foreign-inverse-mass))
                                               percent))))
                                 (nv- (rigidbody-position)
                                      (v* correction this-inverse-mass))
                                 (nv+ (rigidbody-position foreign-rigidbody)
                                      (v* correction foreign-inverse-mass)))
                               ;; Don't resolve unless the colliders are moving towards each other
                               (unless (> velocity-along-normal 0)
                                 ;; Collision impulse resolution
                                 (setf (rigidbody-velocity)
                                       (v- (rigidbody-velocity)
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
                                                               (rigidbody-velocity)))
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
                                                                (v* tangent (- (min (* normal-impulse dynamic-friction)
                                                                                    (abs tangent-magnitude))))))))
                                   ;; Friction impulse resolution
                                   (setf (rigidbody-velocity)
                                         (v- (rigidbody-velocity)
                                             (v* friction-impulse this-inverse-mass)))
                                   (when foreign-rigidbody
                                     (setf (rigidbody-velocity foreign-rigidbody)
                                           (v+ (rigidbody-velocity foreign-rigidbody)
                                               (v* friction-impulse foreign-inverse-mass)))))))))))))
   ;; Update position
   (setf (rigidbody-position)
         (v+ (rigidbody-position) (rigidbody-velocity)))))
  (:rigidbody-velocity+ (force)
   (setf (rigidbody-velocity) (nv+ (rigidbody-velocity) force))))
