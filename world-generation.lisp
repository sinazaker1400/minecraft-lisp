(in-package :minecraft-3d)

;; Initialize random state with seed
(defparameter *world-seed* 12345)
(defparameter *random-state* (make-random-state))

(defun initialize-randomness (seed)
  "Initialize the random state with a seed for reproducible generation"
  (setf *world-seed* seed)
  (setf *random-state* (make-random-state t))
  ;; Set the state using the seed
  (dotimes (_ seed)
    (random 65536 *random-state*)))

(defun seeded-random (seed)
  "Deterministic pseudo-random number based on seed"
  (let ((x (sxhash seed)))
    (/ (mod x 10000) 10000.0)))

(defun perlin-like-noise (x z scale)
  "Simple Perlin-like noise using sxhash for deterministic randomness"
  (let ((n1 (seeded-random (list (floor (/ x scale)) (floor (/ z scale)) *world-seed*)))
        (n2 (seeded-random (list (floor (/ (+ x 1) scale)) (floor (/ z scale)) *world-seed*)))
        (n3 (seeded-random (list (floor (/ x scale)) (floor (/ (+ z 1) scale)) *world-seed*)))
        (n4 (seeded-random (list (floor (/ (+ x 1) scale)) (floor (/ (+ z 1) scale)) *world-seed*)))
        (local-x (- (/ x scale) (floor (/ x scale))))
        (local-z (- (/ z scale) (floor (/ z scale)))))
    ;; Smooth interpolation
    (let ((u (* local-x local-x (- 3 (* 2 local-x))))
          (v (* local-z local-z (- 3 (* 2 local-z)))))
      (+ (* n1 (- 1 u) (- 1 v))
         (* n2 u (- 1 v))
         (* n3 (- 1 u) v)
         (* n4 u v)))))

(defun calculate-height (x z)
  "Calculate terrain height at world coordinates with improved noise"
  (let ((base-height 64)
        ;; Large hills (scale 0.05 = 400 blocks wide)
        (large-hills (* 15 (perlin-like-noise x z 0.05)))
        ;; Medium features (scale 0.2 =
