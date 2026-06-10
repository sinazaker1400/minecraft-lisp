(in-package :minecraft-3d)

(defun seeded-random (x y seed)
  "Deterministic random number from coordinates and seed"
  (let ((n (sxhash (list x y seed))))
    (/ (mod (abs n) 10000) 10000.0)))

(defun perlin-lerp (a b t-val)
  "Linear interpolation"
  (+ a (* (- b a) t-val)))

(defun perlin-smooth (t-val)
  "Smoothstep interpolation"
  (* t-val t-val (- 3 (* 2 t-val))))

(defun perlin-noise (x z scale octaves)
  "Multi-octave Perlin-like noise"
  (let ((result 0.0)
        (amplitude 1.0)
        (frequency 1.0)
        (max-value 0.0))
    (dotimes (i octaves)
      (let* ((sample-x (* x frequency scale))
             (sample-z (* z frequency scale))
             (grid-x (floor sample-x))
             (grid-z (floor sample-z))
             (local-x (- sample-x grid-x))
             (local-z (- sample-z grid-z))
             (smooth-x (perlin-smooth local-x))
             (smooth-z (perlin-smooth local-z))
             (v00 (seeded-random grid-x grid-z (+ *world-seed* i)))
             (v10 (seeded-random (+ grid-x 1) grid-z (+ *world-seed* i)))
             (v01 (seeded-random grid-x (+ grid-z 1) (+ *world-seed* i)))
             (v
