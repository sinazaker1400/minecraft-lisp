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
             (v11 (seeded-random (+ grid-x 1) (+ grid-z 1) (+ *world-seed* i)))
             (v0 (perlin-lerp v00 v10 smooth-x))
             (v1 (perlin-lerp v01 v11 smooth-x))
             (value (perlin-lerp v0 v1 smooth-z)))
        (incf result (* value amplitude))
        (incf max-value amplitude)
        (setf amplitude (* amplitude 0.5))
        (setf frequency (* frequency 2.0))))
    (/ result max-value)))

(defun generate-chunk-terrain (chunk)
  "Generate terrain for a chunk using noise"
  (let ((blocks (chunk-blocks chunk))
        (chunk-x (chunk-x chunk))
        (chunk-z (chunk-z chunk)))
    (dotimes (lx *chunk-size*)
      (dotimes (lz *chunk-size*)
        (let* ((world-x (+ (* chunk-x *chunk-size*) lx))
               (world-z (+ (* chunk-z *chunk-size*) lz))
               (height-noise (perlin-noise (/ world-x 50.0)
                                           (/ world-z 50.0)
                                           1.0
                                           4))
               (height (+ 32 (* 32 height-noise)))
               (height-int (floor height)))
          
          (dotimes (y *chunk-height*)
            (let ((block-type 0))
              (cond
                ((< y (- height-int 5))
                 (setf block-type 3))
                ((< y height-int)
                 (setf block-type 2))
                ((= y height-int)
                 (setf block-type 1))
                ((and (< y 32) (> y (- height-int 10)))
                 (setf block-type 5)))
              
              (setf (aref blocks lx y lz) block-type))))))))

(defun ensure-chunk-generated (chunk)
  "Generate chunk if not already generated"
  (when (chunk-needs-update chunk)
    (generate-chunk-terrain chunk)
    (setf (chunk-needs-update chunk) nil)))

(defun preload-world (player radius)
  "Generate chunks around player"
  (let ((px (floor (game-player-x player) *chunk-size*))
        (pz (floor (game-player-z player) *chunk-size*)))
    (loop for dx from (- radius) to radius do
      (loop for dz from (- radius) to radius do
        (let ((chunk (get-chunk (+ px dx) 0 (+ pz dz))))
          (ensure-chunk-generated chunk))))))
