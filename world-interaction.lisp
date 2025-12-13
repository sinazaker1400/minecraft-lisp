;; File: world-interaction.lisp
(defpackage #:minecraft-3d
  (:use #:cl #:lispbuilder-sdl)
  (:export #:perform-raycast
           #:break-block
           #:place-block
           #:update-chunk-geometries
           #:start-game))
(in-package #:minecraft-3d)

(defun perform-raycast (player)
  "Performs a raycast from the player's viewpoint."
  ;; Use let* instead of let so that later bindings can reference earlier ones
  (let* ((start-x (game-player-x player))
         (start-y (game-player-y player))
         (start-z (game-player-z player))
         (rot-x (game-player-rot-x player))
         (rot-y (game-player-rot-y player))
         (cos-pitch (float (cos rot-x) 0.0))
         (dir-x (float (* cos-pitch (cos rot-y)) 0.0))
         (dir-y (float (sin rot-x) 0.0))
         (dir-z (float (* cos-pitch (sin rot-y)) 0.0)))
    (raycast-block start-x start-y start-z dir-x dir-y dir-z)))

(defun raycast-block (start-x start-y start-z dir-x dir-y dir-z)
  "Cast a ray and return information about the first block face hit."
  (let ((len (sqrt (+ (* dir-x dir-x) (* dir-y dir-y) (* dir-z dir-z)))))
    (if (plusp len)
        (let ((norm-dir-x (/ dir-x len))
              (norm-dir-y (/ dir-y len))
              (norm-dir-z (/ dir-z len)))
          (let ((map-x (floor start-x))
                (map-y (floor start-y))
                (map-z (floor start-z))
                (step-x (if (>= norm-dir-x 0) 1 -1))
                (step-y (if (>= norm-dir-y 0) 1 -1))
                (step-z (if (>= norm-dir-z 0) 1 -1))
                ;; Fixed delta calculations - now properly compute 1.0 / abs(direction)
                (delta-x (if (zerop norm-dir-x) most-positive-single-float (/ 1.0 (abs norm-dir-x))))
                (delta-y (if (zerop norm-dir-y) most-positive-single-float (/ 1.0 (abs norm-dir-y))))
                (delta-z (if (zerop norm-dir-z) most-positive-single-float (/ 1.0 (abs norm-dir-z))))
                ;; Fixed side-dist calculations - now use pre-computed deltas instead of recalculating
                (side-dist-x (if (>= norm-dir-x 0)
                                 (* (- (1+ map-x) start-x) delta-x)
                                 (* (- start-x map-x) delta-x)))
                (side-dist-y (if (>= norm-dir-y 0)
                                 (* (- (1+ map-y) start-y) delta-y)
                                 (* (- start-y map-y) delta-y)))
                (side-dist-z (if (>= norm-dir-z 0)
                                 (* (- (1+ map-z) start-z) delta-z)
                                 (* (- start-z map-z) delta-z)))
                (hit-side nil)
                (distance 0.0)
                (max-distance +max-ray-distance+))

            (loop
              (when (> distance max-distance)
                (return-from raycast-block (make-raycast-result)))

              (let ((closest-side (cond
                                    ((and (<= side-dist-x side-dist-y) (<= side-dist-x side-dist-z)) 'x)
                                    ((<= side-dist-y side-dist-z) 'y)
                                    (t 'z))))

                (case closest-side
                  (x (setf distance side-dist-x
                           side-dist-x (+ side-dist-x delta-x)
                           map-x (+ map-x step-x)
                           hit-side 'x))
                  (y (setf distance side-dist-y
                           side-dist-y (+ side-dist-y delta-y)
                           map-y (+ map-y step-y)
                           hit-side 'y))
                  (z (setf distance side-dist-z
                           side-dist-z (+ side-dist-z delta-z)
                           map-z (+ map-z step-z)
                           hit-side 'z)))

                (let ((block-type (get-block map-x map-y map-z)))
                  (when block-type
                    (let ((hit-x (+ start-x (* norm-dir-x distance)))
                          (hit-y (+ start-y (* norm-dir-y distance)))
                          (hit-z (+ start-z (* norm-dir-z distance)))
                          (hit-face (case hit-side
                                      (x (if (< norm-dir-x 0) :right :left))
                                      (y (if (< norm-dir-y 0) :top :bottom))
                                      (z (if (< norm-dir-z 0) :front :back)))))
                      (return-from raycast-block
                        (make-raycast-result
                         :hit-p t
                         :block-x map-x
                         :block-y map-y
                         :block-z map-z
                         :face hit-face
                         :hit-x hit-x
                         :hit-y hit-y
                         :hit-z hit-z)))))))))
        (make-raycast-result))))

(defun break-block (world-x world-y world-z)
  "Removes a block at the specified world coordinates."
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (setf (aref (chunk-blocks chunk) local-x local-y local-z) nil)
        (setf (chunk-needs-geometry-update chunk) t)
        (loop for dx from -1 to 1 do
          (loop for dy from -1 to 1 do
            (loop for dz from -1 to 1 do
              (unless (and (zerop dx) (zerop dy) (zerop dz))
                (let ((neighbor-chunk (get-chunk (+ chunk-x dx) (+ chunk-y dy) (+ chunk-z dz))))
                  (setf (chunk-needs-geometry-update neighbor-chunk) t))))))))))

(defun place-block (world-x world-y world-z block-type)
  "Places a block of the specified type at the given world coordinates."
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)
        (setf (chunk-needs-geometry-update chunk) t)
        (loop for dx from -1 to 1 do
          (loop for dy from -1 to 1 do
            (loop for dz from -1 to 1 do
              (unless (and (zerop dx) (zerop dy) (zerop dz))
                (let ((neighbor-chunk (get-chunk (+ chunk-x dx) (+ chunk-y dy) (+ chunk-z dz))))
                  (setf (chunk-needs-geometry-update neighbor-chunk) t))))))))))

(defun update-chunk-geometries ()
  "Recalculates geometry for chunks marked as needing an update."
  (maphash (lambda (key chunk)
             (declare (ignore key))
             (when (chunk-needs-geometry-update chunk)
               (calculate-chunk-geometry chunk)
               (setf (chunk-needs-geometry-update chunk) nil)))
           *world-chunks*))

(defun start-game ()
  "Initializes and starts the game."
  ;; Placeholder for game initialization logic
  (format t "Game started!"))
