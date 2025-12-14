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
    ;; Added debug output to show raycast start position
    (format t "[v0] Raycast from (~,2f, ~,2f, ~,2f) dir: (~,2f, ~,2f, ~,2f)~%" 
            start-x start-y start-z dir-x dir-y dir-z)
    (raycast-block start-x start-y start-z dir-x dir-y dir-z)))

(defun raycast-block (start-x start-y start-z dir-x dir-y dir-z)
  "Cast a ray and return information about the first block face hit."
  (let ((len (sqrt (+ (* dir-x dir-x) (* dir-y dir-y) (* dir-z dir-z)))))
    (if (plusp len)
        ;; Use let* with all DDA variables at the same scope level to avoid nesting issues
        (let* ((norm-dir-x (/ dir-x len))
               (norm-dir-y (/ dir-y len))
               (norm-dir-z (/ dir-z len))
               (map-x (floor start-x))
               (map-y (floor start-y))
               (map-z (floor start-z))
               (step-x (if (>= norm-dir-x 0) 1 -1))
               (step-y (if (>= norm-dir-y 0) 1 -1))
               (step-z (if (>= norm-dir-z 0) 1 -1))
               (delta-x (if (zerop norm-dir-x) most-positive-single-float (/ 1.0 (abs norm-dir-x))))
               (delta-y (if (zerop norm-dir-y) most-positive-single-float (/ 1.0 (abs norm-dir-y))))
               (delta-z (if (zerop norm-dir-z) most-positive-single-float (/ 1.0 (abs norm-dir-z))))
               (side-dist-x (if (>= norm-dir-x 0)
                                (* (- (1+ map-x) start-x) delta-x)
                                (* (- start-x map-x) delta-x)))
               (side-dist-y (if (>= norm-dir-y 0)
                                (* (- (1+ map-y) start-y) delta-y)
                                (* (- start-y map-y) delta-y)))
               (side-dist-z (if (>= norm-dir-z 0)
                                (* (- (1+ map-z) start-z) delta-z)
                                (* (- start-z map-z) delta-z)))
               (max-distance +max-ray-distance+))
          
          ;; Main DDA loop - use simple do* instead of loop for better control
          ;; Refactored to use do* with explicit variable updates
          (do* ((current-map-x map-x)
                (current-map-y map-y)
                (current-map-z map-z)
                (current-side-dist-x side-dist-x)
                (current-side-dist-y side-dist-y)
                (current-side-dist-z side-dist-z)
                (distance 0.0)
                (hit-side nil))
               ((> distance max-distance) (make-raycast-result))
            
            ;; Find the closest side
            (let ((closest-side (cond
                                  ((and (<= current-side-dist-x current-side-dist-y) 
                                        (<= current-side-dist-x current-side-dist-z)) 'x)
                                  ((<= current-side-dist-y current-side-dist-z) 'y)
                                  (t 'z))))
              
              ;; Step along the ray to the next grid crossing
              (case closest-side
                (x (setf distance current-side-dist-x
                         current-side-dist-x (+ current-side-dist-x delta-x)
                         current-map-x (+ current-map-x step-x)
                         hit-side 'x))
                (y (setf distance current-side-dist-y
                         current-side-dist-y (+ current-side-dist-y delta-y)
                         current-map-y (+ current-map-y step-y)
                         hit-side 'y))
                (z (setf distance current-side-dist-z
                         current-side-dist-z (+ current-side-dist-z delta-z)
                         current-map-z (+ current-map-z step-z)
                         hit-side 'z)))
              
              ;; Check if there's a block at this position
              (let ((block-type (get-block current-map-x current-map-y current-map-z)))
                (when block-type
                  ;; Calculate exact hit point and face
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
                       :block-x current-map-x
                       :block-y current-map-y
                       :block-z current-map-z
                       :face hit-face
                       :hit-x hit-x
                       :hit-y hit-y
                       :hit-z hit-z))))))))
        (make-raycast-result))))

(defun break-block (world-x world-y world-z)
  "Removes a block at the specified world coordinates."
  ;; Added debug output to verify block breaking is called
  (format t "[v0] Breaking block at (~A, ~A, ~A)~%" world-x world-y world-z)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (let ((old-block (aref (chunk-blocks chunk) local-x local-y local-z)))
          (format t "[v0] Old block: ~A~%" old-block)
          (setf (aref (chunk-blocks chunk) local-x local-y local-z) nil)
          (setf (chunk-needs-geometry-update chunk) t)
          ;; Mark neighboring chunks as potentially needing updates too
          (loop for dx from -1 to 1 do
            (loop for dy from -1 to 1 do
              (loop for dz from -1 to 1 do
                (unless (and (zerop dx) (zerop dy) (zerop dz))
                  (let ((neighbor-chunk (get-chunk (+ chunk-x dx) (+ chunk-y dy) (+ chunk-z dz))))
                    (setf (chunk-needs-geometry-update neighbor-chunk) t)))))))))))

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

;; Placeholder for handle-input function
;; Placeholder for init-opengl function
;; Placeholder for setup-opengl function
;; Placeholder for get-block-color function
