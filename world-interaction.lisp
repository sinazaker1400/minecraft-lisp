;; File: world-interaction.lisp
(defpackage #:minecraft-3d
  (:use #:cl #:lispbuilder-sdl)
  (:export #:perform-raycast
           #:break-block
           #:place-block
           #:update-chunk-geometries
           #:start-game))
(in-package #:minecraft-3d)

(defun raycast-block (start-x start-y start-z dir-x dir-y dir-z)
  "Cast a ray using Amanatides-Woo DDA algorithm for guaranteed closest block detection."
  ;; Rewritten using proper grid-based DDA that checks each voxel in order
  (let* ((max-distance 200.0)
         ;; Initialize grid position
         (grid-x (floor start-x))
         (grid-y (floor start-y))
         (grid-z (floor start-z))
         ;; Determine step direction (1 or -1 for each axis)
         (step-x (cond ((> dir-x 0.0001) 1) ((< dir-x -0.0001) -1) (t 0)))
         (step-y (cond ((> dir-y 0.0001) 1) ((< dir-y -0.0001) -1) (t 0)))
         (step-z (cond ((> dir-z 0.0001) 1) ((< dir-z -0.0001) -1) (t 0)))
         ;; Calculate t-delta (how far along ray to cross grid boundary for each axis)
         (t-delta-x (if (zerop step-x) max-distance (/ 1.0 (abs dir-x))))
         (t-delta-y (if (zerop step-y) max-distance (/ 1.0 (abs dir-y))))
         (t-delta-z (if (zerop step-z) max-distance (/ 1.0 (abs dir-z))))
         ;; Calculate t-max (distance to next grid boundary for each axis)
         (t-max-x (cond ((> step-x 0) (- (1+ grid-x) start-x))
                        ((< step-x 0) (- start-x grid-x))
                        (t max-distance)))
         (t-max-y (cond ((> step-y 0) (- (1+ grid-y) start-y))
                        ((< step-y 0) (- start-y grid-y))
                        (t max-distance)))
         (t-max-z (cond ((> step-z 0) (- (1+ grid-z) start-z))
                        ((< step-z 0) (- start-z grid-z))
                        (t max-distance)))
         (t-traveled 0.0)
         (last-face :top))
    
    ;; Traverse grid cells until we find a block or exceed max distance
    (loop
      ;; Find which axis has the smallest t-max (next boundary to cross)
      (let ((t-min (min t-max-x t-max-y t-max-z)))
        ;; Stop if we've traveled too far
        (when (> t-min max-distance)
          (return (make-raycast-result)))
        
        ;; Determine which axis we're crossing and update grid position
        (cond
          ((= t-min t-max-x)
           (setf last-face (if (> step-x 0) :left :right))
           (incf grid-x step-x)
           (incf t-max-x t-delta-x))
          ((= t-min t-max-y)
           (setf last-face (if (> step-y 0) :bottom :top))
           (incf grid-y step-y)
           (incf t-max-y t-delta-y))
          (t
           (setf last-face (if (> step-z 0) :back :front))
           (incf grid-z step-z)
           (incf t-max-z t-delta-z)))
        
        ;; Check if there's a block at this grid position
        (let ((block-type (get-block grid-x grid-y grid-z)))
          (when block-type
            ;; Calculate exact hit position
            (let* ((hit-x (+ start-x (* t-min dir-x)))
                   (hit-y (+ start-y (* t-min dir-y)))
                   (hit-z (+ start-z (* t-min dir-z))))
              (return-from raycast-block
                (make-raycast-result
                 :hit-p t
                 :block-x grid-x
                 :block-y grid-y
                 :block-z grid-z
                 :face last-face
                 :hit-x hit-x
                 :hit-y hit-y
                 :hit-z hit-z)))))))))

(defun perform-raycast (player)
  "Cast a ray from the player's eye in the look direction and return the first block hit."
  (let* ((start-x (game-player-x player))
         (start-y (game-player-y player))
         (start-z (game-player-z player))
         (rot-x (game-player-rot-x player))
         (rot-y (game-player-rot-y player))
         (cos-pitch (float (cos rot-x) 0.0))
         ;; Fixed direction to match camera direction in rendering.lisp
         (dir-x (* cos-pitch (float (cos rot-y) 0.0)))
         (dir-y (float (sin rot-x) 0.0))
         (dir-z (* cos-pitch (float (sin rot-y) 0.0)))
         ;; Normalize direction vector
         (length (sqrt (+ (* dir-x dir-x) (* dir-y dir-y) (* dir-z dir-z))))
         (normalized-dir-x (/ dir-x length))
         (normalized-dir-y (/ dir-y length))
         (normalized-dir-z (/ dir-z length)))
    ;; Add comprehensive debug output showing eye position and direction
    (format t "[v0] Raycast Eye: (~,3F, ~,3F, ~,3F) Direction: (~,3F, ~,3F, ~,3F)~%"
            start-x start-y start-z normalized-dir-x normalized-dir-y normalized-dir-z)
    (raycast-block start-x start-y start-z
                   normalized-dir-x normalized-dir-y normalized-dir-z)))






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
