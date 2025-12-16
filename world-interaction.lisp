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
  "Cast a ray and return information about the first block face hit using DDA algorithm."
  (let ((map-x (floor start-x))
        (map-y (floor start-y))
        (map-z (floor start-z))
        (step-x (if (>= dir-x 0) 1 -1))
        (step-y (if (>= dir-y 0) 1 -1))
        (step-z (if (>= dir-z 0) 1 -1))
        (delta-x (if (zerop dir-x) most-positive-single-float (/ 1.0 (abs dir-x))))
        (delta-y (if (zerop dir-y) most-positive-single-float (/ 1.0 (abs dir-y))))
        (delta-z (if (zerop dir-z) most-positive-single-float (/ 1.0 (abs dir-z))))
        (max-distance +max-ray-distance+))
    
    ;; Calculate initial side distances
    (let ((side-dist-x (if (>= dir-x 0)
                           (* (- (1+ map-x) start-x) delta-x)
                           (* (- start-x map-x) delta-x)))
          (side-dist-y (if (>= dir-y 0)
                           (* (- (1+ map-y) start-y) delta-y)
                           (* (- start-y map-y) delta-y)))
          (side-dist-z (if (>= dir-z 0)
                           (* (- (1+ map-z) start-z) delta-z)
                           (* (- start-z map-z) delta-z))))
      
      ;; Main DDA loop
      (do* ((current-x map-x)
            (current-y map-y)
            (current-z map-z)
            (t-max-x side-dist-x)
            (t-max-y side-dist-y)
            (t-max-z side-dist-z))
           ((> (min t-max-x t-max-y t-max-z) max-distance)
            (make-raycast-result))  ; No block found within max distance
        
        ;; Find which plane was crossed first
        (let ((t-max (min t-max-x t-max-y t-max-z)))
          (cond
            ;; X plane was crossed
            ((= t-max t-max-x)
             (setf current-x (+ current-x step-x))
             (setf t-max-x (+ t-max-x delta-x)))
            ;; Y plane was crossed
            ((= t-max t-max-y)
             (setf current-y (+ current-y step-y))
             (setf t-max-y (+ t-max-y delta-y)))
            ;; Z plane was crossed
            (t
             (setf current-z (+ current-z step-z))
             (setf t-max-z (+ t-max-z delta-z))))
          
          ;; Check if there's a block at the current position
          (let ((block-type (get-block current-x current-y current-z)))
            (when block-type
              ;; Improved block detection - only return if block actually exists
              (let ((face (cond
                            ((= (min t-max-x t-max-y t-max-z) t-max-x)
                             (if (< dir-x 0) :right :left))
                            ((= (min t-max-x t-max-y t-max-z) t-max-y)
                             (if (< dir-y 0) :top :bottom))
                            (t
                             (if (< dir-z 0) :front :back)))))
                (return-from raycast-block
                  (make-raycast-result
                   :hit-p t
                   :block-x current-x
                   :block-y current-y
                   :block-z current-z
                   :face face))))))))))

(defun perform-raycast (player)
  "Cast a ray from the player's eye in the look direction and return the first block hit."
  (let* ((start-x (+ (game-player-x player) 0.1))
         (start-y (game-player-y player))
         (start-z (+ (game-player-z player) 0.1))
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
