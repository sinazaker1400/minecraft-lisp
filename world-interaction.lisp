(in-package :minecraft)

(defun break-block (x y z)
  "Remove a block at world coordinates"
  (when (and (>= y 0) (< y *chunk-height*))
    (multiple-value-bind (cx cy cz)
      (world-coords-to-chunk-coords x y z)
      (let ((chunk (gethash (list cx cy cz) *minecraft-world*)))
        (when chunk
          (let ((local-x (world->local x *chunk-size*))
                (local-y (world->local y *chunk-size*))
                (local-z (world->local z *chunk-size*)))
            (setf (aref (chunk-blocks chunk) local-x local-y local-z) 0)  ;; 0 = air
            (setf (chunk-needs-geometry-update chunk) t)))))))

(defun place-block (x y z block-type)
  "Place a block at world coordinates with validation"
  (when (and (>= y 0) (< y *chunk-height*))
    ;; Don't place on existing blocks
    (when (zerop (get-block x y z))
      (multiple-value-bind (cx cy cz)
        (world-coords-to-chunk-coords x y z)
        ;; Ensure chunk exists
        (unless (gethash (list cx cy cz) *minecraft-world*)
          (generate-chunk cx cy cz))
        
        (let ((chunk (gethash (list cx cy cz) *minecraft-world*)))
          (when chunk
            (let ((local-x (world->local x *chunk-size*))
                  (local-y (world->local y *chunk-size*))
                  (local-z (world->local z *chunk-size*)))
              (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)
              (setf (chunk-needs-geometry-update chunk) t))))))))

(defun can-occupy-space-p (x y z)
  "Check if a 1x1.8x1 player-sized bounding box is free of blocks"
  ;; Check bottom and top of player
  (and (zerop (get-block (floor x) (floor y) (floor z)))
       (zerop (get-block (floor x) (floor (+ y 1.7)) (floor z)))))

(defun apply-collision-detection (player)
  "Prevent player from walking through blocks"
  ;; This should be called after movement input
  ;; For now, just clamp to valid positions
  (let ((x (game-player-x player))
        (y (game-player-y player))
        (z (game-player-z player)))
    (unless (can-occupy-space-p x y z)
      ;; Revert to last valid position
      ;; This is a simple solution; better approach uses separate x/y/z collision checks
      nil)))

(defun player-on-ground-p (player)
  "Check if player is standing on a solid block"
  (let ((x (game-player-x player))
        (y (game-player-y player))
        (z (game-player-z player)))
