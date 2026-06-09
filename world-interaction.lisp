(in-package :minecraft-3d)

(defun break-block (x y z)
  "Remove a block at world coordinates"
  (when (and (>= y 0) (< y *chunk-height*))
    (multiple-value-bind (cx cy cz)
        (world-coords-to-chunk-coords x y z)
      (let ((chunk (get-chunk cx cy cz)))
        (when chunk
          (let ((local-x (world->local x *chunk-size-x*))
                (local-y (world->local y *chunk-size-y*))
                (local-z (world->local z *chunk-size-z*)))
            (setf (aref (chunk-blocks chunk) local-x local-y local-z) 0) ;; 0 = air
            (setf (chunk-needs-geometry-update chunk) t)))))))

(defun place-block (x y z block-type)
  "Place a block at world coordinates with validation"
  (when (and (>= y 0) (< y *chunk-height*))
    ;; Don't place on existing blocks
    (when (zerop (get-block x y z))
      ;; Check if reachable (within 8 blocks)
      (when (< (raycast-distance-to-position x y z) 8.0)
        (multiple-value-bind (cx cy cz)
            (world-coords-to-chunk-coords x y z)
          ;; Ensure chunk exists
          (let ((chunk (get-chunk cx cy cz)))
            (when chunk
              (let ((local-x (world->local x *chunk-size-x*))
                    (local-y (world->local y *chunk-size-y*))
                    (local-z (world->local z *chunk-size-z*)))
                (setf (aref (chunk-blocks chunk) local-x local-y local-z) 
                      (if (symbolp block-type)
                          (gethash block-type *block-id-map*)
                          block-type))
                (setf (chunk-needs-geometry-update chunk) t)))))))))

(defun can-occupy-space-p (x y z)
  "Check if a 1x1.8x1 player-sized bounding box is free of blocks"
  ;; Check a cylinder-like shape around the player
  (let ((player-width 0.3)
        (player-height 1.7))
    (and 
      ;; Feet and lower body
      (zerop (get-block (floor (+ x player-width)) (floor y) (floor z)))
      (zerop (get-block (floor (- x player-width)) (floor y) (floor z)))
      (zerop (get-block (floor x) (floor y) (floor (+ z player-width))))
      (zerop (get-block (floor x) (floor y) (floor (- z player-width))))
      
      ;; Head
      (zerop (get-block (floor (+ x player-width)) (floor (+ y player-height)) (floor z)))
      (zerop (get-block (floor (- x player-width)) (floor (+ y player-height)) (floor z)))
      (zerop (get-block (floor x) (floor (+ y player-height)) (floor (+ z player-width))))
      (zerop (get-block (floor x) (floor (+ y player-height)) (floor (- z player-width)))))))

(defun player-on-ground-p (player)
  "Check if player is standing on a solid block"
  (let ((x (game-player-x player))
        (y (game-player-y player))
        (z (game-player-z player))
        (ground-distance 0.1))
    (not (zerop (get-block (floor x) (floor (- y ground-distance)) (floor z))))))

(defun apply-gravity (player delta-time)
  "Apply gravity to the player"
  (let ((gravity 9.8)
        (terminal-velocity 60.0))
    ;; Only apply if not on ground
    (unless (player-on-ground-p player)
      (let ((new-y (- (game-player-y player) (* gravity delta-time))))
        (when (can-occupy-space-p (game-player-x player) new-y (game-player-z player))
          (setf (game-player-y player) new-y))))))

(defun raycast-distance-to-position (x y z)
  "Return distance from player to world coordinates (simple Euclidean)"
  ;; This is a placeholder - ideally should use actual player position
  ;; For now return a fixed safe distance
  0.0)
