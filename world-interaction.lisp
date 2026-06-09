(in-package :minecraft-3d)

;; ====== COORDINATE CONVERSION ======
(defun world->local (world-coord chunk-size)
  "Convert world coordinate to local chunk coordinate"
  (mod world-coord chunk-size))

(defun world->chunk (world-coord chunk-size)
  "Convert world coordinate to chunk index"
  (floor world-coord chunk-size))

;; ====== BLOCK INTERACTION ======
(defun break-block (x y z)
  "Remove block at world coordinates"
  (let ((chunk-x (world->chunk x *chunk-size*))
        (chunk-z (world->chunk z *chunk-size*))
        (local-x (world->local x *chunk-size*))
        (local-y y)
        (local-z (world->local z *chunk-size*)))
    (let ((chunk-key (cons chunk-x chunk-z)))
      (when (gethash chunk-key *minecraft-world*)
        (let ((chunk (gethash chunk-key *minecraft-world*)))
          (when (and (>= local-y 0) (< local-y *chunk-height*)
                     (>= local-x 0) (< local-x *chunk-size*)
                     (>= local-z 0) (< local-z *chunk-size*))
            (setf (aref (chunk-blocks chunk) local-x local-y local-z) 0)))))))

(defun place-block (x y z block-id)
  "Place block at world coordinates"
  (let ((chunk-x (world->chunk x *chunk-size*))
        (chunk-z (world->chunk z *chunk-size*))
        (local-x (world->local x *chunk-size*))
        (local-y y)
        (local-z (world->local z *chunk-size*)))
    (let ((chunk-key (cons chunk-x chunk-z)))
      ;; Generate chunk if it doesn't exist
      (unless (gethash chunk-key *minecraft-world*)
        (generate-chunk chunk-x chunk-z))
      (let ((chunk (gethash chunk-key *minecraft-world*)))
        (when (and (>= local-y 0) (< local-y *chunk-height*)
                   (>= local-x 0) (< local-x *chunk-size*)
                   (>= local-z 0) (< local-z *chunk-size*))
          (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-id))))))

(defun raycast-from-player (player max-distance)
  "Cast ray from player and return first block hit"
  (let ((pitch (game-player-rot-x player))
        (yaw (game-player-rot-y player))
        (px (game-player-x player))
        (py (game-player-y player))
        (pz (game-player-z player)))
    ;; Direction vector from spherical coordinates
    (let ((cos-pitch (cos pitch))
          (sin-pitch (sin pitch))
          (cos-yaw (cos yaw))
          (sin-yaw (sin yaw)))
      (let ((dx (* cos-pitch sin-yaw))
            (dy (- sin-pitch))
            (dz (* cos-pitch cos-yaw)))
        ;; Step along ray
        (let ((steps (floor (* max-distance 2))))
          (dotimes (i steps)
            (let ((t-dist (/ i 2.0)))
              (let ((x (+ px (* t-dist dx)))
                    (y (+ py (* t-dist dy)))
                    (z (+ pz (* t-dist dz))))
                (when (> (or (get-block (floor x) (floor y) (floor z)) 0) 0)
                  ;; Hit a block, return position
                  (return-from raycast-from-player
                    (values (floor x) (floor y) (floor z) t-dist)))))))))))
