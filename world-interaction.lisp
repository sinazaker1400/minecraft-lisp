(in-package #:minecraft-3d)

(defun break-block (x y z)

  (multiple-value-bind (cx cy cz)
      (world-coords-to-chunk-coords x y z)

    (let ((chunk (get-chunk cx cy cz)))

      (setf
       (aref
        (chunk-blocks chunk)
        (mod x 16)
        (mod y 16)
        (mod z 16))
       nil)

      (setf (chunk-needs-geometry-update chunk)
            t))))

(defun place-block (x y z type)

  (multiple-value-bind (cx cy cz)
      (world-coords-to-chunk-coords x y z)

    (let ((chunk (get-chunk cx cy cz)))

      (setf
       (aref
        (chunk-blocks chunk)
        (mod x 16)
        (mod y 16)
        (mod z 16))
       type)

      (setf (chunk-needs-geometry-update chunk)
            t))))

(defun raycast-block (sx sy sz dx dy dz)

  (loop
    for distance from 0.0 to +max-ray-distance+ by 0.05

    for x = (floor (+ sx (* dx distance)))
    for y = (floor (+ sy (* dy distance)))
    for z = (floor (+ sz (* dz distance)))

    when (get-block x y z)
      do
        (return
          (make-raycast-result
            :hit-p t
            :block-x x
            :block-y y
            :block-z z
            :hit-x (+ sx (* dx distance))
            :hit-y (+ sy (* dy distance))
            :hit-z (+ sz (* dz distance))))

    finally
      (return
        (make-raycast-result :hit-p nil))))

(defun perform-raycast (player)

  (let* ((pitch (game-player-rot-x player))
         (yaw   (game-player-rot-y player))

         (cos-pitch (cos pitch))

         (dx (* cos-pitch (cos yaw)))
         (dy (sin pitch))
         (dz (* cos-pitch (sin yaw))))

    (raycast-block
     (game-player-x player)
     (game-player-y player)
     (game-player-z player)
     dx dy dz)))