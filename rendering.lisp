(in-package #:minecraft-3d)

(defun draw-face (face-data)
  (gl:with-primitives :quads
    (dolist (vertex face-data)
      (destructuring-bind
          (x y z nx ny nz r g b)
          vertex

        (gl:normal nx ny nz)
        (gl:color r g b)
        (gl:vertex x y z)))))

(defun draw-highlighted-block (bx by bz)

  (let ((half-size 0.5))

    (gl:disable :lighting)

    (gl:color 1.0 1.0 0.0)
    (gl:line-width 2.0)

    ;; top
    (gl:with-primitives :line-loop
      (gl:vertex (- bx half-size) (+ by half-size) (- bz half-size))
      (gl:vertex (+ bx half-size) (+ by half-size) (- bz half-size))
      (gl:vertex (+ bx half-size) (+ by half-size) (+ bz half-size))
      (gl:vertex (- bx half-size) (+ by half-size) (+ bz half-size)))

    ;; bottom
    (gl:with-primitives :line-loop
      (gl:vertex (- bx half-size) (- by half-size) (- bz half-size))
      (gl:vertex (+ bx half-size) (- by half-size) (- bz half-size))
      (gl:vertex (+ bx half-size) (- by half-size) (+ bz half-size))
      (gl:vertex (- bx half-size) (- by half-size) (+ bz half-size)))

    ;; verticals
    (gl:with-primitives :lines

      (gl:vertex (- bx half-size) (- by half-size) (- bz half-size))
      (gl:vertex (- bx half-size) (+ by half-size) (- bz half-size))

      (gl:vertex (+ bx half-size) (- by half-size) (- bz half-size))
      (gl:vertex (+ bx half-size) (+ by half-size) (- bz half-size))

      (gl:vertex (+ bx half-size) (- by half-size) (+ bz half-size))
      (gl:vertex (+ bx half-size) (+ by half-size) (+ bz half-size))

      (gl:vertex (- bx half-size) (- by half-size) (+ bz half-size))
      (gl:vertex (- bx half-size) (+ by half-size) (+ bz half-size)))

    (gl:line-width 1.0)
    (gl:enable :lighting)))

(defun draw-crosshair ()

  (gl:matrix-mode :projection)
  (gl:push-matrix)
  (gl:load-identity)

  (gl:ortho
   0.0
   (float *window-width*)
   (float *window-height*)
   0.0
   -1.0
   1.0)

  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:load-identity)

  (gl:disable :depth-test)
  (gl:disable :lighting)

  (let ((cx (/ *window-width* 2.0))
        (cy (/ *window-height* 2.0))
        (size 10.0))

    (gl:color 1.0 1.0 1.0)

    (gl:with-primitives :lines

      (gl:vertex (- cx size) cy 0.0)
      (gl:vertex (+ cx size) cy 0.0)

      (gl:vertex cx (- cy size) 0.0)
      (gl:vertex cx (+ cy size) 0.0)))

  (gl:enable :depth-test)
  (gl:enable :lighting)

  (gl:pop-matrix)

  (gl:matrix-mode :projection)
  (gl:pop-matrix)

  (gl:matrix-mode :modelview))

(defun render-chunk (chunk)

  (when (or (chunk-needs-geometry-update chunk)
            (null (chunk-visible-faces-geometry chunk)))

    (calculate-chunk-geometry chunk)

    (setf (chunk-needs-geometry-update chunk)
          nil))

  (dolist (face-data
           (chunk-visible-faces-geometry chunk))

    (draw-face face-data)))

(defun setup-camera (player)

  (let* ((eye-x (game-player-x player))
         (eye-y (game-player-y player))
         (eye-z (game-player-z player))

         (pitch (game-player-rot-x player))
         (yaw   (game-player-rot-y player))

         (cos-pitch (cos pitch))

         (dir-x (* cos-pitch (cos yaw)))
         (dir-y (sin pitch))
         (dir-z (* cos-pitch (sin yaw))))

    (glu:look-at
     eye-x eye-y eye-z

     (+ eye-x dir-x)
     (+ eye-y dir-y)
     (+ eye-z dir-z)

     0.0 1.0 0.0)))

(defun render-visible-chunks (player)

  (multiple-value-bind (pcx pcy pcz)

      (world-coords-to-chunk-coords
       (game-player-x player)
       (game-player-y player)
       (game-player-z player))

    (loop
      for cx from (- pcx *render-distance-xz*)
      to       (+ pcx *render-distance-xz*)
      do

      (loop
        for cy from (- pcy *render-distance-y*)
        to       (+ pcy *render-distance-y*)
        do

        (loop
          for cz from (- pcz *render-distance-xz*)
          to       (+ pcz *render-distance-xz*)
          do

          (render-chunk
           (get-chunk cx cy cz)))))))

(defun render-world (player)

  (gl:clear-color 0.5 0.7 1.0 1.0)

  (gl:clear
   :color-buffer
   :depth-buffer)

  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (setup-camera player)

  (render-visible-chunks player)

  (when *targeted-block*

    (destructuring-bind
        (bx by bz)
        *targeted-block*

      (draw-highlighted-block
       bx by bz)))

  (draw-crosshair)

  (gl:flush))

(defun block-color (block)

  (case block
    (grass '(0.2 0.8 0.2))
    (dirt  '(0.5 0.3 0.1))
    (stone '(0.6 0.6 0.6))
    (t     '(1.0 1.0 1.0))))

(defun make-cube-face
       (x y z nx ny nz block)

  (destructuring-bind
      (r g b)
      (block-color block)

    (cond

      ((and (= nx 1) (= ny 0) (= nz 0))
       (list
        (list (+ x 1) y z nx ny nz r g b)
        (list (+ x 1) (+ y 1) z nx ny nz r g b)
        (list (+ x 1) (+ y 1) (+ z 1) nx ny nz r g b)
        (list (+ x 1) y (+ z 1) nx ny nz r g b)))

      ((and (= nx -1) (= ny 0) (= nz 0))
       (list
        (list x y z nx ny nz r g b)
        (list x y (+ z 1) nx ny nz r g b)
        (list x (+ y 1) (+ z 1) nx ny nz r g b)
        (list x (+ y 1) z nx ny nz r g b)))

      ((and (= ny 1) (= nx 0) (= nz 0))
       (list
        (list x (+ y 1) z nx ny nz r g b)
        (list x (+ y 1) (+ z 1) nx ny nz r g b)
        (list (+ x 1) (+ y 1) (+ z 1) nx ny nz r g b)
        (list (+ x 1) (+ y 1) z nx ny nz r g b)))

      ((and (= ny -1) (= nx 0) (= nz 0))
       (list
        (list x y z nx ny nz r g b)
        (list (+ x 1) y z nx ny nz r g b)
        (list (+ x 1) y (+ z 1) nx ny nz r g b)
        (list x y (+ z 1) nx ny nz r g b)))

      ((and (= nz 1) (= nx 0) (= ny 0))
       (list
        (list x y (+ z 1) nx ny nz r g b)
        (list (+ x 1) y (+ z 1) nx ny nz r g b)
        (list (+ x 1) (+ y 1) (+ z 1) nx ny nz r g b)
        (list x (+ y 1) (+ z 1) nx ny nz r g b)))

      (t
       (list
        (list x y z nx ny nz r g b)
        (list x (+ y 1) z nx ny nz r g b)
        (list (+ x 1) (+ y 1) z nx ny nz r g b)
        (list (+ x 1) y z nx ny nz r g b))))))

(defun calculate-chunk-geometry (chunk)

  (let ((faces nil))

    (loop
      for lx below 16 do

      (loop
        for ly below 16 do

        (loop
          for lz below 16 do

          (let ((block
                 (aref
                  (chunk-blocks chunk)
                  lx ly lz)))

            (when block

              (let ((wx (+ (* (chunk-x chunk) 16) lx))
                    (wy (+ (* (chunk-y chunk) 16) ly))
                    (wz (+ (* (chunk-z chunk) 16) lz)))

                (unless (find-block (1+ wx) wy wz)
                  (push
                   (make-cube-face
                    wx wy wz
                    1 0 0
                    block)
                   faces))

                (unless (find-block (1- wx) wy wz)
                  (push
                   (make-cube-face
                    wx wy wz
                    -1 0 0
                    block)
                   faces))

                (unless (find-block wx (1+ wy) wz)
                  (push
                   (make-cube-face
                    wx wy wz
                    0 1 0
                    block)
                   faces))

                (unless (find-block wx (1- wy) wz)
                  (push
                   (make-cube-face
                    wx wy wz
                    0 -1 0
                    block)
                   faces))

                (unless (find-block wx wy (1+ wz))
                  (push
                   (make-cube-face
                    wx wy wz
                    0 0 1
                    block)
                   faces))

                (unless (find-block wx wy (1- wz))
                  (push
                   (make-cube-face
                    wx wy wz
                    0 0 -1
                    block)
                   faces))))))))

    (setf
     (chunk-visible-faces-geometry chunk)
     faces)))

(defun update-chunk-geometries ()

  (let ((chunks nil))

    (maphash
     (lambda (k v)
       (declare (ignore k))
       (push v chunks))
     *world-chunks*)

    (dolist (chunk chunks)

      (when (chunk-needs-geometry-update chunk)

        (calculate-chunk-geometry chunk)

        (setf (chunk-needs-geometry-update chunk)
              nil)))))