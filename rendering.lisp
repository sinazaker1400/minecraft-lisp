(in-package #:minecraft-3d)

;; Function to get block color (defined BEFORE calculate-face-geometry uses it)
;; This function must be available before calculate-face-geometry is defined.
(defun get-block-color (block-type)
  (case block-type
    (grass (values 0.2 0.7 0.2))
    (dirt (values 0.6 0.4 0.2))
    (stone (values 0.5 0.5 0.5))
    (wood (values 0.6 0.4 0.2))
    (otherwise (values 0.5 0.5 0.5))))

;; Old draw-cube function is no longer used directly for rendering.
;; (defun draw-cube (x y z block-type) ...)

(defun init-opengl (width height)
  (gl:viewport 0 0 width height)
  (gl:enable :depth-test)
  (gl:depth-func :less)
  (gl:clear-color 0.5 0.7 1.0 1.0))

(defun setup-opengl (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let* ((fovy 45.0)
         (aspect (/ width height))
         (near 0.1)
         (far 100.0)
         (f (/ 1.0 (tan (/ (* fovy 3.14159) 180.0 2.0)))))
    (gl:frustum (* (/ -1.0 aspect) f near)
                (* (/ 1.0 aspect) f near)
                (* -1.0 f near)
                (* 1.0 f near)
                near
                far))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at 12.0 70.0 12.0
               8.0  65.0 8.0
               0.0  1.0 0.0))

(defun render-world (player)
  (gl:clear-color 0.5 0.7 1.0 1.0)
  (gl:clear :color-buffer :depth-buffer)

  (gl:matrix-mode :modelview)
  (gl:load-identity)

  ;; The main let* block defining eye-x, eye-y, eye-z, etc.
  ;; EVERYTHING that needs eye-x, eye-y, eye-z goes INSIDE this let* or within nested lets inside it
  (let* ((eye-x (game-player-x player))      ; <-- eye-x defined here
         (eye-y (game-player-y player))      ; <-- eye-y defined here
         (eye-z (game-player-z player))      ; <-- eye-z defined here
         (rot-x (game-player-rot-x player))
         (rot-y (game-player-rot-y player))
         (cos-pitch (float (cos rot-x) 0.0))
         (forward-x (* cos-pitch (float (cos rot-y) 0.0)))
         (forward-y (float (sin rot-x) 0.0))
         (forward-z (* cos-pitch (float (sin rot-y) 0.0)))
         (distance 1.0)
         (look-at-x (+ eye-x (* distance forward-x)))
         (look-at-y (+ eye-y (* distance forward-y)))
         (look-at-z (+ eye-z (* distance forward-z))))
    ;; glu:look-at call using eye-x, eye-y, eye-z INSIDE the main let*
    (glu:look-at eye-x eye-y eye-z
                 look-at-x look-at-y look-at-z
                 0.0 1.0 0.0)

    ;; NOW, the rendering loop WITHIN the main let*
    ;; Define render distances
    (let ((render-distance-xz 2)
          (render-distance-y  4))
      ;; Calculate player chunk coordinates based on the eye position (still inside the main let*)
      ;; This line MUST be inside the main let* where eye-x, eye-y, eye-z are bound
      (multiple-value-bind (player-chunk-x player-chunk-y player-chunk-z)
          ;; This call uses eye-x, eye-y, eye-z from the OUTER let*
          (world-coords-to-chunk-coords eye-x eye-y eye-z) ; <-- CORRECT PLACE: INSIDE MAIN LET*, INSIDE INNER LET
        ;; Loop through chunks in X, Y, Z range
        ;; All the nested loops and rendering code go here, INSIDE the multiple-value-bind
        (loop for cx from (- player-chunk-x render-distance-xz) to (+ player-chunk-x render-distance-xz) do
          (loop for cy from (- player-chunk-y render-distance-y) to (+ player-chunk-y render-distance-y) do
            (loop for cz from (- player-chunk-z render-distance-xz) to (+ player-chunk-z render-distance-xz) do
              (let ((chunk (get-chunk cx cy cz))) ; Ensure chunk exists/gets generated
                ;; Calculate geometry only if not already calculated
                (when (null (chunk-visible-faces-geometry chunk))
                  (calculate-chunk-geometry chunk))
                ;; Render the visible faces
                (dolist (face-data (chunk-visible-faces-geometry chunk))
                  (gl:with-primitives :quads
                    (dolist (vertex face-data)
                      (destructuring-bind (x y z nx ny nz r g b) vertex
                        (gl:normal nx ny nz)
                        (gl:color r g b)
                        (gl:vertex x y z))))))))))))

  (gl:flush))