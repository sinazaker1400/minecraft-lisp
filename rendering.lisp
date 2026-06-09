(in-package :minecraft-3d)

;; ====== VERTEX & FACE DEFINITIONS ======
;; Cube vertices (normalized to 1x1x1)
(defparameter *cube-vertices*
  #(0.0 0.0 0.0  1.0 0.0 0.0  1.0 1.0 0.0  0.0 1.0 0.0  ;; Front
    1.0 0.0 1.0  0.0 0.0 1.0  0.0 1.0 1.0  1.0 1.0 1.0  ;; Back
    0.0 0.0 1.0  0.0 0.0 0.0  0.0 1.0 0.0  0.0 1.0 1.0  ;; Left
    1.0 0.0 0.0  1.0 0.0 1.0  1.0 1.0 1.0  1.0 1.0 0.0  ;; Right
    0.0 1.0 0.0  1.0 1.0 0.0  1.0 1.0 1.0  0.0 1.0 1.0  ;; Top
    0.0 0.0 1.0  1.0 0.0 1.0  1.0 0.0 0.0  0.0 0.0 0.0)) ;; Bottom

;; Face indices (quads - 2 triangles each)
(defparameter *cube-faces*
  #(0 1 2  2 3 0     ;; Front
    4 5 6  6 7 4     ;; Back
    8 9 10 10 11 8   ;; Left
    12 13 14 14 15 12 ;; Right
    16 17 18 18 19 16 ;; Top
    20 21 22 22 23 20)) ;; Bottom

(defun render-block (x y z block-id)
  "Render a single block at position"
  (when (> block-id 0) ;; Don't render air
    (let ((color (get-block-color block-id)))
      (gl:color3 (aref color 0) (aref color 1) (aref color 2))
      (gl:push-matrix)
      (gl:translate (float x) (float y) (float z))
      
      ;; Draw cube as triangles
      (gl:with-primitives :triangles
        (loop for i from 0 below (length *cube-faces*) by 3 do
          (let ((v1 (* (aref *cube-faces* i) 3))
                (v2 (* (aref *cube-faces* (+ i 1)) 3))
                (v3 (* (aref *cube-faces* (+ i 2)) 3)))
            (gl:vertex (aref *cube-vertices* v1)
                       (aref *cube-vertices* (+ v1 1))
                       (aref *cube-vertices* (+ v1 2)))
            (gl:vertex (aref *cube-vertices* v2)
                       (aref *cube-vertices* (+ v2 1))
                       (aref *cube-vertices* (+ v2 2)))
            (gl:vertex (aref *cube-vertices* v3)
                       (aref *cube-vertices* (+ v3 1))
                       (aref *cube-vertices* (+ v3 2))))))
      
      (gl:pop-matrix))))

(defun should-render-face-p (x y z direction)
  "Check if a face should be rendered (not blocked by adjacent block)"
  (multiple-value-bind (nx ny nz)
      (ecase direction
        (:front (values x y (+ z 1)))
        (:back (values x y (- z 1)))
        (:left (values (- x 1) y z))
        (:right (values (+ x 1) y z))
        (:top (values x (+ y 1) z))
        (:bottom (values x (- y 1) z)))
    (zerop (or (get-block nx ny nz) 0))))

(defun render-chunk (chunk player)
  "Render visible blocks in a chunk"
  (let ((blocks (chunk-blocks chunk))
        (chunk-x (chunk-x chunk))
        (chunk-z (chunk-z chunk))
        (player-chunk-dist 3)) ;; Only render chunks this distance away
    ;; Check if chunk is in view distance
    (let ((dist (+ (abs (- (floor (game-player-x player) *chunk-size*) chunk-x))
                   (abs (- (floor (game-player-z player) *chunk-size*) chunk-z)))))
      (when (<= dist player-chunk-dist)
        ;; Render each block
        (dotimes (lx *chunk-size*)
          (dotimes (ly *chunk-height*)
            (dotimes (lz *chunk-size*)
              (let ((block-id (aref blocks lx ly lz)))
                (when (> block-id 0) ;; Only render non-air blocks
                  (let ((world-x (+ (* chunk-x *chunk-size*) lx))
                        (world-y ly)
                        (world-z (+ (* chunk-z *chunk-size*) lz)))
                    (render-block world-x world-y world-z block-id)))))))))))

(defun render-scene (player)
  "Render the entire visible scene"
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; Set up camera
  (setup-camera player)
  
  ;; Render all loaded chunks
  (maphash (lambda (key chunk)
             (declare (ignore key))
             (render-chunk chunk player))
           *minecraft-world*)
  
  ;; Swap buffers
  (sdl:update-display))
