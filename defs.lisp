(in-package :minecraft-3d)

;; Game state structure
(defstruct game-player
  (x 0.0 :type single-float)
  (y 64.0 :type single-float)
  (z 0.0 :type single-float)
  (rot-x 0.0 :type single-float)  ;; Pitch
  (rot-y 0.0 :type single-float)) ;; Yaw

(defstruct game-state
  (player (make-game-player))
  (world (make-hash-table :test 'equal))
  (running t)
  (last-frame-time 0)
  (current-chunk-cx 0)
  (current-chunk-cz 0)
  (window-width 800)
  (window-height 600)
  (render-distance 8))

;; Constants
(defparameter *chunk-size* 16)
(defparameter *chunk-height* 256)
(defparameter *move-speed* 20.0)  ;; Units per second (was hardcoded before)
(defparameter *mouse-sensitivity* 0.001)
(defparameter *max-pitch* (/ pi 2.5))  ;; Prevent flipping
(defparameter *pi-half* (/ pi 2.0))

;; Block types (using integers for memory efficiency)
(defparameter *block-types*
  '((0 :air)
    (1 :grass)
    (2 :dirt)
    (3 :stone)
    (4 :sand)
    (5 :water)
    (6 :wood)
    (7 :leaves)))

(defparameter *block-id-map* 
  (let ((map (make-hash-table)))
    (dolist (entry *block-types*) 
      (setf (gethash (second entry) map) (first entry)))
    map))

(defparameter *block-color-map*
  (let ((map (make-hash-table)))
    (setf (gethash 0 map) '(0.0 0.0 0.0))      ;; Air
    (setf (gethash 1 map) '(0.0 1.0 0.0))      ;; Grass (green)
    (setf (gethash 2 map) '(0.6 0.4 0.2))      ;; Dirt (brown)
    (setf (gethash 3 map) '(0.5 0.5 0.5))      ;; Stone (gray)
    (setf (gethash 4 map) '(1.0 0.9 0.5))      ;; Sand (yellow)
    (setf (gethash 5 map) '(0.2 0.5 1.0))      ;; Water (blue)
    (setf (gethash 6 map) '(0.4 0.2 0.0))      ;; Wood (dark brown)
    (setf (gethash 7 map) '(0.0 0.8 0.0))      ;; Leaves (dark green)
    map))

;; Chunk structure
(defstruct chunk
  (blocks (make-array (list *chunk-size* *chunk-height* *chunk-size*) 
                      :element-type '(unsigned-byte 8) 
                      :initial-element 0))
  (x 0)
  (z 0)
  (geometry-list nil)
  (needs-geometry-update t)
  (vao 0)
  (vertex-count 0))

;; Helper: Convert world coordinates to chunk coordinates
(defun world-coords-to-chunk-coords (x y z)
  "Returns (chunk-x chunk-y chunk-z)"
  (values (floor x *chunk-size*)
          (floor y *chunk-size*)
          (floor z *chunk-size*)))

;; Helper: Convert world coordinate to local chunk coordinate
(defun world->local (world-coord chunk-size)
  "Converts a world coordinate to local chunk coordinate (0-15)"
  (mod (floor world-coord) chunk-size))

;; Helper: Get block at world coordinates
(defun get-block (x y z)
  "Returns block type at world coordinates, nil if out of bounds"
  (when (and (>= y 0) (< y *chunk-height*))
    (multiple-value-bind (cx cy cz)
      (world-coords-to-chunk-coords x y z)
      (let ((chunk (gethash (list cx cy cz) *minecraft-world*)))
        (when chunk
          (let ((local-x (world->local x *chunk-size*))
                (local-y (world->local y *chunk-size*))
                (local-z (world->local z *chunk-size*)))
            (aref (chunk-blocks chunk) local-x local-y local-z)))))))

;; Helper: Set block at world coordinates
(defun set-block (x y z block-type)
  "Sets block at world coordinates"
  (when (and (>= y 0) (< y *chunk-height*))
    (multiple-value-bind (cx cy cz)
      (world-coords-to-chunk-coords x y z)
      (let ((chunk (gethash (list cx cy cz) *minecraft-world*)))
        (when chunk
          (let ((local-x (world->local x *chunk-size*))
                (local-y (world->local y *chunk-size*))
                (local-z (world->local z *chunk-size*)))
            (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)
            (setf (chunk-needs-geometry-update chunk) t)))))))

;; Helper: Get block color
(defun get-block-color (block-type)
  "Returns RGB color list for block type"
  (gethash block-type *block-color-map* '(0.5 0.5 0.5)))
