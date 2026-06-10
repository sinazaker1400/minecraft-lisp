(in-package :minecraft-3d)

;; ====== GAME STATE STRUCTURES ======
(defstruct game-player
  (x 0.0 :type single-float)
  (y 64.0 :type single-float)
  (z 0.0 :type single-float)
  (rot-x 0.0 :type single-float) ;; Pitch (up/down)
  (rot-y 0.0 :type single-float) ;; Yaw (left/right)
  (vel-y 0.0 :type single-float)) ;; Vertical velocity for falling

(defstruct chunk
  (blocks (make-array (list 16 256 16)
                      :element-type '(unsigned-byte 8)
                      :initial-element 0))
  (x 0)
  (z 0)
  (vertex-list nil)
  (needs-update t))

;; ====== CONSTANTS ======
(defparameter *chunk-size* 16)
(defparameter *chunk-height* 256)
(defparameter *move-speed* 20.0) ;; Units per second
(defparameter *mouse-sensitivity* 0.003)
(defparameter *max-pitch* (/ pi 2.5))
(defparameter *gravity* 9.81)
(defparameter *pi-half* (/ pi 2.0))

;; ====== WORLD STATE ======
(defparameter *minecraft-world* (make-hash-table :test 'equal))
(defparameter *world-seed* 12345)

;; ====== BLOCK TYPES ======
(defparameter *block-id-map*
  (let ((map (make-hash-table)))
    (setf (gethash :air map) 0)
    (setf (gethash :grass map) 1)
    (setf (gethash :dirt map) 2)
    (setf (gethash :stone map) 3)
    (setf (gethash :sand map) 4)
    (setf (gethash :water map) 5)
    (setf (gethash :wood map) 6)
    (setf (gethash :leaves map) 7)
    map))

(defparameter *block-color-map*
  (let ((map (make-hash-table)))
    (setf (gethash 0 map) #(0.0 0.0 0.0)) ;; Air (invisible)
    (setf (gethash 1 map) #(0.2 0.8 0.2)) ;; Grass (green)
    (setf (gethash 2 map) #(0.6 0.4 0.2)) ;; Dirt (brown)
    (setf (gethash 3 map) #(0.5 0.5 0.5)) ;; Stone (gray)
    (setf (gethash 4 map) #(0.95 0.9 0.5)) ;; Sand (yellow)
    (setf (gethash 5 map) #(0.2 0.5 1.0)) ;; Water (blue)
    (setf (gethash 6 map) #(0.4 0.2 0.0)) ;; Wood (brown)
    (setf (gethash 7 map) #(0.0 0.7 0.0)) ;; Leaves (dark green)
    map))

;; ====== HELPER FUNCTIONS ======
(defun world-coords-to-chunk-coords (x y z)
  "Convert world coordinates to chunk coordinates"
  (values (floor x *chunk-size*)
          (floor y *chunk-size*)
          (floor z *chunk-size*)))

(defun world->local (world-coord)
  "Convert world coordinate to local chunk coordinate (0-15)"
  (mod (floor world-coord) *chunk-size*))

(defun get-chunk (cx cy cz)
  "Get or create chunk at given chunk coordinates"
  (let ((key (list cx cy cz)))
    (or (gethash key *minecraft-world*)
        (let ((new-chunk (make-chunk :x cx :z cz)))
          (setf (gethash key *minecraft-world*) new-chunk)
          new-chunk))))

(defun get-block (x y z)
  "Get block type at world coordinates"
  (when (and (>= y 0) (< y *chunk-height*))
    (multiple-value-bind (cx cy cz)
        (world-coords-to-chunk-coords x y z)
      (let ((chunk (gethash (list cx cy cz) *minecraft-world*)))
        (if chunk
            (aref (chunk-blocks chunk)
                  (world->local x)
                  (world->local y)
                  (world->local z))
            1))))) ;; Return grass if chunk not loaded

(defun set-block (x y z block-id)
  "Set block at world coordinates"
  (when (and (>= y 0) (< y *chunk-height*))
    (multiple-value-bind (cx cy cz)
        (world-coords-to-chunk-coords x y z)
      (let ((chunk (get-chunk cx cy cz)))
        (setf (aref (chunk-blocks chunk)
                    (world->local x)
                    (world->local y)
                    (world->local z))
              block-id)
        (setf (chunk-needs-update chunk) t)))))

(defun get-block-color (block-id)
  "Get RGB color for a block"
  (or (gethash block-id *block-color-map*)
      #(0.5 0.5 0.5)))
