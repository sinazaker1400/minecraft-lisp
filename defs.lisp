(in-package #:minecraft-3d)

;; --- Constants ---
(defparameter *window-width* 800)
(defparameter *window-height* 600)

;; Define the size of a single chunk section (16x16x16)
(defparameter *chunk-size-x* 16)
(defparameter *chunk-size-y* 16)
(defparameter *chunk-size-z* 16)

;; Hash table to store chunk sections, keyed by (list chunk_x chunk_y chunk_z)
(defparameter *world-chunks* (make-hash-table :test 'equal))

(defconstant +max-ray-distance+ 10.0)

;; --- Data Structures ---
(defstruct world-block
  "A block in the world"
  type
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defstruct chunk
  "A 16x16x16 chunk section of the world"
  (x 0 :type fixnum) ; Chunk's X coordinate in the world
  (y 0 :type fixnum) ; Chunk's Y coordinate in the world
  (z 0 :type fixnum) ; Chunk's Z coordinate in the world
  (blocks (make-array (list *chunk-size-x* *chunk-size-y* *chunk-size-z*)
                      :initial-element nil
                      :element-type '(or symbol null))) ; Array storing block types
  ;; Add a slot for visible faces geometry (list of face definitions)
  (visible-faces-geometry '()) ; Initialize as an empty list
  ;; Add a flag to mark if the geometry needs updating
  (needs-geometry-update t)) ; Initialize as true

(defstruct game-player
  (x 8.0 :type single-float)
  (y 80.0 :type single-float)  ; Start above ground
  (z 8.0 :type single-float)
  (rot-x 0.0 :type single-float)  ; Looking direction (pitch)
  (rot-y 0.0 :type single-float)) ; Looking direction (yaw)

;; Add this if it's not already in your defs.lisp file
(defstruct raycast-result
  (hit-p nil :type boolean)
  (block-x 0 :type fixnum)
  (block-y 0 :type fixnum)
  (block-z 0 :type fixnum)
  (face :unknown :type (member :top :bottom :front :back :left :right :unknown))
  (hit-x 0.0 :type single-float)
  (hit-y 0.0 :type single-float)
  (hit-z 0.0 :type single-float))

;; Add this if it's not already in your defs.lisp file
