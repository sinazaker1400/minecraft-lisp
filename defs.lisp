(in-package #:minecraft-3d)

(defparameter *window-width* 1280)
(defparameter *window-height* 720)

(defparameter *chunk-size-x* 16)
(defparameter *chunk-size-y* 16)
(defparameter *chunk-size-z* 16)

(defparameter *render-distance-xz* 2)
(defparameter *render-distance-y* 2)

(defparameter *world-chunks*
  (make-hash-table :test #'equal))

(defconstant +max-ray-distance+ 10.0)

(defparameter *mouse-sensitivity* 0.003)

(defparameter *targeted-block* nil)

(defstruct chunk
  x
  y
  z

  (blocks
   (make-array '(16 16 16)
               :initial-element nil))

  (visible-faces-geometry nil)
  (needs-geometry-update t))

(defstruct game-player
  (x 8.0f0)
  (y 80.0f0)
  (z 8.0f0)

  (rot-x 0.0f0)
  (rot-y 0.0f0))

(defstruct raycast-result
  (hit-p nil)

  (block-x 0)
  (block-y 0)
  (block-z 0)

  (face :unknown)

  (hit-x 0.0f0)
  (hit-y 0.0f0)
  (hit-z 0.0f0))

(defparameter *game-random-state*
  (make-random-state))

(defun initialize-randomness (seed)
  "Initialize random state using a seed."

  (setf *game-random-state*
        (make-random-state t))

  ;; advance state deterministically
  (dotimes (i (mod seed 1000))
    (random 1000000 *game-random-state*))

  *game-random-state*)

(defparameter *debug-mode* nil)