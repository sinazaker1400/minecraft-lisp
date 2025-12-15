(in-package #:minecraft-3d)

(defun handle-input (player)
  "Handles keyboard input for player movement"
  (let ((speed 1.0))
    ;; Movement (WASD keys) with directional calculation
    (when (sdl:key-down-p :sdl-key-w)
      (incf (game-player-x player)
            (float (* speed (cos (game-player-rot-y player))) 0.0))
      (incf (game-player-z player)
            (float (* speed (sin (game-player-rot-y player))) 0.0)))
    (when (sdl:key-down-p :sdl-key-s)
      (decf (game-player-x player)
            (float (* speed (cos (game-player-rot-y player))) 0.0))
      (decf (game-player-z player)
            (float (* speed (sin (game-player-rot-y player))) 0.0)))
    (when (sdl:key-down-p :sdl-key-a)
      (decf (game-player-x player)
            (float (* speed (cos (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0))
      (decf (game-player-z player)
            (float (* speed (sin (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0)))
    (when (sdl:key-down-p :sdl-key-d)
      (incf (game-player-x player)
            (float (* speed (cos (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0))
      (incf (game-player-z player)
            (float (* speed (sin (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0)))

    ;; Vertical movement (Space and LShift)
    (when (sdl:key-down-p :sdl-key-space)
      (incf (game-player-y player) (float speed 0.0)))
    (when (sdl:key-down-p :sdl-key-lshift)
      (decf (game-player-y player) (float speed 0.0)))))

;; Handle mouse look rotation based on mouse movement delta
(defun handle-mouse-look (player delta-x delta-y)
  "Updates player rotation based on mouse movement delta"
  (when (and delta-x delta-y)
    ;; Yaw (horizontal look) - delta-x
    (incf (game-player-rot-y player) (float (* delta-x *mouse-sensitivity*) 0.0))
    ;; Pitch (vertical look) - negate delta-y so mouse up = look up
    (incf (game-player-rot-x player) (float (* (- delta-y) *mouse-sensitivity*) 0.0))
    ;; Clamp pitch to prevent looking too far up/down
    (when (> (game-player-rot-x player) (float (/ pi 2.0) 0.0))
      (setf (game-player-rot-x player) (float (/ pi 2.0) 0.0)))
    (when (< (game-player-rot-x player) (float (/ pi -2.0) 0.0))
      (setf (game-player-rot-x player) (float (/ pi -2.0) 0.0)))))
