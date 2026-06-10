(in-package :minecraft-3d)

(defvar *last-mouse-x* 640)
(defvar *last-mouse-y* 360)
(defvar *mouse-captured* t)

(defun update-player-input (player)
  "Update player movement based on keyboard and mouse input"
  (let ((dt (/ (get-delta-time) 1000.0)))
    ;; Get current keyboard state
    (let ((keys (sdl2:get-keyboard-state)))
      ;; Forward (W)
      (when (aref keys (sdl2:scancode-value :scancode-w))
        (let ((angle (game-player-rot-y player)))
          (incf (game-player-x player) (* 5 dt (cos angle)))
          (incf (game-player-z player) (* 5 dt (sin angle)))))
      
      ;; Backward (S)
      (when (aref keys (sdl2:scancode-value :scancode-s))
        (let ((angle (game-player-rot-y player)))
          (decf (game-player-x player) (* 5 dt (cos angle)))
          (decf (game-player-z player) (* 5 dt (sin angle)))))
      
      ;; Left Strafe (A)
      (when (aref keys (sdl2:scancode-value :scancode-a))
        (let ((angle (- (game-player-rot-y player) (/ pi 2))))
          (incf (game-player-x player) (* 5 dt (cos angle)))
          (incf (game-player-z player) (* 5 dt (sin angle)))))
      
      ;; Right Strafe (D)
      (when (aref keys (sdl2:scancode-value :scancode-d))
        (let ((angle (+ (game-player-rot-y player) (/ pi 2))))
          (incf (game-player-x player) (* 5 dt (cos angle)))
          (incf (game-player-z player) (* 5 dt (sin angle)))))
      
      ;; Up (Space)
      (when (aref keys (sdl2:scancode-value :scancode-space))
        (incf (game-player-y player) (* 5 dt)))
      
      ;; Down (LShift)
      (when (aref keys (sdl2:scancode-value :scancode-lshift))
        (decf (game-player-y player) (* 5 dt)))))
  
  ;; Handle mouse input
  (handle-mouse-input player))

(defun handle-mouse-input (player)
  "Update player camera based on mouse movement"
  (multiple-value-bind (x y state)
      (sdl2:mouse-state)
    (let ((dx (- x *last-mouse-x*))
          (dy (- y *last-mouse-y*)))
      (setf *last-mouse-x* x)
      (setf *last-mouse-y* y)
      
      ;; Update yaw (horizontal rotation)
      (decf (game-player-rot-y player) (* dx 0.005))
      
      ;; Update pitch (vertical rotation)
      (incf (game-player-rot-x player) (* dy 0.005))
      
      ;; Clamp pitch to prevent flipping
      (setf (game-player-rot-x player)
            (max (- (/ pi 2)) (min (/ pi 2) (game-player-rot-x player)))))))

(defun get-delta-time ()
  "Get delta time in milliseconds since last frame"
  (let ((current-time (sdl2:get-ticks)))
    (prog1
        (- current-time *last-frame-time*)
      (setf *last-frame-time* current-time))))
