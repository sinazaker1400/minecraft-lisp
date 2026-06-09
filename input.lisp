(in-package :minecraft-3d)

(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)
(defvar *mouse-captured* t)

(defun update-player-input (player)
  "Update player movement based on keyboard and mouse input"
  (let ((dt (/ (get-delta-time) 1000.0)))
    ;; Get current keyboard state
    (let ((keys (sdl2:get-keyboard-state)))
      ;; Forward (W)
      (when (aref keys (sdl2:scancode-value :scancode-w))
        (let ((angle (player-yaw player)))
          (incf (aref (player-pos player) 0) (* 5 dt (cos angle)))
          (incf (aref (player-pos player) 2) (* 5 dt (sin angle)))))
      
      ;; Backward (S)
      (when (aref keys (sdl2:scancode-value :scancode-s))
        (let ((angle (player-yaw player)))
          (decf (aref (player-pos player) 0) (* 5 dt (cos angle)))
          (decf (aref (player-pos player) 2) (* 5 dt (sin angle)))))
      
      ;; Left Strafe (A)
      (when (aref keys (sdl2:scancode-value :scancode-a))
        (let ((angle (- (player-yaw player) (/ pi 2))))
          (incf (aref (player-pos player) 0) (* 5 dt (cos angle)))
          (incf (aref (player-pos player) 2) (* 5 dt (sin angle)))))
      
      ;; Right Strafe (D)
      (when (aref keys (sdl2:scancode-value :scancode-d))
        (let ((angle (+ (player-yaw player) (/ pi 2))))
          (incf (aref (player-pos player) 0) (* 5 dt (cos angle)))
          (incf (aref (player-pos player) 2) (* 5 dt (sin angle)))))
      
      ;; Jump (Space)
      (when (aref keys (sdl2:scancode-value :scancode-space))
        (when (zerop (aref (player-vel player) 1))
          (setf (aref (player-vel player) 1) 8.0))))
    
    ;; Apply gravity
    (decf (aref (player-vel player) 1) (* 9.81 dt))
    (incf (aref (player-pos player) 1) (* (aref (player-vel player) 1) dt))
    
    ;; Simple ground collision
    (when (<= (aref (player-pos player) 1) 0)
      (setf (aref (player-pos player) 1) 0)
      (setf (aref (player-vel player) 1) 0))))

(defun handle-mouse-input (player)
  "Update player camera based on mouse movement"
  (multiple-value-bind (x y state)
      (sdl2:mouse-state)
    (let ((dx (- x *last-mouse-x*))
          (dy (- y *last-mouse-y*)))
      (setf *last-mouse-x* x)
      (setf *last-mouse-y* y)
      
      ;; Update yaw (horizontal rotation)
      (decf (player-yaw player) (* dx 0.005))
      
      ;; Update pitch (vertical rotation)
      (incf (player-pitch player) (* dy 0.005))
      
      ;; Clamp pitch to prevent flipping
      (setf (player-pitch player)
            (max -1.57 (min 1.57 (player-pitch player)))))))

(defun get-delta-time ()
  "Get delta time in milliseconds since last frame"
  (let ((current-time (sdl2:get-ticks)))
    (prog1
        (- current-time *last-frame-time*)
      (setf *last-frame-time* current-time))))
