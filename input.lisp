(in-package :minecraft-3d)

;; ====== TIMING ======
(defparameter *last-frame-time* 0.0)
(defparameter *frame-delta-time* 0.016)

(defun get-delta-time ()
  "Get elapsed time since last frame in seconds"
  (let ((current-time (/ (get-internal-real-time)
                         (float internal-time-units-per-second))))
    (if (zerop *last-frame-time*)
        (setf *last-frame-time* current-time)
        (let ((dt (- current-time *last-frame-time*)))
          (setf *last-frame-time* current-time)
          (setf *frame-delta-time* (max 0.001 (min 0.05 dt)))))))

;; ====== KEYBOARD INPUT ======

(defun handle-input (player dt)
  "Handle keyboard movement with delta-time"
  (let ((speed (* *move-speed* dt)))
    (let ((yaw (game-player-rot-y player))
          (cos-yaw (cos (game-player-rot-y player)))
          (sin-yaw (sin (game-player-rot-y player))))
      
      ;; Forward (W)
      (when (sdl:key-down-p :sdl-key-w)
        (let ((new-x (+ (game-player-x player) (* speed cos-yaw)))
              (new-z (+ (game-player-z player) (* speed sin-yaw))))
          (when (can-occupy-space-p new-x (game-player-y player) new-z)
            (setf (game-player-x player) new-x)
            (setf (game-player-z player) new-z))))
      
      ;; Backward (S)
      (when (sdl:key-down-p :sdl-key-s)
        (let ((new-x (- (game-player-x player) (* speed cos-yaw)))
              (new-z (- (game-player-z player) (* speed sin-yaw))))
          (when (can-occupy-space-p new-x (game-player-y player) new-z)
            (setf (game-player-x player) new-x)
            (setf (game-player-z player) new-z))))
      
      ;; Strafe left (A)
      (when (sdl:key-down-p :sdl-key-a)
        (let ((angle (+ yaw *pi-half*)))
          (let ((new-x (+ (game-player-x player) (* speed (cos angle))))
                (new-z (+ (game-player-z player) (* speed (sin angle)))))
            (when (can-occupy-space-p new-x (game-player-y player) new-z)
              (setf (game-player-x player) new-x)
              (setf (game-player-z player) new-z)))))
      
      ;; Strafe right (D)
      (when (sdl:key-down-p :sdl-key-d)
        (let ((angle (- yaw *pi-half*)))
          (let ((new-x (+ (game-player-x player) (* speed (cos angle))))
                (new-z (+ (game-player-z player) (* speed (sin angle)))))
            (when (can-occupy-space-p new-x (game-player-y player) new-z)
              (setf (game-player-x player) new-x)
              (setf (game-player-z player) new-z)))))
      
      ;; Jump (Space)
      (when (and (sdl:key-down-p :sdl-key-space) (player-on-ground-p player))
        (setf (game-player-vel-y player) 10.0))
      
      ;; Look up (Arrow Up)
      (when (sdl:key-down-p :sdl-key-up)
        (incf (game-player-rot-x player) (* *mouse-sensitivity* 50 dt))
        (setf (game-player-rot-x player)
              (min *max-pitch* (game-player-rot-x player))))
      
      ;; Look down (Arrow Down)
      (when (sdl:key-down-p :sdl-key-down)
        (decf (game-player-rot-x player) (* *mouse-sensitivity* 50 dt))
        (setf (game-player-rot-x player)
              (max (- *max-pitch*) (game-player-rot-x player))))
      
      ;; Look left (Arrow Left)
      (when (sdl:key-down-p :sdl-key-left)
        (decf (game-player-rot-y player) (* *mouse-sensitivity* 100 dt)))
      
      ;; Look right (Arrow Right)
      (when (sdl:key-down-p :sdl-key-right)
        (incf (game-player-rot-y player) (* *mouse-sensitivity* 100 dt))))))

;; ====== COLLISION DETECTION ======

(defun can-occupy-space-p (x y z)
  "Check if player-sized space is free"
  (let ((margin 0.3))
    (and (zerop (get-block (floor (+ x margin)) (floor y) (floor z)))
         (zerop (get-block (floor (- x margin)) (floor y) (floor z)))
         (zerop (get-block (floor x) (floor y) (floor (+ z margin))))
         (zerop (get-block (floor x) (floor y) (floor (- z margin))))
         (zerop (get-block (floor (+ x margin)) (floor (+ y 1.7)) (floor z)))
         (zerop (get-block (floor (- x margin)) (floor (+ y 1.7)) (floor z)))
         (zerop (get-block (floor x) (floor (+ y 1.7)) (floor (+ z margin))))
         (zerop (get-block (floor x) (floor (+ y 1.7)) (floor (- z margin)))))))

(defun player-on-ground-p (player)
  "Check if player is standing on solid ground"
  (not (zerop (get-block (floor (game-player-x player))
                         (floor (- (game-player-y player) 0.1))
                         (floor (game-player-z player))))))

(defun apply-gravity (player dt)
  "Apply gravity and handle jumping"
  (unless (player-on-ground-p player)
    (decf (game-player-vel-y player) (* *gravity* dt))
    (setf (game-player-vel-y player)
          (max (game-player-vel-y player) -60.0)))
  
  (let ((new-y (+ (game-player-y player)
                  (* (game-player-vel-y player) dt))))
    (if (can-occupy-space-p (game-player-x player) new-y (game-player-z player))
        (setf (game-player-y player) new-y)
        (when (< (game-player-vel-y player) 0)
          (setf (game-player-vel-y player) 0.0)
          (setf (game-player-y player) (floor new-y))))))
