(in-package :minecraft-3d)

;; ====== TIMING ======
(defparameter *last-frame-time* 0.0)
(defparameter *frame-delta-time* 0.016) ;; Start with 60 FPS assumption

(defun get-delta-time ()
  "Get elapsed time since last frame in seconds"
  (let ((current-time (/ (get-internal-real-time)
                         (float internal-time-units-per-second))))
    (if (zerop *last-frame-time*)
        (setf *last-frame-time* current-time)
        (let ((dt (- current-time *last-frame-time*)))
          (setf *last-frame-time* current-time)
          (setf *frame-delta-time* (max 0.001 (min 0.05 dt)))))))

;; ====== MOUSE TRACKING ======
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *mouse-initialized* nil)

(defun update-mouse-look (player)
  "Update camera rotation based on mouse movement"
  ;; lispbuilder-sdl uses SDL:MOUSE-X and SDL:MOUSE-Y
  (let ((x (sdl:mouse-x))
        (y (sdl:mouse-y)))
    (if (not *mouse-initialized*)
        (progn
          (setf *mouse-x* x)
          (setf *mouse-y* y)
          (setf *mouse-initialized* t))
        (let ((delta-x (- x *mouse-x*))
              (delta-y (- y *mouse-y*)))
          ;; Update yaw (horizontal)
          (incf (game-player-rot-y player)
                (* delta-x *mouse-sensitivity*))
          ;; Update pitch (vertical) with clamping
          (incf (game-player-rot-x player)
                (* delta-y *mouse-sensitivity*))
          (setf (game-player-rot-x player)
                (max (- *max-pitch*)
                     (min *max-pitch*
                           (game-player-rot-x player))))
          (setf *mouse-x* x)
          (setf *mouse-y* y)))))

;; ====== COLLISION DETECTION ======
(defun can-occupy-space-p (x y z)
  "Check if player-sized space is free"
  (let ((margin 0.3))
    ;; Check 4 corners at feet level
    (and (zerop (or (get-block (floor (+ x margin)) (floor y) (floor z)) 0))
         (zerop (or (get-block (floor (- x margin)) (floor y) (floor z)) 0))
         (zerop (or (get-block (floor x) (floor y) (floor (+ z margin))) 0))
         (zerop (or (get-block (floor x) (floor y) (floor (- z margin))) 0))
         ;; Check 4 corners at head level
         (zerop (or (get-block (floor (+ x margin)) (floor (+ y 1.7)) (floor z)) 0))
         (zerop (or (get-block (floor (- x margin)) (floor (+ y 1.7)) (floor z)) 0))
         (zerop (or (get-block (floor x) (floor (+ y 1.7)) (floor (+ z margin))) 0))
         (zerop (or (get-block (floor x) (floor (+ y 1.7)) (floor (- z margin))) 0)))))

(defun player-on-ground-p (player)
  "Check if player is standing on solid ground"
  (not (zerop (or (get-block (floor (game-player-x player))
                             (floor (- (game-player-y player) 0.1))
                             (floor (game-player-z player))) 0))))

(defun apply-gravity (player dt)
  "Apply gravity and handle jumping"
  (unless (player-on-ground-p player)
    ;; Falling
    (decf (game-player-vel-y player) (* *gravity* dt))
    ;; Terminal velocity
    (setf (game-player-vel-y player)
          (max (game-player-vel-y player) -60.0)))
  ;; Apply vertical velocity
  (let ((new-y (+ (game-player-y player)
                  (* (game-player-vel-y player) dt))))
    (if (can-occupy-space-p (game-player-x player) new-y (game-player-z player))
        (setf (game-player-y player) new-y)
        ;; Hit something
        (when (< (game-player-vel-y player) 0)
          ;; Hit ground
          (setf (game-player-vel-y player) 0.0)
          ;; Convert floor result to float
          (setf (game-player-y player) (float (floor new-y)))))))

;; ====== KEYBOARD INPUT ======
(defun handle-input (player dt)
  "Handle keyboard movement with delta-time"
  (let ((speed (* *move-speed* dt)))
    (let ((yaw (game-player-rot-y player))
          (cos-yaw (cos (game-player-rot-y player)))
          (sin-yaw (sin (game-player-rot-y player))))
      ;; Forward (W)
      (when (sdl:key-pressed-p :w)
        (let ((new-x (+ (game-player-x player) (* speed cos-yaw)))
              (new-z (+ (game-player-z player) (* speed sin-yaw))))
          (when (can-occupy-space-p new-x (game-player-y player) new-z)
            (setf (game-player-x player) new-x)
            (setf (game-player-z player) new-z))))
      ;; Backward (S)
      (when (sdl:key-pressed-p :s)
        (let ((new-x (- (game-player-x player) (* speed cos-yaw)))
              (new-z (- (game-player-z player) (* speed sin-yaw))))
          (when (can-occupy-space-p new-x (game-player-y player) new-z)
            (setf (game-player-x player) new-x)
            (setf (game-player-z player) new-z))))
      ;; Strafe left (A)
      (when (sdl:key-pressed-p :a)
        (let ((angle (+ yaw *pi-half*)))
          (let ((new-x (+ (game-player-x player) (* speed (cos angle))))
                (new-z (+ (game-player-z player) (* speed (sin angle)))))
            (when (can-occupy-space-p new-x (game-player-y player) new-z)
              (setf (game-player-x player) new-x)
              (setf (game-player-z player) new-z)))))
      ;; Strafe right (D)
      (when (sdl:key-pressed-p :d)
        (let ((angle (- yaw *pi-half*)))
          (let ((new-x (+ (game-player-x player) (* speed (cos angle))))
                (new-z (+ (game-player-z player) (* speed (sin angle)))))
            (when (can-occupy-space-p new-x (game-player-y player) new-z)
              (setf (game-player-x player) new-x)
              (setf (game-player-z player) new-z)))))
      ;; Jump (Space)
      (when (and (sdl:key-pressed-p :space) (player-on-ground-p player))
        (setf (game-player-vel-y player) 10.0)))))
