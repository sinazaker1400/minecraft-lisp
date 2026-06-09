(in-package :minecraft-3d)

;; Global timing state
(defparameter *last-frame-time* 0.0)

(defun get-delta-time ()
  "Get elapsed time since last frame in seconds"
  (let ((current-time (/ (get-internal-real-time) 
                         (float internal-time-units-per-second))))
    (if (zerop *last-frame-time*)
        (setf *last-frame-time* current-time)
        (let ((dt (- current-time *last-frame-time*)))
          (setf *last-frame-time* current-time)
          (max 0.001 (min 0.05 dt))))))  ;; Clamp between 1ms-50ms

;; Mouse tracking state
(defparameter *mouse-last-x* 0)
(defparameter *mouse-last-y* 0)
(defparameter *mouse-initialized* nil)

(defun handle-input (player delta-time)
  "Handle keyboard input with delta-time based movement"
  (let ((speed (* *move-speed* delta-time))
        (yaw (game-player-rot-y player))
        (cos-yaw (cos (game-player-rot-y player)))
        (sin-yaw (sin (game-player-rot-y player))))
    
    ;; Forward movement (W key)
    (when (sdl:key-down-p :sdl-key-w)
      (let ((new-x (+ (game-player-x player) (* speed cos-yaw)))
            (new-z (+ (game-player-z player) (* speed sin-yaw))))
        (when (can-occupy-space-p new-x (game-player-y player) new-z)
          (setf (game-player-x player) new-x
                (game-player-z player) new-z))))
    
    ;; Backward movement (S key)
    (when (sdl:key-down-p :sdl-key-s)
      (let ((new-x (- (game-player-x player) (* speed cos-yaw)))
            (new-z (- (game-player-z player) (* speed sin-yaw))))
        (when (can-occupy-space-p new-x (game-player-y player) new-z)
          (setf (game-player-x player) new-x
                (game-player-z player) new-z))))
    
    ;; Strafe left (A key) - perpendicular to forward direction
    (when (sdl:key-down-p :sdl-key-a)
      (let ((strafe-angle (+ yaw *pi-half*)))
        (let ((new-x (+ (game-player-x player) (* speed (cos strafe-angle))))
              (new-z (+ (game-player-z player) (* speed (sin strafe-angle)))))
          (when (can-occupy-space-p new-x (game-player-y player) new-z)
            (setf (game-player-x player) new-x
                  (game-player-z player) new-z)))))
    
    ;; Strafe right (D key)
    (when (sdl:key-down-p :sdl-key-d)
      (let ((strafe-angle (- yaw *pi-half*)))
        (let ((new-x (+ (game-player-x player) (* speed (cos strafe-angle))))
              (new-z (+ (game-player-z player) (* speed (sin strafe-angle)))))
          (when (can-occupy-space-p new-x (game-player-y player) new-z)
            (setf (game-player-x player) new-x
                  (game-player-z player) new-z)))))
    
    ;; Up movement (Space key)
    (when (sdl:key-down-p :sdl-key-space)
      (incf (game-player-y player) speed))
    
    ;; Down movement (LCtrl key)
    (when (sdl:key-down-p :sdl-key-lctrl)
      (decf (game-player-y player) speed))))

(defun handle-mouse-look (player delta-x delta-y)
  "Handle mouse-based camera rotation"
  (let ((sensitivity *mouse-sensitivity*))
    ;; Update yaw (horizontal rotation) - X axis movement
    (incf (game-player-rot-y player) (* delta-x sensitivity))
    
    ;; Update pitch (vertical rotation) - Y axis movement
    (incf (game-player-rot-x player) (* delta-y sensitivity))
    
    ;; Clamp pitch to prevent flipping
    (setf (game-player-rot-x player)
          (max (- *max-pitch*)
               (min *max-pitch* (game-player-rot-x player))))))

(defun update-mouse-look (player)
  "Get current mouse position and update camera accordingly"
  (multiple-value-bind (x y _) (sdl:get-mouse-state)
    (if (not *mouse-initialized*)
        ;; First frame: just set the baseline
        (progn
          (setf *mouse-last-x* x)
          (setf *mouse-last-y* y)
          (setf *mouse-initialized* t))
        ;; Subsequent frames: calculate delta and update
        (let ((delta-x (- x *mouse-last-x*))
              (delta-y (- y *mouse-last-y*)))
          (handle-mouse-look player delta-x delta-y)
          (setf *mouse-last-x* x)
          (setf *mouse-last-y* y)))))

(defun perform-raycast (player)
  "Cast a ray from player camera and find intersected block with metadata"
  (let ((max-distance 8.0)
        (step-size 0.1)
        (x (game-player-x player))
        (y (+ (game-player-y player) 1.6))  ;; Eye height
        (z (game-player-z player))
        (pitch (game-player-rot-x player))
        (yaw (game-player-rot-y player)))
    
    ;; Ray direction based on pitch and yaw
    (let ((cos-pitch (cos pitch))
          (sin-pitch (sin pitch))
          (cos-yaw (cos yaw))
          (sin-yaw (sin yaw)))
      (let ((dx (* cos-pitch cos-yaw))
            (dy (sin pitch))
            (dz (* cos-pitch sin-yaw)))
        
        ;; Step along the ray
        (loop for distance from 0 upto max-distance by step-size
              for ray-x = (+ x (* distance dx))
              for ray-y = (+ y (* distance dy))
              for ray-z = (+ z (* distance dz))
              for block-x = (floor ray-x)
              for block-y = (floor ray-y)
              for block-z = (floor ray-z)
              for block-type = (get-block block-x block-y block-z)
              when (and block-type (> block-type 0))
                do (return (make-raycast-result
                            :hit-p t
                            :block-x block-x
                            :block-y block-y
                            :block-z block-z
                            :block-type block-type
                            :distance distance
                            :face (determine-face-hit ray-x ray-y ray-z
                                                      block-x block-y block-z)))
              finally (return (make-raycast-result :hit-p nil))))))

;; Raycast result structure
(defstruct raycast-result
  (hit-p nil)
  (block-x 0)
  (block-y 0)
  (block-z 0)
  (block-type 0)
  (distance 0.0)
  (face :front))  ;; :top, :bottom, :left, :right, :front, :back

(defun determine-face-hit (ray-x ray-y ray-z block-x block-y block-z)
  "Determine which face of the block was hit by the ray"
  (let ((fx (- ray-x block-x))
        (fy (- ray-y block-y))
        (fz (- ray-z block-z)))
    (let ((abs-x (abs fx))
          (abs-y (abs fy))
          (abs-z (abs fz)))
      (cond
        ((and (>= abs-x abs-y) (>= abs-x abs-z))
         (if (>= fx 0.5) :right :left))
        ((and (>= abs-y abs-x) (>= abs-y abs-z))
         (if (>= fy 0.5) :top :bottom))
        (t
         (if (>= fz 0.5) :front :back))))))
