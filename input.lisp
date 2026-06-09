(in-package :minecraft)

;; Mouse tracking state
(defparameter *mouse-last-x* 0)
(defparameter *mouse-last-y* 0)
(defparameter *mouse-initialized* nil)

(defun handle-input (player delta-time)
  "Handle keyboard input with delta-time based movement"
  (let ((speed (* *move-speed* delta-time))
        (yaw (game-player-rot-y player))
        (cos-yaw (cos yaw))
        (sin-yaw (sin yaw)))
    
    ;; Forward movement (W key)
    (when (sdl:key-down-p :sdl-key-w)
      (incf (game-player-x player) (* speed cos-yaw))
      (incf (game-player-z player) (* speed sin-yaw)))
    
    ;; Backward movement (S key)
    (when (sdl:key-down-p :sdl-key-s)
      (decf (game-player-x player) (* speed cos-yaw))
      (decf (game-player-z player) (* speed sin-yaw)))
    
    ;; Strafe left (A key) - perpendicular to forward direction
    (when (sdl:key-down-p :sdl-key-a)
      (let ((strafe-angle (+ yaw *pi-half*)))
        (incf (game-player-x player) (* speed (cos strafe-angle)))
        (incf (game-player-z player) (* speed (sin strafe-angle)))))
    
    ;; Strafe right (D key)
    (when (sdl:key-down-p :sdl-key-d)
      (let ((strafe-angle (- yaw *pi-half*)))
        (incf (game-player-x player) (* speed (cos strafe-angle)))
        (incf (game-player-z player) (* speed (sin strafe-angle)))))
    
    ;; Up movement (Space key)
    (when (sdl:key-down-p :sdl-key-space)
      (incf (game-player-y player) speed))
    
    ;; Down movement (Ctrl key)
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

(defun handle-mouse-clicks (player)
  "Handle block breaking and placing"
  (let ((buttons (sdl:get-mouse-state)))
    ;; Left click: break block
    (when (sdl:mouse-button-p :button-left buttons)
      (let ((target (raycast-to-block player)))
        (when target
          (destructuring-bind (x y z) target
            (break-block x y z)))))
    
    ;; Right click: place block
    (when (sdl:mouse-button-p :button-right buttons)
      (let ((target (raycast-to-block player t)))  ;; t = get adjacent block
        (when target
          (destructuring-bind (x y z) target
            (place-block x y z 1)))))))  ;; 1 = grass block

(defun raycast-to-block (player &optional (adjacent nil))
  "Cast a ray from player camera and find intersected block.
   If adjacent is T, returns the block adjacent to the intersection (for placement)."
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
            (dy (* sin-pitch))
            (dz (* cos-pitch sin-yaw)))
        
        ;; Step along the ray
        (loop for distance from 0 upto max-distance by step-size
              for ray-x = (+ x (* distance dx))
              for ray-y = (+ y (* distance dy))
              for ray-z = (+ z (* distance dz))
              for block-type = (get-block (floor ray-x) (floor ray-y) (floor ray-z))
              when (and block-type (> block-type 0))
                do (if adjacent
                       ;; Return the block we came from (for placement)
                       (let ((prev-x (+ x (* (- distance step-size) dx)))
                             (prev-y (+ y (* (- distance step-size) dy)))
                             (prev-z (+ z (* (- distance step-size) dz))))
                         (return (list (floor prev-x) (floor prev-y) (floor prev-z))))
                       ;; Return the block we hit (for breaking)
                       (return (list (floor ray-x) (floor ray-y) (floor ray-z)))))))))
