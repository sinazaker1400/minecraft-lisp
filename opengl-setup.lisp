(in-package :minecraft-3d)

;; ====== WINDOW STATE ======
(defparameter *window-width* 1280)
(defparameter *window-height* 720)
(defparameter *window* nil)

(defun initialize-window ()
  "Initialize SDL and OpenGL context"
  ;; Initialize SDL with video support
  (sdl:with-init ()
    ;; Create window - lispbuilder-sdl uses different syntax
    (setf *window*
          (sdl:make-surface :width *window-width*
                            :height *window-height*))
    
    ;; Setup OpenGL
    (gl:clear-color 0.5 0.7 1.0 1.0) ;; Sky blue
    
    ;; Perspective setup using frustum (manual perspective)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    
    ;; Calculate frustum parameters for 70 degree FOV
    (let* ((fov 70.0)
           (aspect (/ *window-width* *window-height*))
           (near 0.1)
           (far 500.0)
           (f (/ 1.0 (tan (/ fov 2.0)))))
      (gl:frustum (- (/ near (* f aspect)))
                  (/ near (* f aspect))
                  (- (/ near f))
                  (/ near f)
                  near
                  far))
    
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    
    ;; Enable depth testing
    (gl:enable :depth-test)
    (gl:depth-func :lequal)
    
    ;; Enable face culling
    (gl:enable :cull-face)
    (gl:cull-face :back)))

(defun shutdown-window ()
  "Clean up SDL and OpenGL"
  ;; lispbuilder-sdl handles cleanup with with-init macro
  nil)

(defun setup-camera (player)
  "Set up camera position and orientation"
  (gl:load-identity)
  
  ;; Rotate by pitch (up/down)
  (gl:rotate (- (game-player-rot-x player)) 1.0 0.0 0.0)
  
  ;; Rotate by yaw (left/right)
  (gl:rotate (- (game-player-rot-y player)) 0.0 1.0 0.0)
  
  ;; Translate by player position (negative because we move world, not camera)
  (gl:translate (- (game-player-x player))
                (- (game-player-y player))
                (- (game-player-z player))))

(defun update-display ()
  "Update the display"
  (gl:flush)
  ;; lispbuilder-sdl uses sdl:update-display or similar
  (sdl:update-surface *window*))
