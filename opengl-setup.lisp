(in-package :minecraft-3d)

;; ====== WINDOW STATE ======
(defparameter *window-width* 1280)
(defparameter *window-height* 720)
(defparameter *window-open* nil)

(defun initialize-window ()
  "Initialize SDL and OpenGL context"
  (sdl:init-sdl)
  
  ;; Create window with lispbuilder-sdl
  (setf *window-open* 
        (sdl:create-video-surface
         *window-width*
         *window-height*
         :flags '(sdl:sdl-opengl)))
  
  ;; Setup OpenGL
  (gl:clear-color 0.5 0.7 1.0 1.0) ;; Sky blue
  
  ;; Perspective setup
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 70.0 (/ *window-width* *window-height*) 0.1 500.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  ;; Enable depth testing
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  
  ;; Enable face culling
  (gl:enable :cull-face)
  (gl:cull-face :back))

(defun shutdown-window ()
  "Clean up SDL and OpenGL"
  (when *window-open*
    (sdl:quit)))

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
  (sdl:update-display))
