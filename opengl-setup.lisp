(in-package :minecraft-3d)

(defvar *window* nil)
(defvar *gl-context* nil)
(defvar *running* t)
(defvar *last-frame-time* 0)

(defun initialize-window (&optional (width 1280) (height 720))
  "Initialize SDL2 window with OpenGL context"
  (setf *window*
        (sdl2:create-window :title "Minecraft Lisp"
                            :x :centered
                            :y :centered
                            :w width
                            :h height
                            :flags '(:shown :opengl)))
  (setf *gl-context* (sdl2:gl-create-context *window*))
  (sdl2:gl-make-current *window* *gl-context*)
  (sdl2:gl-set-swap-interval 1)
  (setup-opengl width height)
  (setf *last-frame-time* (sdl2:get-ticks))
  *window*)

(defun setup-opengl (width height)
  "Configure OpenGL state"
  (gl:clear-color 0.5 0.7 1.0 1.0)
  (gl:enable :depth-test)
  (gl:depth-func :less)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45.0 (/ width height) 0.1 1000.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun cleanup-window ()
  "Clean up SDL and OpenGL resources"
  (when *gl-context*
    (sdl2:gl-delete-context *gl-context*))
  (when *window*
    (sdl2:destroy-window *window*))
  (sdl2:quit))

(defun swap-buffers ()
  "Swap OpenGL buffers"
  (sdl2:gl-swap-window *window*))

(defun set-viewport (width height)
  "Set OpenGL viewport"
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45.0 (/ width height) 0.1 1000.0)
  (gl:matrix-mode :modelview))

(defun setup-camera (player)
  "Setup camera position and orientation based on player"
  (gl:load-identity)
  ;; Apply pitch rotation (up/down)
  (gl:rotate (- (* (game-player-rot-x player) 180) (/ pi 2)) 1.0 0.0 0.0)
  ;; Apply yaw rotation (left/right)
  (gl:rotate (* (game-player-rot-y player) (/ 180 pi)) 0.0 1.0 0.0)
  ;; Apply translation (inverse of player position)
  (gl:translate (- (game-player-x player))
                (- (game-player-y player))
                (- (game-player-z player))))
