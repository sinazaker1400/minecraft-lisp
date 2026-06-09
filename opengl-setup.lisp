(in-package :minecraft-3d)

(defvar *window* nil)
(defvar *gl-context* nil)
(defvar *running* t)
(defvar *last-frame-time* 0)

(defun initialize-window (&optional (width 1280) (height 720))
  "Initialize SDL2 window with OpenGL context"
  (sdl2:with-init (:video)
    (setf *window* 
          (sdl2:create-window :title "Minecraft"
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
    
    *window*))

(defun setup-opengl (width height)
  "Configure OpenGL state"
  (clear-color 0.5 0.7 1.0 1.0)
  (enable :depth-test)
  (depth-func :less)
  (enable :cull-face)
  (cull-face :back)
  (front-face :ccw)
  
  (matrix-mode :projection)
  (load-identity)
  (perspective 45.0 (/ width height) 0.1 1000.0)
  
  (matrix-mode :modelview)
  (load-identity))

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
  (viewport 0 0 width height)
  (matrix-mode :projection)
  (load-identity)
  (perspective 45.0 (/ width height) 0.1 1000.0)
  (matrix-mode :modelview))
