(in-package :minecraft-3d)

(defparameter *window* nil)
(defparameter *running* t)
(defparameter *width* 1280)
(defparameter *height* 720)

(defun initialize-window ()
  "Initialize SDL and OpenGL"
  ;; Initialize SDL video subsystem
  (sdl:init-sdl :video t)
  
  ;; Load OpenGL core
  (load-lisp-opengl-core)
  
  ;; Setup viewport and clear color
  (viewport 0 0 *width* *height*)
  (clear-color 0.53 0.81 0.92 1.0))  ;; Sky blue

(defun shutdown-window ()
  "Clean up SDL resources"
  (sdl:quit-sdl))

(defun swap-buffers ()
  "Swap front and back buffers"
  (sdl:update-display))

(defun handle-events (player)
  "Process SDL events"
  (sdl:with-events ()
    (:quit-event () 
      (setf *running* nil)
      nil)
    (:key-down-event (:keysym keysym)
      (when (eq (sdl:scancode keysym) :sdl-scancode-escape)
        (setf *running* nil)))
    (t () t))
  *running*)
