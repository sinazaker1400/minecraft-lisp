(defpackage #:minecraft-3d
  (:use #:cl #:sdl2 #:sdl2-image #:sdl2-mixer #:sdl2-ttf #:trivial-gamekit)
  (:export #:main))

(in-package #:minecraft-3d)

;; Include all the system files
(load "defs.lisp")
(load "inventory.lisp")
(load "world-interaction.lisp")
(load "biome-generation.lisp")
(load "physics.lisp")
(load "entities.lisp")
(load "day-night-cycle.lisp")
(load "game-state.lisp")

(defparameter *game-state* nil)
(defparameter *last-frame-time* 0)
(defparameter *window-width* 1024)
(defparameter *window-height* 768)

(defun main ()
  "Main entry point for the game"
  (sdl2:with-init (:sdl2-init-video :sdl2-init-timer)
    (sdl2:with-window (win :title "Minecraft 3D" :w *window-width* :h *window-height* 
                           :flags '(:shown))
      (sdl2:with-gl-context (gl-context win)
        ;; Initialize OpenGL
        (gl:clear-color 0.5 0.7 1.0 1.0)  ; Sky color
        
        ;; Initialize game state with a random seed
        (setf *game-state* (initialize-game-state (get-universal-time)))
        
        ;; Main game loop
        (setf *last-frame-time* (sdl2:get-ticks))
        (loop as quit = nil
              while (not quit)
              do (let ((current-time (sdl2:get-ticks))
                      (events (sdl2:poll-event)))
                   (loop for event in events
                         do (case (sdl2:evt-type event)
                              (:quit (setf quit t))
                              (:key-down
                               (handle-key-press (sdl2:keyboard-event-keysym-sym event)))
                              (:key-up
                               (handle-key-release (sdl2:keyboard-event-keysym-sym event)))
                              (:mouse-button-down
                               (handle-mouse-button-down (sdl2:mouse-button-event-button event)))
                              (:mouse-button-up
                               (handle-mouse-button-up (sdl2:mouse-button-event-button event)))
                              (:mouse-motion
                               (handle-mouse-motion 
                                (sdl2:mouse-motion-event-xrel event)
                                (sdl2:mouse-motion-event-yrel event)))))
                   
                   ;; Calculate delta time
                   (let ((delta-time (/ (- current-time *last-frame-time*) 1000.0)))
                     (setf *last-frame-time* current-time)
                     
                     ;; Update game state
                     (update-game-state *game-state* delta-time)
                     
                     ;; Render the frame
                     (render-frame))
                   
                   ;; Control frame rate
                   (sdl2:delay 16)))))))  ; ~60 FPS

(defun handle-key-press (key)
  "Handle key press events"
  (case key
    (:sdl-keycode-escape (sdl2:push-event :quit))
    (:sdl-keycode-w (handle-player-input *game-state* :w t))
    (:sdl-keycode-s (handle-player-input *game-state* :s t))
    (:sdl-keycode-a (handle-player-input *game-state* :a t))
    (:sdl-keycode-d (handle-player-input *game-state* :d t))
    (:sdl-keycode-space (handle-player-input *game-state* :space t))
    (:sdl-keycode-lshift (handle-player-input *game-state* :left-shift t))
    (:sdl-keycode-lctrl (handle-player-input *game-state* :left-control t))
    (:sdl-keycode-1 (handle-player-input *game-state* :1 t))
    (:sdl-keycode-2 (handle-player-input *game-state* :2 t))
    (:sdl-keycode-3 (handle-player-input *game-state* :3 t))
    (:sdl-keycode-4 (handle-player-input *game-state* :4 t))
    (:sdl-keycode-5 (handle-player-input *game-state* :5 t))
    (:sdl-keycode-6 (handle-player-input *game-state* :6 t))
    (:sdl-keycode-7 (handle-player-input *game-state* :7 t))
    (:sdl-keycode-8 (handle-player-input *game-state* :8 t))
    (:sdl-keycode-9 (handle-player-input *game-state* :9 t))))

(defun handle-key-release (key)
  "Handle key release events"
  (case key
    (:sdl-keycode-w (handle-player-input *game-state* :w nil))
    (:sdl-keycode-s (handle-player-input *game-state* :s nil))
    (:sdl-keycode-a (handle-player-input *game-state* :a nil))
    (:sdl-keycode-d (handle-player-input *game-state* :d nil))
    (:sdl-keycode-space (handle-player-input *game-state* :space nil))
    (:sdl-keycode-lshift (handle-player-input *game-state* :left-shift nil))
    (:sdl-keycode-lctrl (handle-player-input *game-state* :left-control nil))))

(defun handle-mouse-button-down (button)
  "Handle mouse button down events"
  (case button
    (1 (handle-player-input *game-state* :mouse-left t))   ; Left button
    (3 (handle-player-input *game-state* :mouse-right t)))) ; Right button

(defun handle-mouse-button-up (button)
  "Handle mouse button up events"
  (case button
    (1 (handle-player-input *game-state* :mouse-left nil))   ; Left button
    (3 (handle-player-input *game-state* :mouse-right nil)))) ; Right button

(defun handle-mouse-motion (xrel yrel)
  "Handle mouse motion for camera control"
  (when *game-state*
    (let* ((player (game-state-player *game-state*))
           (sensitivity 0.002))
      (incf (player-rotation-y player) (* xrel sensitivity))
      (incf (player-rotation-x player) (* yrel sensitivity))
      ;; Clamp vertical rotation to prevent flipping
      (setf (player-rotation-x player) 
            (max (- (/ pi 2)) (min (/ pi 2) (player-rotation-x player)))))))

(defun render-frame ()
  "Render a single frame"
  ;; Clear the screen with sky color
  (let ((sky-color (get-sky-color *game-state*)))
    (gl:clear-color (first sky-color) (second sky-color) (third sky-color) 1.0))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; Set up 3D perspective
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 75.0 (/ *window-width* *window-height*) 0.1 1000.0)
  
  ;; Set up camera view
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  ;; Position camera at player location and rotation
  (let ((player (game-state-player *game-state*)))
    ;; Apply inverse of player position to simulate camera
    (gl:translate (- (player-x player)) (- (player-y player)) (- (player-z player)))
    
    ;; Apply camera rotation
    (gl:rotate (player-rotation-x player) 1 0 0)  ; Pitch (up/down)
    (gl:rotate (player-rotation-y player) 0 1 0))  ; Yaw (left/right)
  
  ;; Render the world
  (render-world)
  
  ;; Render UI elements (inventory, health bar, etc.)
  (render-ui)
  
  ;; Swap buffers to display the frame
  (sdl2:gl-swap-window sdl2:*current-window*))

(defun render-world ()
  "Render the 3D world"
  ;; In a real implementation, this would render all the blocks in view
  ;; For now, we'll just render a simple grid of blocks
  
  ;; Render all loaded chunks
  (maphash (lambda (key chunk)
             (declare (ignore key))
             (render-chunk chunk))
           (game-state-world *game-state*)))

(defun render-chunk (chunk)
  "Render a single chunk"
  (let ((blocks (chunk-blocks chunk))
        (chunk-x (chunk-x chunk))
        (chunk-z (chunk-z chunk)))
    ;; Render all blocks in the chunk that are not air
    (loop for x from 0 below +chunk-size+
          do (loop for y from 0 below 256
                   do (loop for z from 0 below +chunk-size+
                            do (let ((block-id (aref blocks x y z)))
                                 (when (not (zerop block-id))
                                   (render-block (+ (* chunk-x +chunk-size+) x) y 
                                                 (+ (* chunk-z +chunk-size+) z) 
                                                 block-id))))))))

(defun render-block (x y z block-id)
  "Render a single block at the given position"
  (let ((block-type (gethash block-id *block-types-hash*)))
    (when block-type
      ;; In a real implementation, this would render the actual 3D block
      ;; with proper textures and lighting
      (gl:push-matrix)
      (gl:translate x y z)
      
      ;; Simple colored cube rendering for now
      (case block-id
        (1 (gl:color 0.0 1.0 0.0))   ; Grass
        (2 (gl:color 0.6 0.4 0.2))   ; Dirt
        (3 (gl:color 0.5 0.5 0.5))   ; Stone
        (4 (gl:color 0.8 0.6 0.2))   ; Wood
        (5 (gl:color 0.0 0.8 0.0))   ; Leaves
        (6 (gl:color 0.0 0.5 1.0))   ; Water
        (7 (gl:color 0.9 0.9 0.1))   ; Sand
        (otherwise (gl:color 0.5 0.5 0.5)))  ; Default color
        
      ;; Draw a cube
      (draw-cube)
      (gl:pop-matrix))))

(defun draw-cube ()
  "Draw a unit cube centered at the origin"
  (gl:begin :quads)
    ;; Front face
    (gl:vertex 0.5 0.5 0.5)
    (gl:vertex -0.5 0.5 0.5)
    (gl:vertex -0.5 -0.5 0.5)
    (gl:vertex 0.5 -0.5 0.5)
    
    ;; Back face
    (gl:vertex 0.5 0.5 -0.5)
    (gl:vertex 0.5 -0.5 -0.5)
    (gl:vertex -0.5 -0.5 -0.5)
    (gl:vertex -0.5 0.5 -0.5)
    
    ;; Top face
    (gl:vertex 0.5 0.5 0.5)
    (gl:vertex 0.5 0.5 -0.5)
    (gl:vertex -0.5 0.5 -0.5)
    (gl:vertex -0.5 0.5 0.5)
    
    ;; Bottom face
    (gl:vertex 0.5 -0.5 0.5)
    (gl:vertex -0.5 -0.5 0.5)
    (gl:vertex -0.5 -0.5 -0.5)
    (gl:vertex 0.5 -0.5 -0.5)
    
    ;; Right face
    (gl:vertex 0.5 0.5 0.5)
    (gl:vertex 0.5 0.5 -0.5)
    (gl:vertex 0.5 -0.5 -0.5)
    (gl:vertex 0.5 -0.5 0.5)
    
    ;; Left face
    (gl:vertex -0.5 0.5 0.5)
    (gl:vertex -0.5 -0.5 0.5)
    (gl:vertex -0.5 -0.5 -0.5)
    (gl:vertex -0.5 0.5 -0.5)
  (gl:end))

(defun render-ui ()
  "Render UI elements like inventory and health bar"
  ;; In a real implementation, this would render 2D UI elements
  ;; For now, we'll just print some debug info
  (format t "Time of day: ~a~%" (game-state-time-of-day *game-state*))
  (format t "Player position: (~a, ~a, ~a)~%" 
          (player-x (game-state-player *game-state*))
          (player-y (game-state-player *game-state*))
          (player-z (game-state-player *game-state*)))
  (format t "Player rotation: (~a, ~a)~%" 
          (player-rotation-x (game-state-player *game-state*))
          (player-rotation-y (game-state-player *game-state*)))
  (format t "Player health: ~a~%" (player-health (game-state-player *game-state*)))
  (format t "Player on ground: ~a~%" (player-on-ground (game-state-player *game-state*))))
