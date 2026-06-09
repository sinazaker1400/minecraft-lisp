(in-package :minecraft-3d)

(defvar *game-player* nil)
(defvar *world-seed* 12345)
(defvar *running* t)
(defvar *chunks* (make-hash-table :test #'equal))

(defun start-game (&optional (seed 12345) (width 1280) (height 720))
  "Start the Minecraft game"
  (setf *world-seed* seed)
  (setf *running* t)
  (setf *game-player* (make-game-player))
  
  (sdl2:with-init (:video)
    (initialize-window width height)
    
    ;; Preload initial chunks around player
    (preload-world *game-player* 3)
    
    ;; Main game loop
    (loop while *running* do
      (handle-events)
      (update-player-input *game-player*)
      (render-scene *game-player* width height)
      (swap-buffers)
      
      ;; Cap framerate at ~60 FPS
      (sdl2:delay 16))
    
    ;; Cleanup
    (cleanup-window)))

(defun handle-events ()
  "Handle SDL events"
  (sdl2:with-event-loop (:one-shot-p t)
    (:quit ()
      (setf *running* nil))
    
    (:keydown (:keysym keysym)
      (when (sdl2:scancode= (sdl2:scancode-value :scancode-escape)
                            (sdl2:scancode keysym))
        (setf *running* nil)))
    
    (:mousebuttondown (:button button :x x :y y)
      (cond
        ((= button 1) ; Left click - break block
         (when-let ((block-pos (raycast-to-block *game-player* 100)))
           (destructuring-bind (bx by bz) block-pos
             (break-block bx by bz))))
        ((= button 3) ; Right click - place block
         (when-let ((block-pos (raycast-to-block *game-player* 100)))
           (destructuring-bind (bx by bz) block-pos
             (place-block (+ bx 1) by bz 1))))))))

(defun get-delta-time ()
  "Get delta time in milliseconds since last frame"
  (let ((current-time (sdl2:get-ticks)))
    (prog1
        (- current-time *last-frame-time*)
      (setf *last-frame-time* current-time))))

(defun update-player-input (player)
  "Update player movement based on input"
  (let ((dt (/ (get-delta-time) 1000.0)))
    (declare (ignore dt))
    ;; For now, just a placeholder
    ;; Add WASD movement here when ready
    ))
