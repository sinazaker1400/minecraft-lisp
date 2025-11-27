(defpackage #:minecraft-3d
  (:use #:cl #:lispbuilder-sdl) ; Use CL and lispbuilder-sdl
  ;; We will use gl: and glu: prefixes explicitly in the code
  ;; This avoids needing to import and potentially conflict with 'color'
  (:export #:start-game))
(in-package #:minecraft-3d)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defstruct world-block
  "A block in the world"
  type
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defstruct world-chunk
  "A 16x16x16 chunk of blocks"
  (x 0 :type fixnum)
  (z 0 :type fixnum)
  (blocks (make-array '(16 128 16) :initial-element nil)))

(defstruct game-player
  (x 8.0 :type single-float)
  (y 80.0 :type single-float)  ; Start above ground
  (z 8.0 :type single-float)
  (rot-x 0.0 :type single-float)  ; Looking direction (pitch)
  (rot-y 0.0 :type single-float)) ; Looking direction (yaw)

;; Function to initialize randomness based on a seed
;; This is a workaround for SBCL's seeding mechanism.
(defun initialize-randomness (seed)
  "Initialize the global random state using the seed."
  ;; This is a workaround to make the global random state depend on the seed.
  ;; SBCL's make-random-state doesn't take an integer seed directly.
  ;; We use the seed to influence the state by calling random a few times.
  (let ((hash (sxhash seed)))
    ;; Stir the global *random-state* using the hash
    ;; This makes the subsequent sequence depend on the initial 'seed' value.
    (loop repeat (mod hash 10) do (random 1.0 *random-state*))
    ;; The global *random-state* is now initialized in a way influenced by the seed.
    ;; For per-coordinate deterministic generation, calculate-height will use
    ;; the *current* global state but with a deterministic input (hash of x, z, seed).
    ))

;; Function to calculate height based on position and seed
(defun calculate-height (x z global_seed)
  "Calculate a height value based on x, z, and the global seed."
  ;; Combine x, z, and global_seed into a single value for hashing
  (let* ((combined-input (sxhash (list x z global_seed)))
         ;; Use the hash value itself, modulo a range, added to a base height.
         ;; This is very basic but deterministic.
         (base_height 60)
         (height_variation 10)
         (deterministic_offset (mod combined-input height_variation)))
    (+ base_height deterministic_offset)))

(defun get-block-color (block-type)
  "Get color for a block type as RGB values"
  (case block-type
    (grass (values 0.2 0.7 0.2))  ; Forest green
    (dirt (values 0.6 0.4 0.2))   ; Saddle brown approximation
    (stone (values 0.5 0.5 0.5))  ; Gray
    (wood (values 0.6 0.4 0.2))   ; Saddle brown approximation (or use wood colors)
    (otherwise (values 0.5 0.5 0.5)))) ; Gray

(defun draw-cube (x y z block-type)
  "Draw a cube at position (x, y, z) using OpenGL"
  (multiple-value-bind (r g b) (get-block-color block-type)
    (gl:with-pushed-matrix
      (gl:translate x y z) ; Move to the block's position
      (gl:color r g b)     ; Set the color for this block using gl:color

      ;; Draw the 6 faces of a cube using quads
      (gl:with-primitives :quads
        ;; Front face (z = 0.5)
        (gl:vertex -0.5 -0.5  0.5)
        (gl:vertex  0.5 -0.5  0.5)
        (gl:vertex  0.5  0.5  0.5)
        (gl:vertex -0.5  0.5  0.5)

        ;; Back face (z = -0.5)
        (gl:vertex -0.5 -0.5 -0.5)
        (gl:vertex -0.5  0.5 -0.5)
        (gl:vertex  0.5  0.5 -0.5)
        (gl:vertex  0.5 -0.5 -0.5)

        ;; Top face (y = 0.5)
        (gl:vertex -0.5  0.5 -0.5)
        (gl:vertex -0.5  0.5  0.5)
        (gl:vertex  0.5  0.5  0.5)
        (gl:vertex  0.5  0.5 -0.5)

        ;; Bottom face (y = -0.5)
        (gl:vertex -0.5 -0.5 -0.5)
        (gl:vertex  0.5 -0.5 -0.5)
        (gl:vertex  0.5 -0.5  0.5)
        (gl:vertex -0.5 -0.5  0.5)

        ;; Right face (x = 0.5)
        (gl:vertex  0.5 -0.5 -0.5)
        (gl:vertex  0.5 -0.5  0.5)
        (gl:vertex  0.5  0.5  0.5)
        (gl:vertex  0.5  0.5 -0.5)

        ;; Left face (x = -0.5)
        (gl:vertex -0.5 -0.5 -0.5)
        (gl:vertex -0.5  0.5 -0.5)
        (gl:vertex -0.5  0.5  0.5)
        (gl:vertex -0.5 -0.5  0.5)))))

(defun init-opengl (width height)
  "Basic OpenGL setup"
  (gl:viewport 0 0 width height)
  (gl:enable :depth-test) ; Enable depth testing for 3D
  (gl:depth-func :less)
  (gl:clear-color 0.5 0.7 1.0 1.0)) ; Sky blue background

(defun setup-opengl (width height)
  "Setup 3D perspective projection and initial modelview"
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; Set up a perspective projection manually using raw OpenGL calls
  (let* ((fovy 45.0) ; Field of view in degrees
         (aspect (/ width height))
         (near 0.1)
         (far 100.0)
         (f (/ 1.0 (tan (/ (* fovy 3.14159) 180.0 2.0)))))
    (gl:frustum (* (/ -1.0 aspect) f near) ; left
                (* (/ 1.0 aspect) f near)  ; right
                (* -1.0 f near)            ; bottom
                (* 1.0 f near)             ; top
                near                       ; near
                far))                      ; far

  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Set initial camera position to be slightly offset from the player, looking towards them
  ;; This should provide a better initial view of the surrounding blocks
  (glu:look-at 12.0 70.0 12.0   ; Eye position (e.g., back and slightly above player)
               8.0  65.0 8.0    ; Look-at point (player's head level)
               0.0  1.0  0.0))  ; Up vector

(defun render-world (player)
  "Render the world from the player's perspective using OpenGL"
  ;; Explicitly set the clear color again just before clearing (good practice)
  (gl:clear-color 0.5 0.7 1.0 1.0) ; Sky blue background
  (gl:clear :color-buffer :depth-buffer) ; Clear both color and depth buffers

  ;; Set the camera based on player position and rotation
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  ;; Calculate the look-at point based on eye position and rotation (pitch/yaw)
  (let* ((eye-x (game-player-x player))
         (eye-y (game-player-y player))
         (eye-z (game-player-z player))
         (rot-x (game-player-rot-x player)) ; Pitch (up/down look)
         (rot-y (game-player-rot-y player)) ; Yaw (left/right look)
         (cos-pitch (float (cos rot-x) 0.0))
         (forward-x (* cos-pitch (float (cos rot-y) 0.0)))
         (forward-y (float (sin rot-x) 0.0)) ; Y component comes directly from sin(pitch)
         (forward-z (* cos-pitch (float (sin rot-y) 0.0)))
         (distance 1.0)
         (look-at-x (+ eye-x (* distance forward-x))) ; Apply distance scaling
         (look-at-y (+ eye-y (* distance forward-y)))
         (look-at-z (+ eye-z (* distance forward-z))))

    ;; Use glu:look-at with the calculated eye and look-at points
    (glu:look-at eye-x eye-y eye-z         ; Eye position (player's x, y, z)
                 look-at-x look-at-y look-at-z ; Look-at point (calculated based on rotation)
                 0.0 1.0 0.0))             ; Up vector (world up is positive Y)

  ;; --- Generate World Blocks using Seeded Randomness ---
  ;; Define the size of the area to render relative to the player
  (let ((render-range 15) ; Adjust size as needed
        (global_seed 12345)) ; Use the same seed for consistency
    ;; Loop to draw the terrain
    (loop for bx from (floor (- (game-player-x player) render-range)) to (floor (+ (game-player-x player) render-range)) do
      (loop for bz from (floor (- (game-player-z player) render-range)) to (floor (+ (game-player-z player) render-range)) do
        (let* ((ground-y (calculate-height bx bz global_seed)) ; Use the global seed
               (int-ground-y (floor ground-y)))
          ;; Draw a column of blocks at this x, z based on the calculated height
          (loop for by from (- int-ground-y 3) to int-ground-y do
            (let ((block-type ; Correctly bind block-type here
                   (cond
                     ;; Top block is grass
                     ((= by int-ground-y) 'grass)
                     ;; Blocks just below ground are dirt
                     ((> by (- int-ground-y 2)) 'dirt)
                     ;; Blocks further down are stone
                     (t 'stone))))
              (draw-cube bx by bz block-type) ; Use the correctly bound block-type, and renamed loop vars
              ))))))

  ;; Ensure all OpenGL commands are processed
  (gl:flush))

(defun handle-input (player)
  "Handle player input for 3D movement and rotation"
  (let ((speed 1.0))
    ;; Movement (WASD keys)
    ;; Convert results of trig functions and PI to single-float
    (when (sdl:key-down-p :sdl-key-w)
      (incf (game-player-x player)
            (float (* speed (cos (game-player-rot-y player))) 0.0))
      (incf (game-player-z player)
            (float (* speed (sin (game-player-rot-y player))) 0.0)))
    (when (sdl:key-down-p :sdl-key-s)
      (decf (game-player-x player)
            (float (* speed (cos (game-player-rot-y player))) 0.0))
      (decf (game-player-z player)
            (float (* speed (sin (game-player-rot-y player))) 0.0)))
    (when (sdl:key-down-p :sdl-key-a)
      (decf (game-player-x player)
            (float (* speed (cos (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0))
      (decf (game-player-z player)
            (float (* speed (sin (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0)))
    (when (sdl:key-down-p :sdl-key-d)
      (incf (game-player-x player)
            (float (* speed (cos (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0))
      (incf (game-player-z player)
            (float (* speed (sin (+ (float (game-player-rot-y player) 0.0) (/ (float pi 0.0) 2.0)))) 0.0)))
    ;; Vertical movement (Space and LShift)
    (when (sdl:key-down-p :sdl-key-space)
      (incf (game-player-y player) (float speed 0.0)))
    (when (sdl:key-down-p :sdl-key-lshift)
      (decf (game-player-y player) (float speed 0.0)))

    ;; Rotation (arrow keys for simplicity, mouse look is more complex)
    (when (sdl:key-down-p :sdl-key-left)
      (decf (game-player-rot-y player) (float 0.05 0.0))) ; Turn left (yaw)
    (when (sdl:key-down-p :sdl-key-right)
      (incf (game-player-rot-y player) (float 0.05 0.0))) ; Turn right (yaw)
    (when (sdl:key-down-p :sdl-key-up)
      (decf (game-player-rot-x player) (float 0.05 0.0))) ; Look up (pitch)
    (when (sdl:key-down-p :sdl-key-down)
      (incf (game-player-rot-x player) (float 0.05 0.0))) ; Look down (pitch)
    ))

;; Update start-game to accept and use the seed
(defun start-game (&optional (seed 12345)) ; Add an optional seed parameter
  "Initialize and start the 3D game with a given seed"
  ;; Initialize the global random state with the seed using our workaround
  (initialize-randomness seed)

  (sdl:with-init ()
    (sdl:window *window-width* *window-height*
                :flags '(sdl-cffi::sdl-opengl)
                :title-caption "Minecraft 3D Lisp"
                :icon-caption "Minecraft 3D Lisp")

    ;; Initialize OpenGL settings
    (init-opengl *window-width* *window-height*)
    (setup-opengl *window-width* *window-height*)

    (let ((player (make-game-player)))
      ;; Initial render
      (render-world player)

      ;; Main game loop
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event ()
         (setup-opengl *window-width* *window-height*)
         (render-world player)
         (sdl:update-display))
        (:key-down-event (:key key)
                         (case key
                           (:sdl-key-escape (sdl:push-quit-event))))
        (:idle ()
               (handle-input player)
               (render-world player)
               (sdl:update-display))))))
