;; File: main.lisp
(in-package #:minecraft-3d)

;; Update start-game to accept and use the seed and handle mouse input
(defun start-game (&optional (seed 12345)) ; Add an optional seed parameter
  "Initialize and start the 3D game with a given seed"
  ;; Initialize the global random state with the seed using our workaround
  (initialize-randomness seed)

  (sdl:with-init ()
    ;; IMPORTANT: Include sdl-cffi::sdl-opengl flag when creating the window
    (sdl:window *window-width* *window-height*
                :flags '(sdl-cffi::sdl-opengl) ; This flag is crucial for OpenGL rendering
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
        ;; Quit event (e.g., closing the window)
        (:quit-event () t)

        ;; Video expose event (e.g., window was minimized/maximized or uncovered)
        (:video-expose-event ()
         (setup-opengl *window-width* *window-height*)
         (render-world player)
         (sdl:update-display))

        ;; Key down event (for movement, quitting)
        (:key-down-event (:key key)
                         (case key
                           (:sdl-key-escape (sdl:push-quit-event))))

        ;; NEW: Mouse button down event (for block interaction)
        (:mouse-button-down-event (:button button :x mouse-x :y mouse-y)
         ;; Perform raycast based on the player's current state when a mouse button is pressed
         (let ((raycast-result (perform-raycast player))) ; Use the function that just does the raycast
           ;; Check if the raycast hit a block
           (when (raycast-result-hit-p raycast-result)
             ;; Determine the action based on which mouse button was pressed
             (case button
               (:sdl-button-left ; Left click: Break the block
                (break-block (raycast-result-block-x raycast-result)
                             (raycast-result-block-y raycast-result)
                             (raycast-result-block-z raycast-result))
                (format t "Broke block at (~A, ~A, ~A)~%"
                        (raycast-result-block-x raycast-result)
                        (raycast-result-block-y raycast-result)
                        (raycast-result-block-z raycast-result)))
               (:sdl-button-right ; Right click: Place a block adjacent to the hit face
                (let ((adjacent-x (raycast-result-block-x raycast-result))
                      (adjacent-y (raycast-result-block-y raycast-result))
                      (adjacent-z (raycast-result-block-z raycast-result)))
                  ;; Calculate the coordinates of the block adjacent to the hit face
                  ;; The face information tells us which direction to offset from the hit block
                  (case (raycast-result-face raycast-result)
                    (:top    (incf adjacent-y))    ; Place block above the top face
                    (:bottom (decf adjacent-y))    ; Place block below the bottom face
                    (:right  (incf adjacent-x))    ; Place block to the right of the right face
                    (:left   (decf adjacent-x))    ; Place block to the left of the left face
                    (:front  (incf adjacent-z))    ; Place block in front of the front face (positive Z)
                    (:back   (decf adjacent-z)))   ; Place block behind the back face (negative Z)
                  ;; Place a block (e.g., grass) at the calculated adjacent coordinates
                  (place-block adjacent-x adjacent-y adjacent-z 'grass)
                  (format t "Placed block at (~A, ~A, ~A)~%" adjacent-x adjacent-y adjacent-z)))))))

        ;; Idle event (runs continuously when no other events are pending)
        ;; This is where we handle continuous input (like holding down keys) and rendering
        (:idle ()
         (handle-input player) ; Handle keyboard movement/rotation (WASD, Space, LShift, Arrow Keys)
         ;; Update geometries for chunks that were modified (e.g., by break-block or place-block)
         ;; This checks the chunk-needs-geometry-update flag for all loaded chunks
         (update-chunk-geometries)
         ;; Render the world from the player's perspective
         (render-world player)
         ;; Update the display to show the rendered frame
         (sdl:update-display))))))