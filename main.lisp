(in-package #:minecraft-3d)

;; Update start-game to accept and use the seed and handle mouse input

;; Update start-game to accept and use the seed and handle mouse input
(defun start-game (&optional (seed 14738273645))
  "Initialize and start the 3D game with a given seed"
  ;; Initialize the global random state with the seed
  (initialize-randomness seed)

  (sdl:with-init ()
    ;; IMPORTANT: Include sdl-cffi::sdl-opengl flag when creating the window
    (sdl:window *window-width* *window-height*
                :flags '(sdl-cffi::sdl-opengl)
                :title-caption "Minecraft 3D Lisp"
                :icon-caption "Minecraft 3D Lisp")

    ; Hide the mouse cursor during gameplay
    (sdl-cffi::sdl-show-cursor 0)

    ;; Initialize OpenGL settings
    (init-opengl *window-width* *window-height*)
    (setup-opengl *window-width* *window-height*)

    (let ((player (make-game-player)))
      ;; Initial render
      (render-world player)

      ;; Main game loop
      (sdl:with-events ()
        ;; Quit event
        (:quit-event ()
         ; Show cursor when quitting
         (sdl-cffi::sdl-show-cursor 1)
         t)

        ;; Video expose event
        (:video-expose-event ()
         (setup-opengl *window-width* *window-height*)
         (render-world player)
         (sdl:update-display))

        ;; Key down event
        (:key-down-event (:key key)
                         (case key
                           (:sdl-key-escape 
                            ; Show cursor and quit when ESC is pressed
                            (sdl-cffi::sdl-show-cursor 1)
                            (sdl:push-quit-event))))

        ;; Mouse motion event for mouse look - extract x and y and calculate delta
        (:mouse-motion-event (:x mouse-x :y mouse-y)

 (let* ((center-x (/ *window-width* 2))
        (center-y (/ *window-height* 2))

        (delta-x (- mouse-x center-x))
        (delta-y (- mouse-y center-y)))

   (handle-mouse-look player delta-x delta-y)

   ;; Warp mouse back to center every frame
   (sdl-cffi::sdl-warp-mouse
    center-x
    center-y)))

        ;; Mouse button down event for block interaction
        (:mouse-button-down-event (:button button :x mouse-x :y mouse-y)
         
         (handler-case
             (let ((raycast-result (perform-raycast player)))
               
               (when (raycast-result-hit-p raycast-result)
                 
                 ;; More explicit button matching
                 (when (eq button 1)  ; Left click
                   
                   (break-block (raycast-result-block-x raycast-result)
                                (raycast-result-block-y raycast-result)
                                (raycast-result-block-z raycast-result)))
                 (when (eq button 3)  ; Right click
                   
                   (let ((adjacent-x (raycast-result-block-x raycast-result))
                         (adjacent-y (raycast-result-block-y raycast-result))
                         (adjacent-z (raycast-result-block-z raycast-result)))
                     (case (raycast-result-face raycast-result)
                       (:top    (incf adjacent-y))
                       (:bottom (decf adjacent-y))
                       (:right  (incf adjacent-x))
                       (:left   (decf adjacent-x))
                       (:front  (incf adjacent-z))
                       (:back   (decf adjacent-z)))
                     (place-block adjacent-x adjacent-y adjacent-z 'grass)))))
           (error (e) )))

        ;; Idle event for continuous rendering and input
        (:idle ()
         (handle-input player)
         (update-chunk-geometries)
         ;; Update targeted block every frame for highlighting
         (let ((raycast-result (perform-raycast player)))
           (if (raycast-result-hit-p raycast-result)
               (setf *targeted-block* (list (raycast-result-block-x raycast-result)
                                           (raycast-result-block-y raycast-result)
                                           (raycast-result-block-z raycast-result)))
               (setf *targeted-block* nil)))
         (render-world player)
         (sdl:update-display)
         (format t "~&X=~,1f Y=~,1f Z=~,1f~%"
        (game-player-x player)
        (game-player-y player)
        (game-player-z player)))))))
