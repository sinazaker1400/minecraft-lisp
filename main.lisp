(in-package #:minecraft-3d)

;; Update start-game to accept and use the seed and handle mouse input
(defun start-game (&optional (seed 12345))
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

    (let ((player (make-game-player))
          (prev-mouse-x nil)
          (prev-mouse-y nil))
      ;; Initialize mouse position tracking locally instead of globally
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
         (when (and prev-mouse-x prev-mouse-y)
           (let ((delta-x (- mouse-x prev-mouse-x))
                 (delta-y (- mouse-y prev-mouse-y)))
             (handle-mouse-look player delta-x delta-y)))
         (setf prev-mouse-x mouse-x)
         (setf prev-mouse-y mouse-y))

        ;; Mouse button down event for block interaction
        (:mouse-button-down-event (:button button :x mouse-x :y mouse-y)
         (format t "[v0] Mouse click detected: button=~A at (~A, ~A)~%" button mouse-x mouse-y)
         (handler-case
             (let ((raycast-result (perform-raycast player)))
               (format t "[v0] Raycast result hit: ~A~%" (raycast-result-hit-p raycast-result))
               (when (raycast-result-hit-p raycast-result)
                 (format t "[v0] Hit block at (~A, ~A, ~A) face: ~A~%" 
                         (raycast-result-block-x raycast-result)
                         (raycast-result-block-y raycast-result)
                         (raycast-result-block-z raycast-result)
                         (raycast-result-face raycast-result))
                 ;; More explicit button matching
                 (when (eq button 1)  ; Left click
                   (format t "[v0] Left click - breaking block~%" )
                   (break-block (raycast-result-block-x raycast-result)
                                (raycast-result-block-y raycast-result)
                                (raycast-result-block-z raycast-result)))
                 (when (eq button 3)  ; Right click
                   (format t "[v0] Right click - placing block~%" )
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
           (error (e) (format t "[v0] Error handling mouse click: ~A~%" e))))

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
         (sdl:update-display))))))
