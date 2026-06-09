(defpackage :minecraft-3d
  (:use :cl)
  (:import-from :cl-opengl
                #:clear-color
                #:enable
                #:depth-func
                #:cull-face
                #:front-face
                #:matrix-mode
                #:load-identity
                #:viewport
                #:clear
                #:begin
                #:vertex
                #:color
                #:normal
                #:end
                #:translate
                #:rotate
                #:scale
                #:push-matrix
                #:pop-matrix
                #:draw-arrays)
  (:import-from :cl-glu
                #:perspective)
  (:export
   ;; Window management
   #:initialize-window
   #:cleanup-window
   #:swap-buffers
   #:set-viewport
   #:setup-opengl
   
   ;; Game state
   #:*game-player*
   #:*world-seed*
   #:*running*
   #:*chunks*
   #:*last-frame-time*
   
   ;; Player
   #:game-player
   #:make-game-player
   #:player-pos
   #:player-vel
   #:player-yaw
   #:player-pitch
   
   ;; World
   #:chunk
   #:make-chunk
   #:get-block
   #:set-block
   #:generate-world
   #:get-or-create-chunk
   #:preload-world
   #:world->local
   
   ;; Input
   #:handle-events
   #:update-player-input
   #:get-delta-time
   
   ;; Rendering
   #:render-scene
   #:render-chunk
   
   ;; Interaction
   #:break-block
   #:place-block
   #:raycast-to-block
   
   ;; Main
   #:start-game))
