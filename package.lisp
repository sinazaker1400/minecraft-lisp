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
    #:draw-arrays
    #:with-primitives)
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
    #:game-player-x
    #:game-player-y
    #:game-player-z
    #:game-player-rot-x
    #:game-player-rot-y
    #:game-player-vel-y
    ;; World
    #:chunk
    #:make-chunk
    #:chunk-blocks
    #:chunk-x
    #:chunk-z
    #:chunk-needs-update
    #:get-block
    #:set-block
    #:generate-chunk-terrain
    #:get-or-create-chunk
    #:preload-world
    #:world->local
    #:*minecraft-world*
    #:*chunk-size*
    #:*chunk-height*
    ;; Input
    #:handle-events
    #:update-player-input
    #:handle-mouse-input
    #:get-delta-time
    ;; Rendering
    #:render-scene
    #:render-chunk
    #:setup-camera
    ;; Interaction
    #:break-block
    #:place-block
    #:raycast-from-player
    ;; Main
    #:start-game
    #:*window*
    #:*gl-context*))
