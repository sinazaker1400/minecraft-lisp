;; File: world-interaction.lisp
(defpackage #:minecraft-3d
  (:use #:cl #:lispbuilder-sdl) ; Use CL and lispbuilder-sdl, but NOT cl-opengl
  ;; Import specific functions from cl-opengl/cl-glu if needed, or use prefixes (gl:, glu:)
  ;; We will use the gl: and glu: prefixes in the code itself.
  (:export #:perform-raycast
           #:break-block
           #:place-block
           #:update-chunk-geometries))
(in-package #:minecraft-3d)

;; Define the raycast-result struct here or in defs.lisp
;; (defstruct raycast-result ...)

;; Function to perform the raycast from the player's viewpoint
;; This function will call the actual DDA algorithm function
(defun perform-raycast (player)
  "Performs a raycast from the player's viewpoint."
  (let ((start-x (game-player-x player))
        (start-y (game-player-y player))
        (start-z (game-player-z player))
        ;; Calculate the ray direction based on player's rotation
        (rot-x (game-player-rot-x player))
        (rot-y (game-player-rot-y player))
        (cos-pitch (float (cos rot-x) 0.0))
        (dir-x (* cos-pitch (float (cos rot-y) 0.0)))
        (dir-y (float (sin rot-x) 0.0))
        (dir-z (* cos-pitch (float (sin rot-y) 0.0))))
    ;; Call the core DDA raycasting function
    (raycast-block start-x start-y start-z dir-x dir-y dir-z)))

;; The core DDA raycasting function (previously named raycast-block)
;; This function performs the actual line traversal through the block grid.
;; It needs to be defined INSIDE the package where world-coords-to-chunk-coords, get-block, etc. are accessible.
;; The core DDA raycasting function (previously named raycast-block)
;; This function performs the actual line traversal through the block grid.
;; It needs to be defined INSIDE the package where world-coords-to-chunk-coords, get-block, etc. are accessible.
(defun raycast-block (start-x start-y start-z dir-x dir-y dir-z)
  "Cast a ray and return information about the first block face hit."
  ;; Normalize direction vector
  (let ((len (sqrt (+ (* dir-x dir-x) (* dir-y dir-y) (* dir-z dir-z)))))
    (if (plusp len) ; Check if the length is positive (valid direction)
        ;; If positive, normalize the direction vector inside a new let
        (let ((dir-x (/ dir-x len))
              (dir-y (/ dir-y len))
              (dir-z (/ dir-z len)))
          ;; Initialize DDA variables using the *normalized* direction
          (let ((map-x (floor start-x))
                (map-y (floor start-y))
                (map-z (floor start-z))
                ;; Calculate step direction (+1 or -1) based on normalized ray direction
                (step-x (if (>= dir-x 0) 1 -1))
                (step-y (if (>= dir-y 0) 1 -1))
                (step-z (if (>= dir-z 0) 1 -1))
                ;; Calculate Delta Distance (1 / abs(normalized_direction_component))
                ;; Handle zero direction components carefully (shouldn't happen after normalization unless component was tiny)
                (delta-x (if (zerop dir-x) most-positive-single-float (/ (abs dir-x))))
                (delta-y (if (zerop dir-y) most-positive-single-float (/ (abs dir-y))))
                (delta-z (if (zerop dir-z) most-positive-single-float (/ (abs dir-z))))
                ;; Calculate initial side distances based on normalized direction
                ;; Distance along ray to the next X, Y, Z grid line from the starting point
                (side-dist-x (if (>= dir-x 0) ; Ray going positive X
                                 (* (- (1+ map-x) start-x) delta-x)
                                 (* (- start-x map-x) delta-x)))
                (side-dist-y (if (>= dir-y 0) ; Ray going positive Y
                                 (* (- (1+ map-y) start-y) delta-y)
                                 (* (- start-y map-y) delta-y)))
                (side-dist-z (if (>= dir-z 0) ; Ray going positive Z
                                 (* (- (1+ map-z) start-z) delta-z)
                                 (* (- start-z map-z) delta-z)))
                ;; Track which side (X, Y, Z plane) was hit last during stepping
                (hit-side nil) ; Will be 'x, 'y, or 'z
                ;; Track total distance traveled along the ray
                (distance 0.0)
                ;; Maximum distance to travel
                (max-distance +max-ray-distance+)) ; Use your defined constant

            ;; Main DDA loop
            (loop
              ;; Check if we've traveled too far
              (when (> distance max-distance)
                ;; If no block was hit within the max distance, return a miss
                (return-from raycast-block (make-raycast-result))) ; Return default "miss" result

              ;; Determine which grid line (X, Y, or Z) we will hit next (smallest distance)
              (let ((closest-side (cond
                                    ;; Check X first, then Y, then Z
                                    ((and (<= side-dist-x side-dist-y) (<= side-dist-x side-dist-z)) 'x)
                                    ((<= side-dist-y side-dist-z) 'y)
                                    (t 'z))))

                ;; Move to the next grid line in the determined direction
                (case closest-side
                  (x (setf distance side-dist-x      ; Set distance to the X line we are hitting
                           side-dist-x (+ side-dist-x delta-x) ; Update distance to the *next* X line
                           map-x (+ map-x step-x)   ; Move the map coordinate to the next X block
                           hit-side 'x))            ; Record that we hit an X-side
                  (y (setf distance side-dist-y      ; Set distance to the Y line we are hitting
                           side-dist-y (+ side-dist-y delta-y) ; Update distance to the *next* Y line
                           map-y (+ map-y step-y)   ; Move the map coordinate to the next Y block
                           hit-side 'y))            ; Record that we hit a Y-side
                  (z (setf distance side-dist-z      ; Set distance to the Z line we are hitting
                           side-dist-z (+ side-dist-z delta-z) ; Update distance to the *next* Z line
                           map-z (+ map-z step-z)   ; Move the map coordinate to the next Z block
                           hit-side 'z)))           ; Record that we hit a Z-side

                ;; Check if a block exists at the current map coordinates
                (let ((block-type (get-block map-x map-y map-z)))
                  (when block-type ; If get-block returned a non-nil type (i.e., a solid block exists)
                    ;; Calculate the exact hit position along the ray
                    ;; The hit occurred at 'distance' units along the ray direction from the start
                    (let ((hit-x (+ start-x (* dir-x distance)))
                          (hit-y (+ start-y (* dir-y distance)))
                          (hit-z (+ start-z (* dir-z distance)))
                          ;; Determine the face based on which side was hit and the ray direction
                          ;; The face is the one facing *away* from the direction the ray came from
                          ;; when it hit the block boundary.
                          (hit-face (case hit-side
                                      (x (if (< dir-x 0) :right :left))  ; Hit X boundary -> face is Left/Right
                                      (y (if (< dir-y 0) :top :bottom)) ; Hit Y boundary -> face is Bottom/Top
                                      (z (if (< dir-z 0) :front :back))))) ; Hit Z boundary -> face is Back/Front
                      ;; Return the result structure containing hit information
                      (return-from raycast-block
                        (make-raycast-result
                         :hit-p t                 ; Indicate a hit occurred
                         :block-x map-x           ; World coordinates of the hit block
                         :block-y map-y
                         :block-z map-z
                         :face hit-face           ; The face ID of the hit block
                         :hit-x hit-x             ; Exact world coordinates of the hit point
                         :hit-y hit-y
                         :hit-z hit-z)))))))))
        ;; ELSE branch: If the direction vector was zero (invalid), return a miss
        (make-raycast-result))))

;; Function to break a block at the given world coordinates
;; This function updates the world data and marks chunks for geometry update
(defun break-block (world-x world-y world-z)
  "Removes a block at the specified world coordinates."
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (setf (aref (chunk-blocks chunk) local-x local-y local-z) nil) ; Set block to air/nil
        ;; Mark the chunk as needing geometry update
        (setf (chunk-needs-geometry-update chunk) t)
        ;; Mark neighboring chunks as potentially needing updates too
        (loop for dx from -1 to 1 do
          (loop for dy from -1 to 1 do
            (loop for dz from -1 to 1 do
              (unless (and (zerop dx) (zerop dy) (zerop dz))
                (let ((neighbor-chunk (get-chunk (+ chunk-x dx) (+ chunk-y dy) (+ chunk-z dz))))
                  (setf (chunk-needs-geometry-update neighbor-chunk) t))))))))))

;; Function to place a block at the given world coordinates
;; This function updates the world data and marks chunks for geometry update
(defun place-block (world-x world-y world-z block-type)
  "Places a block of the specified type at the given world coordinates."
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)
        ;; Mark the chunk as needing geometry update
        (setf (chunk-needs-geometry-update chunk) t)
        ;; Mark neighboring chunks as potentially needing updates too
        (loop for dx from -1 to 1 do
          (loop for dy from -1 to 1 do
            (loop for dz from -1 to 1 do
              (unless (and (zerop dx) (zerop dy) (zerop dz))
                (let ((neighbor-chunk (get-chunk (+ chunk-x dx) (+ chunk-y dy) (+ chunk-z dz))))
                  (setf (chunk-needs-geometry-update neighbor-chunk) t))))))))))

;; Function to update geometry for chunks marked as needing it
;; Call this periodically (e.g., once per frame after input handling but before rendering)
(defun update-chunk-geometries ()
  "Recalculates geometry for chunks marked as needing an update."
  ;; Iterate through the *world-chunks* hash table
  (maphash (lambda (key chunk)
             (declare (ignore key)) ; Ignore the chunk coordinates key for now
             (when (chunk-needs-geometry-update chunk)
               (calculate-chunk-geometry chunk) ; Use the function you already have
               (setf (chunk-needs-geometry-update chunk) nil))) ; Reset the flag after updating
           *world-chunks*))