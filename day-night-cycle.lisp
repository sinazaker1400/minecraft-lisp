(in-package #:minecraft-3d)

;; Day/Night cycle system

(defun update-day-night-cycle (game-state delta-time)
  "Update the day/night cycle based on elapsed time"
  (let ((time-elapsed (/ delta-time +day-cycle-duration+)))
    (incf (game-state-time-of-day game-state) time-elapsed)
    ;; Wrap around after a full day
    (when (>= (game-state-time-of-day game-state) 1.0)
      (setf (game-state-time-of-day game-state) 
            (- (game-state-time-of-day game-state) 1.0)))))

(defun get-sky-color (game-state)
  "Get the sky color based on the time of day"
  (let ((time (game-state-time-of-day game-state)))
    ;; Normalize time to 0-1 range where 0.0 is noon, 0.25 is sunset, 0.5 is midnight, 0.75 is sunrise
    (let* ((normalized-time (* time 2))
           (clamped-time (if (> normalized-time 1.0) 
                            (- normalized-time 1.0) 
                            normalized-time)))
      (cond
        ;; Day time (0.0 to 0.5 in normalized time)
        ((< clamped-time 0.5)
         (list 0.5 0.7 1.0))  ; Light blue
        ;; Night time (0.5 to 1.0 in normalized time)
        (t
         (let ((night-factor (- clamped-time 0.5)))
           (list (* 0.1 (- 1.0 (* 2 night-factor)))  ; Reduces red at midnight
                 (* 0.1 (- 1.0 (* 2 night-factor)))  ; Reduces green at midnight
                 (* 0.3 (- 1.0 night-factor))))))))) ; Blue component

(defun get-ambient-light (game-state)
  "Get the ambient light level based on the time of day"
  (let ((time (game-state-time-of-day game-state)))
    ;; Map time where 0.0 and 1.0 are noon (brightest), 0.5 is midnight (darkest)
    (let ((day-factor (abs (- (* time 2) (floor (* time 2))))))  ; 0.0 to 1.0, 0.0 at noon/midnight, 1.0 at dawn/dusk
      (if (< day-factor 0.5)
          ;; Morning/evening - transitioning to/from dark
          (+ 0.1 (* 0.9 (- 0.5 day-factor) 2))  ; From 0.1 (dark) to 1.0 (bright)
          ;; Night time - darkest
          (+ 0.1 (* 0.9 (- day-factor 0.5) 2))))))  ; From 1.0 (bright) to 0.1 (dark)

(defun is-daytime (game-state)
  "Check if it's currently daytime"
  (let ((time (game-state-time-of-day game-state)))
    ;; Daytime is between 0.25 and 0.75 (6am to 6pm)
    (and (>= time 0.25) (< time 0.75))))

(defun is-nighttime (game-state)
  "Check if it's currently nighttime"
  (not (is-daytime game-state)))

(defun should-spawn-hostile-mobs (game-state)
  "Check if hostile mobs should spawn based on time and difficulty"
  (and (is-nighttime game-state)
       (not (eq (game-state-difficulty game-state) :peaceful))))

(defun get-sun-position (game-state)
  "Get the sun's position in the sky based on time of day"
  (let ((time (game-state-time-of-day game-state)))
    ;; Sun position: at noon (0.0) it's at the zenith, at midnight (0.5) it's at nadir
    (let ((sun-angle (* time 2 pi)))  ; Full rotation over the day
      (list (* 100 (cos sun-angle))    ; X position
            (* -100 (abs (sin sun-angle)))  ; Y position (never goes above horizon)
            (* 100 (sin sun-angle))))))  ; Z position

(defun get-moon-position (game-state)
  "Get the moon's position in the sky based on time of day"
  (let ((time (game-state-time-of-day game-state)))
    ;; Moon is opposite to sun
    (let ((moon-angle (+ (* time 2 pi) pi)))  ; Full rotation over the day, offset by 180 degrees
      (list (* 100 (cos moon-angle))    ; X position
            (* -100 (abs (sin moon-angle)))  ; Y position (never goes above horizon)
            (* 100 (sin moon-angle))))))  ; Z position

(defun calculate-light-level (game-state x y z)
  "Calculate the light level at a position based on time of day and nearby light sources"
  (let ((base-light (get-ambient-light game-state))
        (block-light 0.0))
    
    ;; Check for nearby light-emitting blocks (torches, glowstone, etc.)
    (dolist (light-source (game-state-active-light-sources game-state))
      (let ((dist (calculate-distance x y z
                                      (first light-source) 
                                      (second light-source) 
                                      (third light-source))))
        (when (< dist 10)  ; Light falloff distance
          (incf block-light (/ 1.0 (1+ dist))))))
    
    ;; Clamp to [0, 1] range
    (min 1.0 (+ base-light block-light))))

(defun update-mob-spawning (game-state)
  "Handle mob spawning based on the time of day"
  (when (should-spawn-hostile-mobs game-state)
    ;; Spawn zombies, skeletons, etc. in dark areas
    (when (< (random 1.0) 0.001)  ; 0.1% chance per frame to spawn
      (let* ((player (game-state-player game-state))
             (spawn-x (+ (player-x player) (- (random 40) 20)))
             (spawn-z (+ (player-z player) (- (random 40) 20)))
             (spawn-y (find-surface-y spawn-x spawn-z (game-state-world game-state))))
        ;; Only spawn if the area is dark enough
        (when (and (< (calculate-light-level game-state spawn-x spawn-y spawn-z) 0.2)
                   (> spawn-y 0))
          (spawn-entity game-state spawn-x spawn-y spawn-z :zombie))))))

(defun find-surface-y (x z world)
  "Find the surface Y coordinate at the given X, Z position"
  (loop for y from 255 downto 0
        when (block-exists-at world (floor x) y (floor z))
          return (1+ y)
        finally (return 0)))