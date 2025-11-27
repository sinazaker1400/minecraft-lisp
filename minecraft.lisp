(defpackage #:minecraft-3d
  (:use #:cl #:lispbuilder-sdl)
  (:export #:start-game))
(in-package #:minecraft-3d)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defparameter *chunk-size-x* 16)
(defparameter *chunk-size-y* 16)
(defparameter *chunk-size-z* 16)
(defparameter *world-chunks* (make-hash-table :test 'equal))

(defstruct world-block
  type
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defstruct chunk
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum)
  (blocks (make-array (list *chunk-size-x* *chunk-size-y* *chunk-size-z*)
                      :initial-element nil
                      :element-type '(or symbol null))))

(defstruct game-player
  (x 8.0 :type single-float)
  (y 80.0 :type single-float)
  (z 8.0 :type single-float)
  (rot-x 0.0 :type single-float)
  (rot-y 0.0 :type single-float))

(defun initialize-randomness (seed)
  (let ((hash (sxhash seed)))
    (loop repeat (mod hash 10) do (random 1.0 *random-state*))))

(defun calculate-height (x z global_seed)
  (let* ((combined-input (sxhash (list x z global_seed)))
         (base_height 60)
         (height_variation 10)
         (deterministic_offset (mod combined-input height_variation)))
    (+ base_height deterministic_offset)))

(defun world-coords-to-chunk-coords (world-x world-y world-z)
  (values (floor world-x *chunk-size-x*)
          (floor world-y *chunk-size-y*)
          (floor world-z *chunk-size-z*)))

(defun chunk-and-local-to-world-coords (chunk-x chunk-y chunk-z local-x local-y local-z)
  (values (+ (* chunk-x *chunk-size-x*) local-x)
          (+ (* chunk-y *chunk-size-y*) local-y)
          (+ (* chunk-z *chunk-size-z*) local-z)))

(defun generate-chunk (chunk-x chunk-y chunk-z)
  (let ((chunk (make-chunk :x chunk-x :y chunk-y :z chunk-z)))
    (loop for local-x from 0 below *chunk-size-x* do
      (loop for local-y from 0 below *chunk-size-y* do
        (loop for local-z from 0 below *chunk-size-z* do
          (let* ((world-x (+ (* chunk-x *chunk-size-x*) local-x))
                 (world-y (+ (* chunk-y *chunk-size-y*) local-y))
                 (world-z (+ (* chunk-z *chunk-size-z*) local-z))
                 (global_seed 12345)
                 (ground-y (calculate-height world-x world-z global_seed))
                 (block-type
                   (cond
                     ((and (>= world-y (- ground-y 3)) (<= world-y ground-y))
                      (cond
                        ((= world-y (floor ground-y)) 'grass)
                        ((> world-y (- (floor ground-y) 2)) 'dirt)
                        (t 'stone)))
                     (t nil))))
            ;; This SETF line MUST be here, inside the let* and loop
            (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)))))
    chunk)) ; This is the end of the let block and the return value

(defun get-chunk (chunk-x chunk-y chunk-z)
  (or (gethash (list chunk-x chunk-y chunk-z) *world-chunks*)
      (setf (gethash (list chunk-x chunk-y chunk-z) *world-chunks*)
            (generate-chunk chunk-x chunk-y chunk-z))))

(defun get-block (world-x world-y world-z)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (aref (chunk-blocks chunk) local-x local-y local-z)))))

(defun set-block (world-x world-y world-z block-type)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)))))

(defun get-block-color (block-type)
  (case block-type
    (grass (values 0.2 0.7 0.2))
    (dirt (values 0.6 0.4 0.2))
    (stone (values 0.5 0.5 0.5))
    (wood (values 0.6 0.4 0.2))
    (otherwise (values 0.5 0.5 0.5))))

(defun draw-cube (x y z block-type)
  (multiple-value-bind (r g b) (get-block-color block-type)
    (gl:with-pushed-matrix
      (gl:translate x y z)
      (gl:color r g b)
      (gl:with-primitives :quads
        (gl:vertex -0.5 -0.5  0.5)
        (gl:vertex  0.5 -0.5  0.5)
        (gl:vertex  0.5  0.5  0.5)
        (gl:vertex -0.5  0.5  0.5)

        (gl:vertex -0.5 -0.5 -0.5)
        (gl:vertex -0.5  0.5 -0.5)
        (gl:vertex  0.5  0.5 -0.5)
        (gl:vertex  0.5 -0.5 -0.5)

        (gl:vertex -0.5  0.5 -0.5)
        (gl:vertex -0.5  0.5  0.5)
        (gl:vertex  0.5  0.5  0.5)
        (gl:vertex  0.5  0.5 -0.5)

        (gl:vertex -0.5 -0.5 -0.5)
        (gl:vertex  0.5 -0.5 -0.5)
        (gl:vertex  0.5 -0.5  0.5)
        (gl:vertex -0.5 -0.5  0.5)

        (gl:vertex  0.5 -0.5 -0.5)
        (gl:vertex  0.5 -0.5  0.5)
        (gl:vertex  0.5  0.5  0.5)
        (gl:vertex  0.5  0.5 -0.5)

        (gl:vertex -0.5 -0.5 -0.5)
        (gl:vertex -0.5  0.5 -0.5)
        (gl:vertex -0.5  0.5  0.5)
        (gl:vertex -0.5 -0.5  0.5)))))

(defun init-opengl (width height)
  (gl:viewport 0 0 width height)
  (gl:enable :depth-test)
  (gl:depth-func :less)
  (gl:clear-color 0.5 0.7 1.0 1.0))

(defun setup-opengl (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let* ((fovy 45.0)
         (aspect (/ width height))
         (near 0.1)
         (far 100.0)
         (f (/ 1.0 (tan (/ (* fovy 3.14159) 180.0 2.0)))))
    (gl:frustum (* (/ -1.0 aspect) f near)
                (* (/ 1.0 aspect) f near)
                (* -1.0 f near)
                (* 1.0 f near)
                near
                far))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at 12.0 70.0 12.0
               8.0  65.0 8.0
               0.0  1.0 0.0))

(defun render-world (player)
  (gl:clear-color 0.5 0.7 1.0 1.0)
  (gl:clear :color-buffer :depth-buffer)

  (gl:matrix-mode :modelview)
  (gl:load-identity)

  ;; This let* defines eye-x, eye-y, eye-z
  (let* ((eye-x (game-player-x player))
         (eye-y (game-player-y player))
         (eye-z (game-player-z player))
         (rot-x (game-player-rot-x player))
         (rot-y (game-player-rot-y player))
         (cos-pitch (float (cos rot-x) 0.0))
         (forward-x (* cos-pitch (float (cos rot-y) 0.0)))
         (forward-y (float (sin rot-x) 0.0))
         (forward-z (* cos-pitch (float (sin rot-y) 0.0)))
         (distance 1.0)
         (look-at-x (+ eye-x (* distance forward-x)))
         (look-at-y (+ eye-y (* distance forward-y)))
         (look-at-z (+ eye-z (* distance forward-z))))
    ;; glu:look-at uses the variables defined in the let*
    (glu:look-at eye-x eye-y eye-z
                 look-at-x look-at-y look-at-z
                 0.0 1.0 0.0)

    ;; The rendering loop starts here
    (let ((render-distance-xz 2)
          (render-distance-y  4))
      ;; This call to world-coords-to-chunk-coords MUST be here,
      ;; inside the let* where eye-x, eye-y, eye-z are defined
      (multiple-value-bind (player-chunk-x player-chunk-y player-chunk-z)
          (world-coords-to-chunk-coords eye-x eye-y eye-z) ; <-- This line is crucial
        ;; The nested loops for rendering chunks go here
        (loop for cx from (- player-chunk-x render-distance-xz) to (+ player-chunk-x render-distance-xz) do
          (loop for cy from (- player-chunk-y render-distance-y) to (+ player-chunk-y render-distance-y) do
            (loop for cz from (- player-chunk-z render-distance-xz) to (+ player-chunk-z render-distance-xz) do
              (let ((chunk (get-chunk cx cy cz)))
                (loop for local-x from 0 below *chunk-size-x* do
                  (loop for local-y from 0 below *chunk-size-y* do
                    (loop for local-z from 0 below *chunk-size-z* do
                      (let ((block-type (aref (chunk-blocks chunk) local-x local-y local-z)))
                        (when block-type
                          (multiple-value-bind (world-x world-y world-z)
                              (chunk-and-local-to-world-coords cx cy cz local-x local-y local-z)
                            (draw-cube world-x world-y world-z block-type)))))))))))))

  (gl:flush)))

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
    (when (sdl:key-down-p :sdl-key-space)
      (incf (game-player-y player) (float speed 0.0)))
    (when (sdl:key-down-p :sdl-key-lshift)
      (decf (game-player-y player) (float speed 0.0)))

    (when (sdl:key-down-p :sdl-key-left)
      (decf (game-player-rot-y player) (float 0.05 0.0)))
    (when (sdl:key-down-p :sdl-key-right)
      (incf (game-player-rot-y player) (float 0.05 0.0)))
    (when (sdl:key-down-p :sdl-key-up)
      (decf (game-player-rot-x player) (float 0.05 0.0)))
    (when (sdl:key-down-p :sdl-key-down)
      (incf (game-player-rot-x player) (float 0.05 0.0)))))

(defun start-game (&optional (seed 12345))
  (initialize-randomness seed)

  (sdl:with-init ()
    (sdl:window *window-width* *window-height*
                :flags '(sdl-cffi::sdl-opengl)
                :title-caption "Minecraft 3D Lisp"
                :icon-caption "Minecraft 3D Lisp")

    (init-opengl *window-width* *window-height*)
    (setup-opengl *window-width* *window-height*)

    (let ((player (make-game-player)))
      (render-world player)

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
