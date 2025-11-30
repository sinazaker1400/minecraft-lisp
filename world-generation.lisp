(in-package #:minecraft-3d)

;; Function to initialize randomness based on a seed (workaround for SBCL)
(defun initialize-randomness (seed)
  (let ((hash (sxhash seed)))
    (loop repeat (mod hash 10) do (random 1.0 *random-state*))))

;; Function to calculate height based on position and seed
(defun calculate-height (x z global_seed)
  (let* ((combined-input (sxhash (list x z global_seed)))
         (base_height 60)
         (height_variation 10)
         (deterministic_offset (mod combined-input height_variation)))
    (+ base_height deterministic_offset)))

;; Function to convert world coordinates (x, y, z) to chunk coordinates (cx, cy, cz)
(defun world-coords-to-chunk-coords (world-x world-y world-z)
  (values (floor world-x *chunk-size-x*)
          (floor world-y *chunk-size-y*)
          (floor world-z *chunk-size-z*)))

;; Function to convert chunk coordinates (cx, cy, cz) and local coordinates (lx, ly, lz) back to world coordinates
(defun chunk-and-local-to-world-coords (chunk-x chunk-y chunk-z local-x local-y local-z)
  (values (+ (* chunk-x *chunk-size-x*) local-x)
          (+ (* chunk-y *chunk-size-y*) local-y)
          (+ (* chunk-z *chunk-size-z*) local-z)))

;; --- NEW FUNCTIONS FOR FACE CULLING (and dependencies) ---

;; Function to check if a block exists and is non-air at world coordinates
(defun is-block-present (world-x world-y world-z)
  "Checks if a block exists and is non-air at the given world coordinates."
  ;; Use get-block defined earlier
  (not (null (get-block world-x world-y world-z))))

;; Function to calculate geometry for a single face
(defun calculate-face-geometry (world-x world-y world-z face-direction block-type)
  "Calculates the vertices and normals for a specific face of a block."
  (let ((half-size 0.5))
    ;; Get color components using multiple-value-bind
    (multiple-value-bind (r g b) (get-block-color block-type) ; Assumes get-block-color is available
      (case face-direction
        (:top
         `((,(- world-x half-size) ,(+ world-y half-size) ,(- world-z half-size) 0 1 0 ,r ,g ,b)
           (,(+ world-x half-size) ,(+ world-y half-size) ,(- world-z half-size) 0 1 0 ,r ,g ,b)
           (,(+ world-x half-size) ,(+ world-y half-size) ,(+ world-z half-size) 0 1 0 ,r ,g ,b)
           (,(- world-x half-size) ,(+ world-y half-size) ,(+ world-z half-size) 0 1 0 ,r ,g ,b)))
        (:bottom
         `((,(- world-x half-size) ,(- world-y half-size) ,(- world-z half-size) 0 -1 0 ,r ,g ,b)
           (,(+ world-x half-size) ,(- world-y half-size) ,(- world-z half-size) 0 -1 0 ,r ,g ,b)
           (,(+ world-x half-size) ,(- world-y half-size) ,(+ world-z half-size) 0 -1 0 ,r ,g ,b)
           (,(- world-x half-size) ,(- world-y half-size) ,(+ world-z half-size) 0 -1 0 ,r ,g ,b)))
        (:right ; X+
         `((,(+ world-x half-size) ,(- world-y half-size) ,(- world-z half-size) 1 0 0 ,r ,g ,b)
           (,(+ world-x half-size) ,(- world-y half-size) ,(+ world-z half-size) 1 0 0 ,r ,g ,b)
           (,(+ world-x half-size) ,(+ world-y half-size) ,(+ world-z half-size) 1 0 0 ,r ,g ,b)
           (,(+ world-x half-size) ,(+ world-y half-size) ,(- world-z half-size) 1 0 0 ,r ,g ,b)))
        (:left ; X-
         `((,(- world-x half-size) ,(- world-y half-size) ,(- world-z half-size) -1 0 0 ,r ,g ,b)
           (,(- world-x half-size) ,(+ world-y half-size) ,(- world-z half-size) -1 0 0 ,r ,g ,b)
           (,(- world-x half-size) ,(+ world-y half-size) ,(+ world-z half-size) -1 0 0 ,r ,g ,b)
           (,(- world-x half-size) ,(- world-y half-size) ,(+ world-z half-size) -1 0 0 ,r ,g ,b)))
        (:front ; Z+
         `((,(- world-x half-size) ,(- world-y half-size) ,(+ world-z half-size) 0 0 1 ,r ,g ,b)
           (,(+ world-x half-size) ,(- world-y half-size) ,(+ world-z half-size) 0 0 1 ,r ,g ,b)
           (,(+ world-x half-size) ,(+ world-y half-size) ,(+ world-z half-size) 0 0 1 ,r ,g ,b)
           (,(- world-x half-size) ,(+ world-y half-size) ,(+ world-z half-size) 0 0 1 ,r ,g ,b)))
        (:back ; Z-
         `((,(- world-x half-size) ,(- world-y half-size) ,(- world-z half-size) 0 0 -1 ,r ,g ,b)
           (,(- world-x half-size) ,(+ world-y half-size) ,(- world-z half-size) 0 0 -1 ,r ,g ,b)
           (,(+ world-x half-size) ,(+ world-y half-size) ,(- world-z half-size) 0 0 -1 ,r ,g ,b)
           (,(+ world-x half-size) ,(- world-y half-size) ,(- world-z half-size) 0 0 -1 ,r ,g ,b)))
        (otherwise '()))))) ; Return empty list for unknown face

;; Function to calculate visible faces for a chunk
;; Note: This function needs to be defined *before* generate-chunk if generate-chunk calls it.
;; However, generate-chunk needs to be defined *before* get-chunk.
;; To resolve this, we can declare calculate-chunk-geometry first.
(declaim (ftype (function (chunk) t) calculate-chunk-geometry))

;; Function to get a block type at world coordinates
;; This function needs to call get-chunk, so get-chunk must be declared first.
(declaim (ftype (function (fixnum fixnum fixnum) chunk) get-chunk))

(defun get-block (world-x world-y world-z)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (aref (chunk-blocks chunk) local-x local-y local-z)))))

;; Function to set a block type at world coordinates
(defun set-block (world-x world-y world-z block-type)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world-coords-to-chunk-coords world-x world-y world-z)
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (multiple-value-bind (local-x local-y local-z)
          (values (mod world-x *chunk-size-x*)
                  (mod world-y *chunk-size-y*)
                  (mod world-z *chunk-size-z*))
        (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)))))

;; Function to get a chunk (or generate it)
;; Declare generate-chunk first as get-chunk calls it.
(declaim (ftype (function (fixnum fixnum fixnum) chunk) generate-chunk))

(defun get-chunk (chunk-x chunk-y chunk-z)
  (or (gethash (list chunk-x chunk-y chunk-z) *world-chunks*)
      (setf (gethash (list chunk-x chunk-y chunk-z) *world-chunks*)
            (generate-chunk chunk-x chunk-y chunk-z))))

;; Function to generate a chunk section - updated to calculate geometry
;; This function is defined *before* calculate-chunk-geometry so it can call it.
(defun generate-chunk (chunk-x chunk-y chunk-z)
  (let ((chunk (make-chunk :x chunk-x :y chunk-y :z chunk-z)))
    ;; Fill the blocks array (existing logic)
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
            (setf (aref (chunk-blocks chunk) local-x local-y local-z) block-type)))))

    ;; DON'T calculate geometry here - just return the chunk
    chunk)) ; Return the chunk after both filling blocks and calculating geometry

;; Function to calculate visible faces for a chunk (defined AFTER generate-chunk)
;; Now that generate-chunk is defined, we can define this.
(defun calculate-chunk-geometry (chunk)
  "Calculates the visible faces for a chunk and stores them in the chunk structure."
  (let ((geometry-list '()))
    (loop for local-x from 0 below *chunk-size-x* do
      (loop for local-y from 0 below *chunk-size-y* do
        (loop for local-z from 0 below *chunk-size-z* do
          (let ((block-type (aref (chunk-blocks chunk) local-x local-y local-z)))
            (when block-type ; Only process non-air blocks
              ;; Convert local coordinates to world coordinates for neighbor checks
              (let ((world-x (+ (* (chunk-x chunk) *chunk-size-x*) local-x))
                    (world-y (+ (* (chunk-y chunk) *chunk-size-y*) local-y))
                    (world-z (+ (* (chunk-z chunk) *chunk-size-z*) local-z)))
                ;; Check each neighbor direction
                ;; Up (Y+)
                (unless (is-block-present world-x (1+ world-y) world-z)
                  (push (calculate-face-geometry world-x world-y world-z :top block-type) geometry-list))
                ;; Down (Y-)
                (unless (is-block-present world-x (1- world-y) world-z)
                  (push (calculate-face-geometry world-x world-y world-z :bottom block-type) geometry-list))
                ;; East (X+)
                (unless (is-block-present (1+ world-x) world-y world-z)
                  (push (calculate-face-geometry world-x world-y world-z :right block-type) geometry-list))
                ;; West (X-)
                (unless (is-block-present (1- world-x) world-y world-z)
                  (push (calculate-face-geometry world-x world-y world-z :left block-type) geometry-list))
                ;; South (Z+)
                (unless (is-block-present world-x world-y (1+ world-z))
                  (push (calculate-face-geometry world-x world-y world-z :front block-type) geometry-list))
                ;; North (Z-)
                (unless (is-block-present world-x world-y (1- world-z))
                  (push (calculate-face-geometry world-x world-y world-z :back block-type) geometry-list))))))))
    ;; Store the calculated geometry in the chunk structure
    (setf (chunk-visible-faces-geometry chunk) geometry-list
          (chunk-needs-geometry-update chunk) nil))) ; Mark as updated