(in-package #:minecraft-3d)

;; Biome and world generation system with seed-based terrain

(defun generate-terrain-for-chunk (chunk-x chunk-z seed)
  "Generate terrain for a chunk based on the seed and biome"
  (let* ((world (make-hash-table :test 'equal))
         (chunk (make-chunk :x chunk-x :z chunk-z
                            :blocks (make-array (list +chunk-size+ 256 +chunk-size+)
                                                :initial-element 0)))
         (blocks (chunk-blocks chunk))
         (biome (get-biome-for-position (* chunk-x +chunk-size+) (* chunk-z +chunk-size+) seed)))
    
    ;; Set the random state based on the seed and chunk position for consistent generation
    (let ((*random-state* (make-random-state)))
      ;; Initialize random state with a combination of seed and chunk position
      (loop for i from 0 below (mod (+ seed chunk-x chunk-z) 1000)
            do (random 1000000))
      
      ;; Generate the terrain heightmap
      (loop for local-x from 0 below +chunk-size+
            do (loop for local-z from 0 below +chunk-size+
                     do (let* ((world-x (+ (* chunk-x +chunk-size+) local-x))
                               (world-z (+ (* chunk-z +chunk-size+) local-z))
                               (height (calculate-terrain-height world-x world-z seed biome)))
                          
                          ;; Fill the column from bedrock to the calculated height
                          (loop for y from 0 to (min height 255)
                                do (let ((block-id (determine-block-at-height y height biome)))
                                     (setf (aref blocks local-x y local-z) block-id))))))
      
      ;; Add surface features based on biome
      (add-surface-features chunk biome seed)
      (add-underground-features chunk biome seed)
      
      chunk)))

(defun get-biome-for-position (x z seed)
  "Determine the biome at the given position using Perlin noise"
  (let* ((biome-noise-x (/ x 1000.0))  ; Scale down for biome-sized areas
         (biome-noise-z (/ z 1000.0))
         (temperature (+ 0.5 (* 0.5 (simplex-noise:noise (* seed 2) biome-noise-x biome-noise-z))))
         (humidity (+ 0.5 (* 0.5 (simplex-noise:noise (+ seed 100) biome-noise-z biome-noise-x)))))
    
    ;; Determine biome based on temperature and humidity
    (cond
      ((and (>= temperature 0.7) (>= humidity 0.6)) *biome-forest*)  ; Forest
      ((and (>= temperature 0.6) (< humidity 0.4)) *biome-plains*)   ; Plains
      ((and (> temperature 0.8) (< humidity 0.3)) *biome-desert*)    ; Desert
      ((< temperature 0.3) *biome-forest*)                           ; Cold forest
      (t *biome-plains*))))                                          ; Default to plains

(defun calculate-terrain-height (x z seed biome)
  "Calculate the terrain height at the given position"
  (let* ((scale 0.01)  ; Smaller scale for more detailed terrain
         (height1 (* 40 (simplex-noise:noise seed (* x scale) (* z scale))))
         (height2 (* 20 (simplex-noise:noise (+ seed 100) (* x (* scale 2)) (* z (* scale 2)))))
         (height3 (* 10 (simplex-noise:noise (+ seed 200) (* x (* scale 4)) (* z (* scale 4)))))
         (base-height (+ 60 height1 height2 height3))
         (clamped-height (max 5 (min 120 base-height))))
    (floor clamped-height)))

(defun determine-block-at-height (y surface-height biome)
  "Determine which block type to place at the given height"
  (let ((water-level (biome-water-level biome)))
    (cond
      ;; Bedrock at the bottom
      ((= y 0) 3)  ; Stone (bedrock)
      ;; Stone underground
      ((< y (- surface-height 5)) 3)  ; Stone
      ;; Dirt underground
      ((< y surface-height) 2)  ; Dirt
      ;; Surface layer
      ((= y surface-height)
       (if (< surface-height water-level)
           7  ; Sand for underwater areas
           (case (random 4)
             (0 1)  ; Grass
             (1 2)  ; Dirt
             (2 7)  ; Sand
             (3 2))))  ; Dirt
      ;; Water above surface if below water level
      ((and (< y water-level) (> y surface-height)) 6)  ; Water
      ;; Air above surface
      (t 0))))  ; Air

(defun add-surface-features (chunk biome seed)
  "Add surface features like trees, flowers, grass based on biome"
  (let ((blocks (chunk-blocks chunk))
        (chunk-x (chunk-x chunk))
        (chunk-z (chunk-z chunk)))
    
    ;; Add trees based on biome
    (when (> (biome-tree-density biome) (random 1.0))
      (loop for local-x from 0 below +chunk-size+ by 4
            do (loop for local-z from 0 below +chunk-size+ by 4
                     when (< (random 1.0) (biome-tree-density biome))
                     do (let ((world-x (+ (* chunk-x +chunk-size+) local-x))
                              (world-z (+ (* chunk-z +chunk-size+) local-z))
                              (surface-height (find-surface-height blocks local-x local-z)))
                          (when (and (= (aref blocks local-x surface-height local-z) 1)  ; Grass
                                     (> surface-height 0))
                            (add-tree-at blocks local-x (1+ surface-height) local-z))))))
    
    ;; Add flowers and grass
    (loop for local-x from 0 below +chunk-size+
          do (loop for local-z from 0 below +chunk-size+
                   when (< (random 1.0) (biome-grass-density biome))
                   do (let ((surface-height (find-surface-height blocks local-x local-z)))
                        (when (and (= (aref blocks local-x surface-height local-z) 1)  ; Grass
                                   (> surface-height 0))
                          (if (< (random 1.0) 0.7)
                              (setf (aref blocks local-x (1+ surface-height) local-z) 5)  ; Leaves for grass
                              (setf (aref blocks local-x (1+ surface-height) local-z) 1))))))))  ; Grass block

(defun add-underground-features (chunk biome seed)
  "Add underground features like ores"
  (let ((blocks (chunk-blocks chunk)))
    ;; Add ores
    (loop for local-x from 0 below +chunk-size+
          do (loop for y from 5 below 64  ; Ores only underground
                   do (loop for local-z from 0 below +chunk-size+
                            when (< (random 1.0) 0.01)  ; 1% chance per block
                            do (let ((ore-type (select-ore-type y seed)))
                                 (setf (aref blocks local-x y local-z) ore-type)))))))

(defun find-surface-height (blocks local-x local-z)
  "Find the surface height at the given local coordinates"
  (loop for y from 255 downto 0
        when (not (zerop (aref blocks local-x y local-z)))
          return y
        finally (return 0)))

(defun add-tree-at (blocks x y z)
  "Add a tree at the specified location"
  (let ((height (+ 4 (random 3))))  ; Random height between 4-6
    ;; Tree trunk
    (loop for i from 0 below height
          do (setf (aref blocks x (+ y i) z) 4))  ; Wood block
    
    ;; Tree leaves
    (loop for dx from -2 to 2
          do (loop for dy from 0 below 3
                   do (loop for dz from -2 to 2
                            when (and (<= (+ (abs dx) (abs dz)) 3)
                                      (< (+ y height dy) 256))
                            do (let ((current-x (+ x dx))
                                     (current-y (+ y height dy))
                                     (current-z (+ z dz)))
                                 (when (and (>= current-x 0) (< current-x +chunk-size+)
                                            (>= current-z 0) (< current-z +chunk-size+)
                                            (zerop (aref blocks current-x current-y current-z)))
                                   (setf (aref blocks current-x current-y current-z) 5)))))))  ; Leaves

(defun select-ore-type (y seed)
  "Select an ore type based on depth"
  (cond
    ;; Coal ore (most common, from y=5 to y=128)
    ((and (>= y 5) (<= y 128) (< (random 1.0) 0.6)) 9)  ; Coal ore
    ;; Iron ore (common, from y=5 to y=64)
    ((and (>= y 5) (<= y 64) (< (random 1.0) 0.3)) 10)  ; Iron ore
    ;; Gold ore (rare, from y=5 to y=32)
    ((and (>= y 5) (<= y 32) (< (random 1.0) 0.1)) 11)  ; Gold ore
    ;; Diamond ore (very rare, from y=5 to y=16)
    ((and (>= y 5) (<= y 16) (< (random 1.0) 0.05)) 12)  ; Diamond ore
    ;; Default to stone
    (t 3)))

;; Initialize world with seed
(defun initialize-world-with-seed (seed)
  "Initialize the world with the given seed"
  (let ((world (make-hash-table :test 'equal)))
    ;; Generate initial chunks around origin
    (loop for chunk-x from -2 to 2
          do (loop for chunk-z from -2 to 2
                   do (let ((chunk (generate-terrain-for-chunk chunk-x chunk-z seed)))
                        (setf (gethash (list chunk-x chunk-z) world) chunk))))
    world))