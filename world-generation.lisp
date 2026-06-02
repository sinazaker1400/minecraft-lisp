(in-package #:minecraft-3d)

(defun chunk-key (cx cy cz)
  (list cx cy cz))

(defun world->chunk (x size)
  (floor x size))

(defun world->local (x size)
  (mod x size))

(defun world-coords-to-chunk-coords (x y z)
  (values
   (world->chunk x *chunk-size-x*)
   (world->chunk y *chunk-size-y*)
   (world->chunk z *chunk-size-z*)))

(defun calculate-height (x z)
  (+ 60
     (mod (sxhash (list x z)) 8)))

(defun generate-chunk (cx cy cz)

  (let ((chunk (make-chunk
                :x cx
                :y cy
                :z cz)))

    (loop
      for lx below 16 do
      (loop
        for ly below 16 do
        (loop
          for lz below 16 do

          (let* ((wx (+ (* cx 16) lx))
                 (wy (+ (* cy 16) ly))
                 (wz (+ (* cz 16) lz))

                 (h (calculate-height wx wz)))

            (setf
             (aref (chunk-blocks chunk)
                   lx ly lz)

             (cond
               ((= wy h) 'grass)
               ((>= wy (- h 2)) 'dirt)
               ((< wy h) 'stone)
               (t nil)))))))

    chunk))

(defun get-chunk (cx cy cz)

  (or (gethash (chunk-key cx cy cz)
               *world-chunks*)

      (setf (gethash (chunk-key cx cy cz)
                     *world-chunks*)

            (generate-chunk cx cy cz))))

(defun get-block (wx wy wz)

  (multiple-value-bind (cx cy cz)
      (world-coords-to-chunk-coords wx wy wz)

    (let ((chunk (get-chunk cx cy cz)))

      (aref
       (chunk-blocks chunk)

       (world->local wx 16)
       (world->local wy 16)
       (world->local wz 16)))))

(defun find-block (wx wy wz)

  (multiple-value-bind (cx cy cz)
      (world-coords-to-chunk-coords wx wy wz)

    (let ((chunk
           (gethash
            (chunk-key cx cy cz)
            *world-chunks*)))

      (when chunk
        (aref
         (chunk-blocks chunk)
         (world->local wx 16)
         (world->local wy 16)
         (world->local wz 16))))))