
(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((k1 (slope x1 y1 x2 y2))
        (k2 (slope x3 y3 x4 y4))
        (b1 (intercept x1 y1 x2 y2))
        (b2 (intercept x3 y3 x4  y4)))
    (cond ((parallel-p k1 k2) (parallel-intersect x1 y1 x2 y2 x3 y3 x4 y4 b1 b2 k1 k2))
          (t (non-parallel x1 y1 x2 y2 x3 y3 x4 y4 k1 k2 b1 b2)))))

;;;get the slope of the segment
(defun slope (x1 y1 x2 y2)
  (cond ((and (= x1 x2) (= y1 y2)) t)
        ((= x1 x2) nil)
        (t (/ (- y2 y1) (- x2 x1)))))

(defun parallel-p (k1 k2)
  (or (equal k1 t)
      (equal k2 t)
      (equal k1 k2)))

;;;get intersect of two parallel segment
(defun parallel-intersect (x1 y1 x2 y2 x3 y3 x4 y4 b1 b2 k1 k2)
  (cond ((and (equal k1 t) (point-on-segment-p x1 y1 x3 y3 x4 y4)) (values x1 y1 x1 y1))
        ((and (equal k2 t) (point-on-segment-p x3 y3 x1 y1 x2 y2)) (values x3 y3 x3 y3))
        ((or (equal k1 t) (equal k2 t) (/= b1 b2)) nil)
        ((and (equal k2 nil) (equal k1 nil)) (values x1 (max (min y1 y2) (min y3 y4)) x1 (min (max y1 y2) (max y3 y4))))
        (t (parallel-helper x1 y1 x2 y2 x3 y3 x4 y4 b1 b2 k1 k2))))

(defun parallel-helper (x1 y1 x2 y2 x3 y3 x4 y4 b1 b2 k1 k2)
  (let* ((left (max (min x1 x2) (min x3 x4)))
         (right (min (max x1 x2) (max x3 x4))))
    (if (<= left right)
        (values left (+ (* left k1) b1) right (+ (* right k1) b1))
      nil)))
        

;;;get intercept of a line.
(defun intercept (x1 y1 x2 y2)
  (if (= x1 x2)
      most-positive-fixnum
    (- y1 (* x1 (slope x1 y1 x2 y2)))))
                
;;;get intersect of two unparallel segments
(defun non-parallel (x1 y1 x2 y2 x3 y3 x4 y4 k1 k2 b1 b2)
  (let* ((x (/ (- b1 b2) (- k2 k1)))
         (y (+ (* x k1) b1)))
    (if (and (point-on-segment-p x y x1 y1 x2 y2)
               (point-on-segment-p x y x3 y3 x4 y4))
      (values x y) nil)))

;;;check if point is on the segment
(defun point-on-segment-p (point-x point-y x1 y1 x2 y2)
  (and (>= point-x (min x1 x2))
       (<= point-x (max x1 x2))
       (>= point-y (min y1 y2))
       (<= point-y (max y1 y2))))


        
        
            


 




    


         
