
(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((same-point x1 y1 x2 y2) (point-line-intersect x1 y1 x3 y3 x4 y4))
        ((same-point x3 y3 x4 y4) (point-line-intersect x3 y3 x1 y1 x2 y2))
        ((is-parallel x1 y1 x2 y2 x3 y3 x4 y4) (parallel-intersect x1 y1 x2 y2 x3 y3 x4 y4))
        (t (non-parallel x1 y1 x2 y2 x3 y3 x4 y4))))

;;;check if two points have same coordinate
(defun same-point (x1 y1 x2 y2)
  (and (= x1 x2) (= y1 y2)))

;;;check if two segment is parallel
(defun is-parallel (x1 y1 x2 y2 x3 y3 x4 y4)
  (= (slope x1 y1 x2 y2) (slope x3 y3 x4 y4)))

;;;get the slope of the segment
(defun slope (x1 y1 x2 y2)
  (if (= x1 x2) most-positive-fixnum
    (/ (- y2 y1) (- x2 x1))))

;;;get intersect of a point and a segment
(defun point-line-intersect (point-x point-y x1 y1 x2 y2)
  (when (or (and (same-point point-x point-y x1 y1) (same-point point-x point-y x2 y2))
          (point-on-segment point-x point-y x1 y1 x2 y2))
    (values point-x point-y point-x point-y)))

;;;check if point is on the segment
(defun point-on-segment (point-x point-y x1 y1 x2 y2)
  (or (same-point point-x point-y x1 y1)
      (same-point point-x point-y x2 y2)
      (and (= (slope x1 y1 x2 y2) (slope point-x point-y x1 y1))
           (>= point-x (min x1 x2))
           (<= point-x (max x1 x2)))))

;;;get intersect of two parallel segment
(defun parallel-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
  (when (same-line x1 y1 x2 y2 x3 y3 x4 y4)
      (same-line-segment-intersect x1 y1 x2 y2 x3 y3 x4 y4)))
   
;;;check if two parallel segment is on the same line
(defun same-line (x1 y1 x2 y2 x3 y3 x4 y4)
  (= (intercept x1 y1 x2 y2) (intercept x3 y3 x4 y4)))

;;;get intercept of a line.
(defun intercept (x1 y1 x2 y2)
  (if (= x1 x2)
      most-positive-fixnum
    (- y1 (* x1 (slope x1 y1 x2 y2)))))

;;;Get intersect for segment on a line.
(defun same-line-segment-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
  (when (and (>= (max x1 x2) (min x3 x4))
             (<= (min x1 x2) (max x3 x4)))
    (get-overlap x1 y1 x2 y2 x3 y3 x4 y4)))

;;;get overlap for two overlap segments
(defun get-overlap (x1 y1 x2 y2 x3 y3 x4 y4)
  (if (> (slope x1 y1 x2 y2) 0)
      (values (max (min x1 x2) (min x3 x4))
              (max (min y1 y2) (min y3 y4))
              (min (max x1 x2) (max x3 x4))
              (min (max y1 y2) (max y3 y4)))
    (values (max (min x1 x2) (min x3 x4))
            (min (max y1 y2) (max y3 y4))
            (min (max x1 x2) (max x3 x4))
            (max (min y1 y2) (min y3 y4)))))


;;;get intersect of two unparallel segments
(defun non-parallel (x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((intercept1 (intercept x1 y1 x2 y2))
        (intercept2 (intercept x3 y3 x4 y4))
        (k1 (slope x1 y1 x2 y2))
        (k2 (slope x3 y3 x4 y4))
        (x (/ (- intercept1 intercept2) (- k2 k1)))
         (y (+ (* x k1) intercept1)))
    (when (and (point-on-segment x y x1 y1 x2 y2)
             (point-on-segment x y x3 y3 x4 y4))
        (values x y))))
        
        
            


 




    


         