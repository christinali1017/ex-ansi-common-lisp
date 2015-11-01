
(defun solve (f min max epsilon)
  (let* ((res-min (funcall f min))
         (res-max (funcall f max)))
    (cond ((= res-min 0) res-min)
          ((= res-max 0) res-max)
          ((< 0 (* res-min res-max)) nil)
          (t (actual-search f res-min res-max min max epsilon)))))

(defun actual-search (f res-min res-max min max epsilon)
  (let* ((mid (* 0.5 (+ min max)))
        (res-mid (funcall f mid)))
    (cond ((< (- max min) epsilon) mid)
          ((= res-mid 0) mid)
          ((< 0 (* res-min res-mid)) (actual-search f res-mid res-max mid max epsilon))
          ((< 0 (* res-max res-mid)) (actual-search f res-min res-mid min mid epsilon))
          (t nil))))




(defun solve1 (f min max epsilon)
  (let ((mid (* 0.5 (+ min max))))
    (let ((res-min (funcall f min))
          (res-max (funcall f max))
          (res-mid (funcall f mid)))
      (cond ((< (- max min) epsilon) mid)
            ((= res-mid 0) mid)
            ((< 0 (* res-min res-mid)) (solve f mid max epsilon))
            ((< 0 (* res-max res-mid)) (solve f min mid epsilon))
            (t nil)))))

(defun solve2 (f min max epsilon)
  (let (i iv)
    (do ((x min (+ x epsilon)))
        ((> x max))
      (let ((xv (funcall f x)))
        (when (or (null i)
                  (< (abs xv) (abs iv)))
          (setf i x iv xv))))
     i))
  
        
  