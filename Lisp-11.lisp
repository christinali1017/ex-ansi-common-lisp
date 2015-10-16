
(defun map-range (fn start end)
  (do ((step (if (< start end) 1 -1))
       (n start (+ step n))
       (res nil (cons (funcall fn n) res)))
      ((= n end) (reverse res))))

(defun every-range (fn start end)
  (do ((step (if (< start end) 1 -1))
       (n start (+ step n)))
      ((or (= n end) (not (funcall fn n)))
       (or (= n end) (funcall fn n)))))

(defun find-range (fn start end)
  (do ((step (if (< start end) 1 -1))
       (n start (+ step n)))
      ((or (= n end) (funcall fn n))
       (and (/= n end) n))))
   
       

