
(defun longest-path (start end net)
  (let ((res-path (reverse (dfs end (list start) net  nil))))
     (if (and (null res-path) (eql start end))
         (list start)
       res-path)))
        


(defun dfs (end path net best-path)
  (if (and (eql end (car path))
           (/= 1 (length path)))
      path
    (let ((neighbors (cdr (assoc (car path) net))))
      (do ((temp neighbors (cdr temp))
           (best-path best-path(if (or (null (member (car temp) path))
                                       (eql (car temp) end))
                                   (let ((new-path
                                          (dfs end (cons (car temp) path) net best-path)))
                                     (if (> (length new-path) (length best-path))
                                         new-path
                                       best-path))
                                 best-path)))
          ((null temp) best-path)))))
