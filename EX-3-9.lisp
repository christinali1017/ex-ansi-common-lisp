
(defun longest-path (start end net)
  (let ((paths (dfs end (list start) net)))
  	(if (and (null paths) (eql start end))
  		(list start)
     (reverse (car (sort paths
                         (lambda (path-1 path-2)
                           (> (length path-1) (length path-2)))))))))

(defun dfs (end path net)
  (if ( and (eql end (car path)) (/= 1 (length path)))
      (list path)
    (let ((neighbors (cdr (assoc (car path) net))))
     (do ((temp neighbors (cdr temp))
           (paths nil (if (or (null (member (car temp) path)) (eql (car temp) end))
                          (append paths (dfs end (cons (car temp) path) net))
                        paths)))
       ((null temp) paths)))))
