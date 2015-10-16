
(defun longest-path (start end net)
  (or (reverse (dfs end (list start) net  nil))
      (and (eql start end) (list start))))
  
(defun dfs (end path net best-path)
  (if (and (eql end (car path))
           (consp (cdr path)))
      path
    (let ((neighbors (cdr (assoc (car path) net))))
      (do ((temp neighbors (cdr temp))
           (best-path best-path (if (or (null (member (car temp) path))(eql (car temp) end))
                                   (longer-path (dfs end (cons (car temp) path) net best-path) best-path)
                                 best-path)))
          ((null temp) best-path)))))

(defun longer-path (path1 path2) 
  (if (> (length path1) (length path2))
      path1
    path2))
