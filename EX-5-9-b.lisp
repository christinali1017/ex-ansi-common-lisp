(in-package : cs325-user)

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))


(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (let ((neibs (assoc node net)))
          (if (member end neibs)
              (reverse (cons end path))
            (bfs end (append (cdr queue) (new-paths path node net end)) net)))))))


(defun new-paths (path node net end)
  (mapcan #'(lambda (n)
              (if (member n path) nil
                (list (cons n path))))           
    (cdr (assoc node net))))





 


