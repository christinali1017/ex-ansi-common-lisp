(in-package : cs325-user)

(defun shortest-path (start end net)
  (catch nil (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (bfs end (append (cdr queue)
                         (new-paths path node net end))
             net)))))

(defun new-paths (path node net end)
  (find-path-p (assoc node net) end path)
  (mapcan #'(lambda (n)
              (if (member n path) nil
                (list (cons n path))))           
    (cdr (assoc node net))))


(defun find-path-p (neibs end path)
  (when (member end neibs)
      (throw nil (reverse (cons end path)))))
 





 


