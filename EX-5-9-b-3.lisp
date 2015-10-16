(in-package : cs325-user)

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))


(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (let ((paths (new-paths path node net end)))
          (if (assoc end paths)
              (reverse (assoc end paths))
            (bfs end (append (cdr queue) paths) net)))))))


(defun new-paths (path node net end)
  (mapcan #'(lambda (n)
              (let ((res (when (null (member n path))
                           (cons n path))))
                (if (null res) nil
                  (list res))))
    (cdr (assoc node net))))





 


