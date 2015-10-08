(in-package : cs325-user)

(defun shortest-path (start end net)
  (catch nil (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
            (reverse path)
          (bfs end (append (cdr queue)
                           (new-paths path node net end))
               net))))))



(defun new-paths (path node net end)
  (mapcan #'(lambda (n)
              (let ((res (when (null (member n path))
                           (let ((path1 (cons n path)))
                             (if (eql n end)
                                 (throw nil (reverse path1))
                               path1)))))
                (if (null res) nil
                  (list res))))
    (cdr (assoc node net))))





 


