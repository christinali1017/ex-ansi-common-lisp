(in-package : cs325-user)
(critique
(defun shortest-path (start end net)
  (defun bfs (end queue net)
    (if (empty-queue-p queue)
        nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (return-from shortest-path (reverse path))
            (bfs end (append (cdr queue)
                             (mapcan #'(lambda (n)
                                         (let ((res (when (null (member n path))
                                                      (let ((path1 (cons n path)))
                                                        (if (eql n end)
                                                            (return-from shortest-path (reverse path1))
                                                          path1)))))
                                           (if (null res) nil (list res))))
                               (cdr (assoc node net))))
                 net))))))
  (bfs end (list (list start)) net))
)






 


