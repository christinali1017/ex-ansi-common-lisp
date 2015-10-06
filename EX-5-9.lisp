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
          (bfs end (remove nil (append (cdr queue)
                           (new-paths path node net end)))
               net))))))


(defun new-paths (path node net end)
  (mapcar #'(lambda (n)
              (when (eql (is-circle path n) null)
                (let ((path1 (cons n path)))
                  (if (eql n end)
                      (throw nil (reverse path1))
                    path1))))
    (cdr (assoc node net))))


(defun is-circle (path n)
  (cond ((null path) null)
        ((eql n (car path)) t)
        (t (is-circle (cdr path) n))))






 


