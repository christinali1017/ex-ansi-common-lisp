(in-package : cs325-user)

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))


(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (multiple-value-bind (np target)
                  (new-paths path node net end)
                (or target
                    (bfs end (append (cdr queue) np) net))))))))

(defun new-paths (path node net end)
  (let ((new-paths nil) (target nil)
        (neighbors (cdr (assoc node net))))
    (do ((lst neighbors (cdr lst)))
        ((or (null lst) target) (values new-paths target))
      (let ((cur (car lst)))
        (when (eql (is-circle path cur) null)
          (if (eql cur end)
              (setf target (reverse (cons cur path)))
            (push (cons cur path) new-paths)))))))


(defun is-circle (path n)
  (cond ((null path) null)
        ((eql n (car path)) t)
        (t (is-circle (cdr path) n))))








 


