;;;Peform greedy algorithm

(defun make-change (num &optional (coins-list '(25 10 5 1)))
  (let ((coins nil))
    (values-list
     (mapcar #'(lambda (x) (multiple-value-setq (coins num) (truncate num x)))
       coins-list))))
