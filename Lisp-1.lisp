
(defun has-number-p (s-exp)
  (cond
   ((atom s-exp) (numberp s-exp))
   ((some #'has-number-p s-exp) t)
   (t nil)))
 