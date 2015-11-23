
(defun horner (x &rest args)
  (reduce (lambda (a b) (+ (* x a) b)) args))
