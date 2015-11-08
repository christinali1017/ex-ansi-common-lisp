(defmacro nth-expr (n &body expr)
  `(case ,n
     ,@(let ((i 0))
         (mapcar #'(lambda(x) `(,(incf i) ,x))
           expr))))
