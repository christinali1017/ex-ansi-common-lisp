
(defmacro preserve (lst &body body)
  `((lambda ,lst ,@body) ,@lst))
