
(defun max-min (v &key (maxnum nil) (minnum nil) (start 0) (end (length v)))
  (if (= start end)
      (values maxnum minnum)
    (let ((num (svref v start)))
      (max-min v
               :maxnum (if (null maxnum) num (max num maxnum))
               :minnum (if (null minnum) num (min num minnum))
               :start (1+ start)
               :end end))))
               