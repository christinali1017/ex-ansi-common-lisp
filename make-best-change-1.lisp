
(defun make-best-change (num &optional (coins '(25 10 5 1)))
  (values-list (reverse (car (make-change num coins nil nil)))))

;calculate all possible changes
(defun make-change (num coins current best)
  (do ((choices (get-choices num coins) (cdr choices))
       (temp-best best (make-change (- num (* (car choices) (car coins)))
                               (cdr coins)
                               (cons (car choices) current)
                               temp-best)))
      ((null choices) (update-best num current temp-best))))

;get choices for each coin.
(defun get-choices (num coins)
  (cond ((null coins) nil)
        ((null (cdr coins)) (list (floor num (car coins))))
        (t (loop for i from (floor num (car coins)) downto 0 collect i))))

;update best choices.
(defun update-best (num current best)
  (cond ((or (null best)
             (< num (cdr best))
             (and (= num (cdr best))
                  (< (reduce #'+ current) (reduce #'+ (car best)))))
         (cons current num))
        (t best)))

  
  
