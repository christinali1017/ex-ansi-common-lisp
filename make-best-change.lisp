
(defun make-best-change (num &optional (coins '(25 10 5 1)))
  (values-list (reduce #'compare-coins (make-change num coins))))

;calculate all possible changes
(defun make-change (num coins)
  (cond ((= 0 num) (list (make-list (1+ (length coins)) :initial-element 0)))
        ((null coins) (list (list num)))
        ((< num (car coins)) (mapcar #'add-zero (make-change num (cdr coins))))
        (t (append (mapcar #'incf-coin (make-change (- num (car coins)) coins))
                   (mapcar #'add-zero (make-change num (cdr coins)))))))

;compare number of changes of two coins lists.
(defun compare-coins (lst1 lst2)
  (cond ((< (car (last lst1)) (car (last lst2))) lst1)
        ((> (car (last lst1)) (car (last lst2))) lst2)
        ((< (reduce #'+ (butlast lst1)) (reduce #'+ (butlast lst2))) lst1)
        (t lst2)))

;add zero to list
(defun add-zero (num)
  (cons 0 num))

;increase coin count
(defun incf-coin (lst)
  (incf (car lst)) lst)
  
