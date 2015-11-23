
(defun make-best-change (num &optional (coins '(25 10 5 1)))
  (let ((best (make-list (length coins) :initial-element num)))
    (make-change num coins best nil)
    (print best)
    best))
  

;calculate all possible changes
(defun make-change (num coins best current)
  ;(print best)
  ;(print current)
  (cond ((null coins) (setq best (compare-coins best (get-changes num current))))
        (t (do ((count (floor num (car coins)) (1- count)))
               ((< count 0))
             (make-change  (- num (* count (car coins))) (cdr coins) best (cons count current))))))
                                                                   

;compare number of changes of two coins lists.
(defun compare-coins (lst1 lst2)
  (cond ((< (car (last lst1)) (car (last lst2))) lst1)
        ((> (car (last lst1)) (car (last lst2))) lst2)
        ((< (reduce #'+ (butlast lst1)) (reduce #'+ (butlast lst2))) lst1)
        (t lst2)))

(defun get-changes (num changes)
  (if (= num 0) changes 
    (cons num changes)))

;add zero to list
(defun add-zero (num)
  (cons 0 num))

;increase coin count
(defun incf-coin (lst)
  (incf (car lst)) lst)
  
