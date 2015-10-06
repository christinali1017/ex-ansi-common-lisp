(in-package : cs325-user)

(defun greater (x y)
  (cond ((> x y) x)
        (t y)))

(defun has-list-p (lst)
  (cond ((null lst) nil)
        ((listp (car lst)) t)
        (t (has-list-p (cdr lst)))))


(defun print-dots-a (num) ;Recursive
  (when (> num 0)
    (format t ".")
    (print-dots(1- num))))

 (defun print-dots (num) ;Iterative
   (loop for i from 1 to num
       do (format t ".")))

(defun get-a-count-a (lst) ;Recursive
  (cond ((null lst) 0)
        ((eql (car lst) 'a)
         (1+ (get-a-count(cdr lst))))
        (t (get-a-count(cdr lst)))))


(defun get-a-count-b (lst) ;Iterative
  (do ((temp lst (cdr temp))
       (counts 0 
               (+ counts 
                  (if (eql (car temp) 'a) 
                      1 0))))
      ((null temp) counts)))


(defun get-a-count-c (lst) ;Iterative
  (let ((counts 0))
    (dolist (el lst)
      (when (eql el 'a)
        (incf counts)))
    counts))


(defun get-a-count (lst) ;Iterative
  (do ((temp lst (cdr temp))
       (counts 0 (+ counts
                    (if (eql (car temp) 'a)
                        1 0))))
      ((null temp) counts)))
     
  
;Problem : You should know that Lisp is a "Functional programming language", which
;means writing programs that work by returning values, instead of by modifying things.
;Thus the original list is untouched after called remove.
(defun summit (lst) 
  (apply #'+ (remove nil lst)))

;Problem : two problems. 
;  1) no recursion end condition, this it will stackoverflow 
;  2) should use numberp to check if x is number
(defun summit-a (lst)
  (cond ((null lst) 0)
        (t (let ((x (car lst)))
             (if (numberp x)
                 (+ x (summit (cdr lst)))
               (summit (cdr lst)))))))

  
