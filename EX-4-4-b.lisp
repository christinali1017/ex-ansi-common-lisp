(in-package : cs325-user)
(defun bst-elements (bst)
  (let ((lst nil))
    (bst-traverse 
     #'(lambda (x) (push x lst))
     bst)
    lst))
