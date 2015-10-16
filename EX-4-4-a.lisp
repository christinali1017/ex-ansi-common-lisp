(in-package : cs325-user)

(defun bst-elements (bst)
  (if bst
    (append (bst-elements (node-r bst))
            (list (node-elt bst))
            (bst-elements (node-l bst)))
    nil))
