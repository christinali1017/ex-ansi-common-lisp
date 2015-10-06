(in-package : cs325-user)

(defstruct 3tree 
  data
  left
  middle
  right)


(defun 3tree-clone(tree)
  (when tree
    (make-3tree :data (3tree-data tree)
                :left (3tree-clone (3tree-left tree))
                :middle (3tree-clone (3tree-middle tree))
                :right (3tree-clone (3tree-right tree)))))


(defun 3tree-member(obj tree)
  (when tree
      (or
       (eql obj (3tree-data tree))
       (3tree-member obj (3tree-left tree))
       (3tree-member obj (3tree-middle tree))
       (3tree-member obj (3tree-right tree)))))

