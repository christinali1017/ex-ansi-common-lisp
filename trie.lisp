(defpackage trie
  (:use "COMMON-LISP")
  (:export "ADD-WORD" "MAKE-TRIE"
           "MAPC-TRIE" "READ-WORDS"
           "SUBTRIE" "TRIE-COUNT"
           "TRIE-WORD"))
(in-package trie)

(defun make-trie)

(defun add-word (word trie)
  (when (trie-p trie)
    (let ((root (trie-root trie)))
      (if (zerop (length word))))))
          
                 
  
    

  