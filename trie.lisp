(defpackage trie
  (:use "COMMON-LISP")
  (:export "make-trie" "add-word" "subtrie" "trie-word" "trie-count" "mapc-trie" "read-words"))

(in-package trie)

(defstruct node (val nil) branches)

(defun add-word (string trie)
  (let ((tr trie))
    (mapc #'(lambda (x) 
             (cond ((assoc x (trie-branches tr))
                    (setf tr (cdr (assoc x (trie-branches tr)))))
                   (t (push (cons x (make-trie)) (trie-branches tr))
                      (setf tr (cdr (assoc x (trie-branches tr)))))))
          (coerce (string-downcase string) 'list))
    (setf (trie-val tr) (string-downcase string)))
  trie)


(defun subtrie (trie &rest chars)
  (let ((tr trie))
    (cond ((null chars) trie)
          (t (mapc #'(lambda (x) 
                      (cond ((assoc x (trie-branches tr))
                             (setf tr (cdr (assoc x (trie-branches tr)))))
                            (t (return-from subtrie nil)))) 
                   chars)
             tr))))


(defun trie-word (trie)
  (trie-val trie))


(defun trie-count (trie)
  (let ((total 0))
    (mapc #'(lambda (x) (incf total (trie-count (cdr x)))) (trie-branches trie))
    (when (trie-val trie) (incf total))
  total))


(defun mapc-trie (fn trie)
  (mapc #'(lambda (x) (funcall fn (car x) (cdr x))) (trie-branches trie)))


(defun read-words(file trie)
  (with-open-file (stream file :direction :input)
    (let ((eos (gensym)))
      (do ((line (read-line stream nil eos) (read-line stream nil eos)))
          ((eql line eos) trie)
        (add-word line trie)))))
       

          
                 
  
    

  
