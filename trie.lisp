(defpackage #:trie
  (:use #:common-lisp)
  (:export #:trie
           #:make-trie
           #:subtrie
           #:add-word
           #:trie-word
           #:trie-count
           #:mapc-trie
           #:read-words))

(in-package #:trie)

(defstruct trie (word nil) children (count 0))

(defun add-word (string trie)
  (let ((node trie))
    (mapc #'(lambda (x)
              (incf (trie-count node))
              (setf node (next-node x node t)))
      (coerce string 'list))
    (setf (trie-word node) string)
    (incf (trie-count node)))
  trie)

(defun subtrie (trie &rest chars)
  (let ((node trie))
    (mapc #'(lambda (x) (setf node (next-node x node nil))) chars) 
    node))


(defun next-node1 (c node flag)
  (when (and flag (not (assoc c (trie-children node)))) (push (cons c (make-trie)) (trie-children node)))
  (cdr (assoc c (trie-children node))))
  
(defun next-node (c node flag)
  (let ((temp (assoc c (trie-children node))))
    (when (and flag (not temp)) 
      (setf temp (cons c (make-trie)))
      (push temp (trie-children node)))
    (cdr temp)))
  
(defun mapc-trie (fn trie)
  (mapc #'(lambda (x) (funcall fn (car x) (cdr x)))
    (trie-children trie)))

(defun read-words(file trie)
  (with-open-file (in file :direction :input)
    (do ((line (read-line in nil :eof) (read-line in nil :eof)))
        ((eql line :eof) trie)
      (add-word line trie))))

