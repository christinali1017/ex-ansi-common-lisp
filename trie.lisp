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
              (unless (assoc x (trie-children node)) (push (cons x (make-trie)) (trie-children node)))
              (incf (trie-count node))
              (setf node (next-node x node)))
      (coerce string 'list))
    (setf (trie-word node) string)
    (incf (trie-count node)))
  trie)

(defun subtrie (trie &rest chars)
  (let ((node trie))
    (mapc #'(lambda (x) (setf node (next-node x node))) chars) 
    node))

(defun next-node (c node)
  (cdr (assoc c (trie-children node))))
  
(defun mapc-trie (fn trie)
  (mapc #'(lambda (x) (funcall fn (car x) (cdr x)))
    (trie-children trie)))

(defun read-words(file trie)
  (with-open-file (in file :direction :input)
    (do ((line (read-line in nil :eof) (read-line in nil :eof)))
        ((eql line :eof) trie)
      (add-word line trie))))

