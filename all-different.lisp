(in-package :ddr-tests)

(defparameter *all-different-kb*
  '((-> (all-different (cons ?c (cons ?b ?a)))
        (all-different (cons ?b ?a))
        (all-different (cons ?c ?a))
        (all-different (cons ?c ?b)))
    (-> (all-different (cons ?c (cons ?b (cons ?a nil))))
        (different ?b ?a)
        (different ?c ?a)
        (different ?c ?b))
    (-> (different ?a ?b) (different ?b ?a))))
