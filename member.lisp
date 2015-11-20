(in-package :ddr-tests)


(defparameter *member-kb*
  '((<- (member ?x (cons ?x ?l)))
    (<- (member ?x (cons ?a ?l)) (member ?x ?l))))
