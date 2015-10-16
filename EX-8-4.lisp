(defpackage "RING"
  (:use "COMMON-LISP")
  (:export "NEW-BUF" "BUF-INSERT" 
           "BUF-NEXT" "BUF-RESET" 
           "BUF-CLEAR" "BUF-FLUSH"
           "BUF-POP"))

(defpackage "FILE"
  (:use "COMMON-LISP" "RING"))
  
