
(defclass tokenizer ()
  ((str :accessor tokenizer-str
        :initarg :str
        :initform nil)
   
   (begin :accessor tokenizer-begin
          :initarg :begin
          :initform 0)
   
   (delim :accessor tokenizer-delim
          :initarg delim
          :initform 0)))

(defmethod next-token-p ((tk tokenizer))
  (if (space-p (tokenizer-delim tk)) 
      (not (= (tokenizer-begin tk) (length (tokenizer-str tk))))
    (not (null (tokenizer-begin tk)))))
 
(defmethod next-token ((tk tokenizer))
  (let* ((end (position (tokenizer-delim tk) (tokenizer-str tk) :start (tokenizer-begin tk)))
         (res (subseq (tokenizer-str tk) (tokenizer-begin tk) end)))
    (setf (tokenizer-begin tk) (begin-index end (tokenizer-str tk) (tokenizer-delim tk)))
    res))

(defun make-tokenizer (string delim)
  (let ((tk (make-instance 'tokenizer)))
    (setf (tokenizer-str tk) string)
    (setf (tokenizer-begin tk) (init-begin string delim))
    (setf (tokenizer-delim tk) delim)
    tk))

;begin index of next token
(defun begin-index (begin string delim)
  (cond ((null begin) nil)
        ((not (space-p delim)) (1+ begin))
        (t (begin-idx-space begin string))))

;init begin index
(defun init-begin (string delim)
  (if (not (space-p delim)) 0
    (begin-idx-space 0 string)))

;begin index when delim is space
(defun begin-idx-space (begin string)
  (do* ((res begin (1+ res)))
       ((or (= res (length string))
            (not (space-p (char string res))))
        res)))

;check if delim is space
(defun space-p (delim)
  (eql #\space delim))

;end index of next token
(defun end-index (string delim begin)
  (let ((end (position delim string :start begin)))
    (update-begin end string delim)
    end))

;helper function from excercise web page.
(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
        ((not (next-token-p tr)) (nreverse l)))))
