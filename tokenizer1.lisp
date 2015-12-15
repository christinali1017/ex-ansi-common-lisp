
(defclass tokenizer ()
  ((str :accessor tokenizer-str
        :initarg :str
        :initform nil)
   
   (begin :accessor tokenizer-begin
          :initarg :begin
          :initform 0)
   
   (delim :accessor tokenizer-delim
          :initarg :delim
          :initform #\space)
   
   (space-p :accessor space-p
            :initarg :space
            :initform t)))
  
(defmethod next-token-p ((tk tokenizer))
  (if (space-p tk)
      (/= (tokenizer-begin tk) (length (tokenizer-str tk)))
    (not (null (tokenizer-begin tk)))))
 
(defmethod next-token ((tk tokenizer))
  (let* ((end (position (tokenizer-delim tk) (tokenizer-str tk) :start (tokenizer-begin tk)))
         (res (subseq (tokenizer-str tk) (tokenizer-begin tk) end)))
    (setf (tokenizer-begin tk) (get-begin-index end (tokenizer-str tk) (tokenizer-delim tk) tk))
    res))

(defun make-tokenizer (string delim)
  (let ((temp (eql #\space delim)))
  (make-instance 'tokenizer
    :space temp
    :str string
    :begin (init-begin string delim temp)
    :delim delim)))

;begin index of next token
(defun get-begin-index (begin string delim tk)
  (cond ((null begin) nil)
        ((not (space-p tk)) (1+ begin))
        (t (begin-idx-space begin string))))

;init begin index
(defun init-begin (string delim space)
  (if space
      (begin-idx-space 0 string)
    0))
  

;begin index when delim is space
(defun begin-idx-space (begin string)
  (let ((idx (position-if #'(lambda (x) (not (eql #\space x))) string :start begin)))
    (or idx (length string))))

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
