
(defclass tokenizer ()
  ((tokens :accessor tokenizer-tkns
           :initarg :tokens
           :initform nil)))

(defmethod next-token-p ((tk tokenizer))
  (not (null (slot-value tk 'tokens))))

(defmethod next-token ((tk tokenizer))
  (let ((tokens (slot-value tk 'tokens)))
    (setf (slot-value tk 'tokens) (cdr tokens))
    (car tokens)))

(defun make-tokenizer (string delim)
  (let ((tk (make-instance 'tokenizer)))
    (setf (slot-value tk 'tokens) (split string delim))
    tk))

(defun split (string delim)
  (do* ((begin 0 (1+ end))
        (end (position delim string) (position delim string :start begin))
        (tokens (init-tokens begin end delim string) (add-token begin end string delim tokens)))
       ((null end) (reverse tokens))))

;init tokens for split
(defun init-tokens (begin end delim string)
  (when (or (not (eql #\space delim))
            (not (eql begin end)))
    (list (subseq string begin end))))

;add next token to tokens list
(defun add-token (begin end string delim tokens)
  (if (and (eql #\space delim)
           (or (null end)
               (eql begin end)))
      tokens
    (cons (subseq string begin end) tokens)))

;helper function from excercise web page.
(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
        ((not (next-token-p tr)) (nreverse l)))))
