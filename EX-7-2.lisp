(defun map-file (function pathname)
  (with-open-file (stream pathname :direction : input)
    (map-stream function stream)))

(defun map-stream (function stream)
  (let ((end-of-stream (gensym)))
    (do ((el (read stream nil end-of-stream) (read stream nil end-of-stream))
         (x nil (funcall function el)))
        ((eql el end-of-stream) nil))))
