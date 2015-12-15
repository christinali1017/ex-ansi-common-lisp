
(defun camelize (string &optional (capitalize nil))
  (let ((str (remove #\- (string-capitalize string))))
    (if capitalize str
      (string-downcase str :end 1))))

(defun hyphenate (string &optional (case :upper))
  (let ((format-str (if (eql case :upper) "~:@(~a~)" 
                      "~(~a~)")))
    (with-output-to-string (out)
      (dotimes (i (length string))
        (let ((c (char string i)))
          (when (and (> i 0) (upper-case-p c) (lower-case-p (char string (1- i))))
            (format out "-"))
          (format out format-str c))))))
               
(defun convert-case (case c)
  (if (eql case :upper) (char-upcase c)
    (char-downcase c)))

(define-test camelize
  (assert-equal "job" (camelize "job"))
  (assert-equal "Job" (camelize "job" t))
  (assert-equal "jobPosting" (camelize "job-posting"))
  (assert-equal "BookFormatType" (camelize "book-format-type" t))
  (assert-equal "JobPosting" (camelize "job-posting" t))
)

;;; HYPHENATE

(define-test hyphenate
  (assert-equal "JOB" (hyphenate "job"))
  (assert-equal "JOB" (hyphenate "Job"))
  (assert-equal "JOB" (hyphenate "Job" :upper))
  (assert-equal "job" (hyphenate "Job" :lower))
  (assert-equal "JOB-POSTING" (hyphenate "jobPosting"))
  (assert-equal "JOB-POSTING" (hyphenate "JobPosting"))
  (assert-equal "BOOK-FORMAT-TYPE" (hyphenate "BookFormatType"))
  (assert-equal "URL" (hyphenate "URL"))
  (assert-equal "GET-ID" (hyphenate "getID"))
  )
