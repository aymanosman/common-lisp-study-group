(defun our-member (obj lst)
  "Return the tail of LIST beginning with first element satisfying EQLity,
   :TEST, or :TEST-NOT with the given ITEM."
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

(defun ask-number ()
  (format t "Please enter a number.")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(defun show-squares (start end)
  (loop :for i :from start :to end
        :do (format t "~a ~a~%" i (* i i)))
  'done)
