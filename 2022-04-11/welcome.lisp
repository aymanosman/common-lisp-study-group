(defun fast-length (lst)
  (declare (type list lst)
           (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  (length lst))

(defun call-fn (f arg)
  ;; (apply f (list arg))
  (funcall f arg))


((lambda (x) (length x)) (list 1 2 3))

(funcall #'(lambda (x) (+ x 100))
         1)

(funcall (lambda (x) (+ x 100))
         1)

(typep 27 'integer)
(typep 27 '(integer 0 100))
(typep 27 '(integer 0 10))
