(eval '(+ 1 2 3))

(eval '(format t "Hello"))

(eval (list 'lambda (list 'x) (list '+ 'x 1)))
;; (lambda (x) (+ x 1))

(defun our-toplevel ()
  (loop (print (eval (read)))))

;; TODO come up with pre macro example

(defun if-not (test then else)
  (eval `(if (not ,test)
             ,then
             ,else)))

(if-not '(minusp 42)
        '(print "is positive")
        '(print "is negative"))

(defmacro if-not (test then else)
  `(if (not ,test)
       ,then
       ,else))

(if-not (minusp 42)
        (print "is positive")
        (print "is negative"))

`(1 2 ,@(list 3 4))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defvar x 0)
(while (< x 10)
  (print x)
  (incf x))

;; (while-fun (< x 10) (print x) (incf x))

;; Hygiene

(defmacro ntimes (n &rest body)
  (let ((x (gensym)))
    `(do ((,x 0 (+ ,x 1)))
         ((>= ,x ,n))
       ,@body)))

(let ((x 10))
  (ntimes 5
          (setf x (+ x 1)))
  ;; =>
  ;; (do ((x 0 (+ x 1)))
  ;;     ((>= x 5))
  ;;   (setf x (+ x 1)))
  ;; =>
  ;; (do ((#:g598 0 (+ #:g598 1)))
  ;;     ((>= #:g598 5))
  ;;   (setf x (+ x 1)))
  x)
