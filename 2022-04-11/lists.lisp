(setf z (list 'a (list 'b 'c) 'd))

(null nil)

(consp (cons 1 2))

(defun atom? (x) (not (consp x)))

(eq (+ 1 most-positive-fixnum)
    (+ 1 most-positive-fixnum)) ; => nil

(eq 2
    (+ 1 1))

(eql (+ 1 most-positive-fixnum)
     (+ 1 most-positive-fixnum)) ; => t

;; TODO how do you represent NaN?

(eql 1.0 1) ; => nil

(= 1.0 1) ; => t

(equal (cons 'a nil) (cons 'a nil)) ; => t
(equal 1.0 1) ; => nil

(equal "hello" "hello") ; => t

(defstruct point x y)

(equalp (make-point :x 1 :y 2)
        (make-point :x 1 :y 2))

;; COMPRESS

(defun compress-aux (elem num lst)
  (if (null lst)
      (if (= 1 num)
          (list elem)
          (list (list num elem)))
      (if (eql (car lst) elem)
          (compress-aux elem (+ num 1) (cdr lst))
          (cons (if (= 1 num)
                    elem
                    (list num elem))
                (compress-aux (car lst) 1 (cdr lst))))))

(defun compress (lst)
  (if (null lst)
      nil
      (compress-aux (car lst) 1 (cdr lst))))

(compress '(1 1 1 0 1 0 0 0 0 1))

;; ((3 1) 0 1 (4 0) 1)

;; 3.6
