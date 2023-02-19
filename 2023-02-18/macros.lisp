(defmacro ntimes (n &rest body)
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))

(ntimes 10
        (princ "."))

(let ((x 10))
  (ntimes 5
          (setf x (+ x 1)))
  x)

;; expect x = 15
;; got 10

(macroexpand-1
 '(ntimes 5 (setf x (+ x 1))))

#+nil
(let ((x 10))
  (do ((x 0 (+ x 1)))
      ((>= x 5))
    (setf x (+ x 1)) ;; <= this 'x' is captured
    )
  x)

(defmacro ntimes (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
         ((>= ,g ,n))
       ,@body)))

(macroexpand-1
 '(ntimes 5 (setf x (+ x 1))))

#+nil
(do ((#:g294 0 (+ #:g294 1)))
    ((>= #:g294 5))
  (setf x (+ x 1)))

(macroexpand-1
 '(ntimes (setf v (- v 1)) (princ ".")))

#+nil
(do ((#:g295 0 (+ #:g295 1)))
    ((>= #:g295 (setf v (- v 1))))
  (princ "."))

(defmacro ntimes (expr &rest body)
  (let ((g (gensym))
        (n (gensym "N")))
    `(let ((,n ,expr))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,n))
         ,@body))))

(macroexpand-1
 '(ntimes (setf v (- v 1)) (princ ".")))

#+nil
(let ((#:n299 (setf v (- v 1))))
  (do ((#:g298 0 (+ #:g298 1)))
      ((>= #:g298 #:n299))
    (princ ".")))

;; Generalized References

(defmacro cah (lst)
  `(car ,lst))

(let ((x (cons 'a 'b)))
  (setf (car x) 44)
  x)

(defmacro incf* (x &optional (y 1))
  `(setf ,x (+ ,x ,y)))

(define-modify-macro our-incf (&optional (y 1)) +)

(defmacro our-bad-incf (place &optional (y 1))
  (let ((p (gensym "PLACE")))
    `(let ((,p ,place))
       (setf ,p (+ ,p ,y)))))

(macroexpand-1
 '(our-bad-incf x))

;; (LET ((#:PLACE304 X))
;;   (SETF #:PLACE304 (+ #:PLACE304 1)))


(macroexpand-1
 '(our-bad-incf (car x)))

;; (LET ((#:PLACE305 (CAR X)))
;;   (SETF #:PLACE305 (+ #:PLACE305 1)))

(macroexpand-1
 '(our-incf (car x)))

;; (LET* ((#:X306 X))
;;   (SB-KERNEL:%RPLACA #:X306 (+ (CAR #:X306) 1)))

(define-symbol-macro self (current-object))

(macroexpand-1
 'self)

;; compiler macros

(funcall (compiler-macro-function 'ppcre:scan) '(ppcre:scan "a+" "aaa") (sb-kernel:make-null-lexenv))

;; (CL-PPCRE:SCAN (LOAD-TIME-VALUE (CL-PPCRE:CREATE-SCANNER "a+")) "aaa")

;; :(
(let ((it (calculate-something)))
  (if it
      (1+ it)
      0))

;; #jsx <div>hello</div>
