(let ((start 0)
      (end 5))
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~a ~a~%" i (* i i))))

;; expands to

(let ((start 0) (end 5))
  (block nil
    (let ((i start))
      (declare (ignorable i))
      (tagbody
         (go #:g272)
         #:g271
         (format t "~a ~a~%" i (* i i))
         (progn (setq i (+ i 1))
                nil)
         #:g272
         (if (> i end)
             nil
             (go #:g271))
         (return-from nil (progn 'done))))))


;; NOTE understand how macroexpand works
(do* ((x 1 (+ x 1))
      (y x x))
     ((> x 5))
  (format t "(~A ~A)  " x y))

;; expands to

(block nil
  (let* ((x 1)
         (y x))
    (declare (ignorable x y))
    (tagbody
       (go #:g319)
       #:g318
       (format t "(~A ~A)  " x y)
       (setq x (+ x 1)
             y x)
       #:g319
       (if (> x 5)
           nil
           (go #:g318))
       (return))))

(dolist (x '(a b c d) 'done)
  (format t "~A " x))

(block nil
  (let ((#:list320 '(a b c d)))
    (tagbody
       #:start321
       (if (endp #:list320)
           nil
           (progn
             (let ((x (truly-the (member d c b a) (car #:list320))))
               (setq #:list320 (cdr #:list320))
               (format t "~A " x))
             (go #:start321)))))
  ;; == 'done
  (let ((x nil))
    x
    'done))

(defun factorial (n)
  (do ((j n (- j 1))
       (f 1 (* j f)))
      ((= j 0) f)))

;; expands to

(defun factorial (n)
  (block nil
    (let ((j n)
          (f 1))
      (tagbody
         (go #:g347)
         #:g346
         (let* ((#:new1 (- j 1))
                (#:new1 (* j f)))
           (setq j #:new1)
           (setq f #:new1)
           nil)
         #:g347
         (if (= j 0)
             nil
             (go #:g346))
         (return f)))))

(mapc #'(lambda (x y)
          (format t "~a ~a  " x y))
      '(hip flip slip)
      '(hop flop slop))

;; a, b = foo()
;; (multiple-value-bind (a b) (foo)
;;   ...)

(funcall #'+ 1 2 3)
(apply #'+ (list 1 2 3))
(multiple-value-call #'+ (values 1 2 3))


(defun sub ()
  (throw 'abort 99))

(defun super ()
  (catch 'abort
    (sub)
    (format t "We'll never see this")))

;; class Abort extends Exception

;; try {
;;   sub()
;;   print("We'll neve see this")
;; } catch (Abort) {
;;   return 99;
;; }

(defvar x)

(setf x 1)

(catch 'abort
  (unwind-protect
       (throw 'abort 99)
    (setf x 2)))
