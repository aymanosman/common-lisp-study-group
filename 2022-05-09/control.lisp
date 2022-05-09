(progn
  (format t "a")
  (format t "b")
  (+ 11 12))

(defun head ()
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never see this."))

;; compiles to

(block head
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never see this."))

(return 42)

;; means

(return-from nil 42)

(defvar x)

(tagbody
   (setf x 0)
 top
   (setf x (+ x 1))
   (format t "~a " x)
   (if (< x 10) (go top)))

(let ((x 7)
      (y 2))
  (format t "Number")
  (+ x y))

((lambda (x y)
   (format t "Number")
   (+ x y))
 7 2)

(let* ((x 1)
       (y (+ x 1)))
  (+ x y))

(destructuring-bind (w (x y) . z) '(a (b c) d e)
  (list w x y z))

;; [w, [x, y], ...z] = [a, [b, c], d, e]

(case 'may
  (#.(append '(jan mar may)
             '(jul aug oct dec))
   31)
  ((apr jun sept nov) 30)
  (feb 28)
  (otherwise "unknown month"))

(let ((#:s767 'may) (#:h768 0))
  (if (and (sb-kernel:pointerp #:s767)
           (eq #:s767
               (svref
                #(0 aug 0 0 0 0 0 0 0 oct 0 0 0 0 0 apr jun jul sept nov feb 0
                  0 dec 0 0 0 mar may 0 0 jan)
                (setq #:h768
                      (ldb (byte 5 1)
                           (sb-kernel:symbol-hash* #:s767 nil))))))
      (sb-ext:truly-the (or (integer 28 28) (integer 30 31))
                        (aref
                         #(0 31 0 0 0 0 0 0 0 31 0 0 0 0 0 30 30 31 30 30 28 0
                           0 31 0 0 0 31 31 0 0 31)
                         #:h768))
      (progn "unknown month")))
