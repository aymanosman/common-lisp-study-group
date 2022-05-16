;; (boundp '+)
(fboundp '+)

(symbol-function '+)

(defvar add2)

(setf (symbol-function 'add2)
      #'(lambda (x) (+ x 2)))

(defun (setf primo) (val list)
  (setf (car list) val))

(defun foo (x)
  "Some docs"
  x)

(documentation 'foo 'function)

;; like let*
(labels ((add10 (x)
           (+ x 10))
         (consa (x)
           (cons 'a x)))
  (consa (add10 3)))

;; like let
;; (flet ((f (x)
;;          ...)))

;; Racket
;; (define foo ()
;;   (define (add10 x)
;;       (+ x 10))
;;   (define (consa x)
;;       (cons 'a x))
;;   (cons a (add10 3)))
