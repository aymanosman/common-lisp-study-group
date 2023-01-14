(defmacro my-quote (expr)
  (format t "in my-quote ~S~%" expr)
  (list 'quote expr))

(first (my-quote (my-quote (a b c))))
;; in my-quote (MY-QUOTE (A B C))

(first-xxx (my-quote (my-quote (a b c))))
;; in my-quote (MY-QUOTE (A B C))
;; function FIRST-XXX is not defined

;;;

(defmacro nil! (x)
  (list 'setf x nil))

(macroexpand-1 '(nil! y))

(defun expand-nil! (x)
  (list 'setf x nil))

(apply #'expand-nil! (cdr '(nil! y)))

(defun my-macroexpand-1 (expr)
  (cond
    ((macro-function (car expr))
     (funcall (macro-function (car expr)) expr nil))
    (t
     (error "don't know that macro ~A" (car expr)))))

(my-macroexpand-1 '(nil! y))

(defmacro repeat (expr repetitions)
  (format t "expr: ~S~%" expr)
  (format t "repetitions: ~S~%" repetitions)
  (let ((n (gensym "N")))
    `(let ((,n ,repetitions))
       (loop for i below ,n
             collect ,expr))))

(repeat (progn (print "Hi")
               42)
        (+ 1 2))
;; Hi x3
;; => (42 42 42)

;; Expansion
(let ((#:n327 (+ 1 2)))
  (loop for i below #:n327
        collect (progn (print "Hi") 42)))

;;
(quote (sb-int:quasiquote a))

(defvar a)
(defvar b)

(setf a 1 b 2)

`(a is ,a and b is ,b)

;; same as
(list 'a 'is a 'and b 'is b)

(trace sb-impl::expand-quasiquote)
`(a is ,a and b is ,b)

;; 0: (SB-IMPL::EXPAND-QUASIQUOTE (A IS #S(SB-IMPL::COMMA :EXPR A :KIND 0) AND B IS #S(SB-IMPL::COMMA :EXPR B :KIND 0)) NIL)
;; 0: SB-IMPL::EXPAND-QUASIQUOTE returned (LIST 'A 'IS A 'AND 'B 'IS B)

(defvar lst)
(setf lst '(a b c))
`(its elements are ,@lst)
;; 0: (SB-IMPL::EXPAND-QUASIQUOTE (ITS ELEMENTS ARE #S(SB-IMPL::COMMA :EXPR LST :KIND 2)) NIL)
;; 0: SB-IMPL::EXPAND-QUASIQUOTE returned (LIST* 'ITS 'ELEMENTS 'ARE LST)
;; notice the :KIND 2

(setf lst '(2 3))
`(1 ,@lst 4)
