(defvar s)
(setf s (open "/tmp/foo" :direction :output :if-exists :supersede))

(format s "Hello, IO~%")

(force-output s)

(setf s (open "/tmp/foo" :direction :input))

(read-line s)

(with-open-file (str "/tmp/foo" :direction :input)
  (list (read-line str)
        (read-line str)
        (read-line str)
        (read-line str)
        (read-line str)
        (read-line str)
        (read-line str)))

(with-input-from-string (s "(a #.(print 'lol) 42 :foo)")
  (let ((*read-eval* nil))
    (read s)))

(read-from-string "(a 42 :foo)")

(defstruct point x y)
(defclass point2 () ())

(defvar some-symbol)

(dolist (fun (list #'write #'prin1 #'princ #'print #'pprint))
  (print fun)
  (funcall fun (list 1 "abc"
                     (make-point :x 1 :y 2)
                     (make-instance 'point2)))
  (terpri))

(progn
  (print 1)
  (print 2))

(progn
  (prin1 1)
  (prin1 2))

(progn
  (format t "~&~a " 1)
  (format t "~&~a " 2))

(format nil "~a-~a" 1 2)

(format t "~a" "hello")
(format t "~s" "hello")

(format t "~a~%~a~%~10,2,0f" #'print #\🐂 123.456)

(progn
  (format t "~r" 4)
  (terpri))

(progn
  (format t "~@r" -10)
  (terpri))

;; vector
#(1 2 3)
;; arrays
#2a((1 2 3) (4 5 6))
;; char
#\a
;; list
'(a b c)
;; complex
#c(1 2)
;; numbers
#b101
#xfe
