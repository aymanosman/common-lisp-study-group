(symbol-name 'abc)

(defun |Foo| (x)
  (+ 23 x))

(|Foo| 1)

(setf (get 'alizarin 'color) 'red)
(setf (get 'alizarin 'transparency) 'high)

(get 'alizarin 'color)

(symbol-plist 'alizarin)

(intern "RANDOM-SYMBOL")
(unintern 'random-symbol)

(defpackage :my-utilities
  (:use :common-lisp))

(defpackage :my-application
  (:use :common-lisp :my-utilities)
  (:nicknames :app)
  (:export :win :lose :draw))

(do-external-symbols (sym (find-package :my-application))
  (cond
    ((boundp sym)
     (format t "~a is bound to a value~%" sym))
    ((fboundp sym)
     (format t "~a is bound to a function~%" sym))))

(make-package "Mbappé")
(defvar foo-sym (intern "FOO" (find-package "Mbappé")))
(set foo-sym 42)

|Mbappé|::foo
