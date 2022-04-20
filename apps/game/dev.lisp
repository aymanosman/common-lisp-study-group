(ql:quickload "slynk")
(slynk:create-server :port 4005 :dont-close t)
;; git clone https://github.com/longlene/cl-raylib.git ~/.quicklisp/local-projects/cl-raylib
(load "~/.quicklisp/local-projects/cl-raylib/cl-raylib.asd")
(ql:quickload "cl-raylib")

(load "main.lisp")

(main:main)
