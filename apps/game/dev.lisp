(ql:quickload "slynk")
(slynk:create-server :port 4005 :dont-close t)

;; git clone --branch wip https://github.com/aymanosman/cl-raylib.git ~/quicklisp/local-projects/raylib
(ql:quickload :raylib)

(load "shader.lisp")
(load "main.lisp")

(in-package :raylib)

(main)
