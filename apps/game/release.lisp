(ql:quickload :raylib)
(ql:quickload :nibbles)

(load "wav.lisp")
(load "sound.lisp")
(load "sprite.lisp")
(load "font.lisp")
(load "main.lisp")

(sb-ext:save-lisp-and-die "defender" :executable t :toplevel 'raylib::main)
