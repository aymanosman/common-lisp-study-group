(defpackage :main
  (:use :cl :raylib)
  (:export #:main))

(in-package :main)

(defun game-loop ()
  (if (window-should-close) (return-from game-loop))

  (with-drawing
    (clear-background +raywhite+)
    (draw-fps 10 10))

  (game-loop))

(defun main ()
  "Make sure you don't call me inside Emacs!"
  (with-window (800 450 "raylib")
    (with-audio-device
      (set-target-fps 60)
      (game-loop))))
