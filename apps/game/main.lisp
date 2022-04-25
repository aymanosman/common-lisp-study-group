(defpackage #:main
  (:use #:common-lisp #:raylib)
  (:export #:main))

(in-package #:main)

(defvar boom)
(defvar explosion)
(defvar active nil)
(defvar current-frame 0)
(defvar explosions nil)

(setf active nil)

(defun draw-explosion (frame pos)
  (let ((width (/ (texture-width explosion) 5))
        (height (/ (texture-height explosion) 5)))
    (multiple-value-bind (y x) (floor frame 5)
      (draw-texture-rec explosion
                        (make-rectangle :x (* x width)
                                        :y (* y height)
                                        :width width
                                        :height height)
                        pos
                        +white+))))

(defun game-loop ()
  (if (window-should-close) (return-from game-loop))

  (when (and (is-mouse-button-pressed +mouse-left-button+) (not active))
    (setf active t)
    (let ((pos (get-mouse-position)))
      (push (list 0 pos) explosions)))

  (when active
    (setf active nil)
    (play-sound boom))

  (with-drawing
    (clear-background +raywhite+)
    (dolist (e explosions)
      (draw-explosion (first e) (second e))
      (incf (first e))
      (when (> (first e) 24)
        (setf explosions (remove e explosions))))

    (draw-fps 10 10))

  (game-loop))

(defun main ()
  "Make sure you don't call me inside Emacs!"
  (with-window (800 450 "raylib")
    (with-audio-device
      (set-target-fps 60)
      (setf boom (load-sound "boom.wav"))
      (setf explosion (load-texture "explosion.png"))
      (game-loop))))
