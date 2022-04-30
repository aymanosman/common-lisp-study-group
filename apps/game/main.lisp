(defpackage #:main
  (:use #:common-lisp #:raylib)
  (:export #:main))

(in-package #:main)

(defvar boom)
(defvar explosion)
(defvar active nil)
(defvar current-frame 0)
(defvar explosions nil)

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

(defun center-pos (pos)
  (make-vector2 :x (- (vector2-x pos) (/ (texture-width explosion) 5 2))
                :y (- (vector2-y pos) (/ (texture-height explosion) 5 2))))

(defun game-loop ()
  (if (window-should-close) (return-from game-loop))

  (when (and (is-mouse-button-pressed :left) (not active))
    (setf active t)
    (push (list 0 (center-pos (get-mouse-position)))
          explosions))

  (when active
    (setf active nil)
    (play-sound boom))

  (begin-drawing)

  (clear-background +raywhite+)

  (dolist (e explosions)
    (draw-explosion (first e) (second e))
    (incf (first e))
    (when (> (first e) 24)
      (setf explosions (remove e explosions))))

  (draw-fps 10 10)

  (end-drawing)

  (game-loop))

(defun main ()
  "Make sure you don't call me inside Emacs!"
  (init-window 800 450 "raylib")
  (init-audio-device)
  (unwind-protect
       (progn
         (set-target-fps 30)
         (setf boom (load-sound "boom.wav"))
         (setf explosion (load-texture "explosion.png"))
         (game-loop))
    (close-audio-device)
    (close-window)))
