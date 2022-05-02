(defpackage #:main
  (:use #:common-lisp #:raylib)
  (:export #:main))

(in-package #:main)

(defvar shader)
(defvar target)
(defvar scale)
(defvar game-screen-width 200)
(defvar game-screen-height 200)
(defvar fps 30)
(defvar boom)
(defvar explosion)
(defvar active nil)
(defvar current-frame 0)
(defvar missiles nil)
(defvar missile-speed 1)
(defvar city-texture)
(defvar cities)
(defvar game-state :menu)

(defun draw-missile (missile)
  (with-slots (start-pos pos) missile
    (destructuring-bind (x y) pos
      (draw-line (round (first start-pos))
                 (round (second start-pos))
                 (round x)
                 (round y)
                 +red+))))

(defun center-pos (pos)
  (make-vector2 :x (- (vector2-x pos) (/ (texture-width explosion) 5 2))
                :y (- (vector2-y pos) (/ (texture-height explosion) 5 2))))

(defstruct missile start-pos pos direction)

(defun add-missile (origin target)
  (let* ((dx (- (vector2-x target) (vector2-x origin)))
         (dy (- (vector2-y target) (vector2-y origin)))
         (mag (sqrt (dist-sq dx dy)))
         (unit (list (/ dx mag) (/ dy mag))))
    (push (make-missile :start-pos (list (vector2-x origin)
                                         (vector2-y origin))
                        :pos (list (vector2-x origin)
                                   (vector2-y origin))
                        :direction unit)
          missiles)))

(defun update-missile (m)
  (with-slots (pos direction) m
    (destructuring-bind (dx dy) direction
      (incf (first pos) (* missile-speed dx))
      (incf (second pos) (* missile-speed dy)))))

(defun dist-sq (x y)
  (+ (* x x) (* y y)))

(defun remove-missile (m)
  (setf missiles (remove m missiles)))

(defun get-game-mouse-position ()
  (let ((w (get-screen-width))
        (h (get-screen-height))
        (pos (get-mouse-position)))
    (make-vector2 :x (* (/ (vector2-x pos) w) game-screen-width)
                  :y (* (/ (vector2-y pos) h) game-screen-height))))

(defun draw-ground ()
  (draw-rectangle 0 (- game-screen-height 4) game-screen-width 4 +brown+))

(defun draw-cities ()
  (dolist (city cities)
    (draw-texture-rec city-texture
                      (make-rectangle :x 0 :y 0 :width 8 :height 8)
                      (make-vector2 :x city :y (- game-screen-height 8 4))
                      +white+)))

(defun game-loop ()
  (if (window-should-close) (return-from game-loop))

  (ecase game-state
    (:menu

     ;; press enter -> goto level
     (when (is-key-pressed :space)
       (dotimes (n 3)
         (add-missile (make-vector2 :x (random game-screen-width) :y 0)
                      (make-vector2 :x (random game-screen-width) :y (- game-screen-height 4))))
       (setf game-state :level))

     (begin-texture-mode target)
     (clear-background +black+)
     (draw-text "Press Space to begin" 10 10 10 +white+)
     (end-texture-mode)

     (begin-drawing)
     (clear-background +black+)
     (begin-shader-mode shader)
     (draw-texture-pro (render-texture-texture target)
                       (make-rectangle :x 0 :y 0
                                       :width game-screen-width :height (- game-screen-height))
                       (make-rectangle :x 0 :y 0
                                       :width (* game-screen-width scale)
                                       :height (* game-screen-height scale))
                       (make-vector2 :x 0 :y 0)
                       0.0
                       +white+)

     (end-shader-mode)
     (draw-fps 10 10)
     (end-drawing))
    ((:level :play)

     (when (is-key-pressed :r)
       (setf game-state :menu)
       (setf missiles nil))

     (begin-texture-mode target)
     (clear-background +black+)
     (draw-ground)
     (draw-cities)

     (dolist (m missiles)
       (draw-missile m)
       (update-missile m)

       (when (or (with-slots (pos) m
                   (>= (second pos) (- game-screen-height 4))))
         (remove-missile m)))

     (end-texture-mode)

     (begin-drawing)
     (clear-background +black+)
     (begin-shader-mode shader)

     (draw-texture-pro (render-texture-texture target)
                       (make-rectangle :x 0 :y 0
                                       :width game-screen-width :height (- game-screen-height))
                       (make-rectangle :x 0 :y 0
                                       :width (* game-screen-width scale)
                                       :height (* game-screen-height scale))
                       (make-vector2 :x 0 :y 0)
                       0.0
                       +white+)

     (end-shader-mode)
     (draw-fps 10 10)
     (end-drawing)))

  (game-loop))

(defun init-cities ()
  (setf cities (list 5 50)))

(defun main ()
  "Make sure you don't call me inside Emacs!"
  (init-window 400 400 "game")
  (init-audio-device)
  (unwind-protect
       (progn
         (set-target-fps fps)
         (setf scale (max (floor (min (/ 400 game-screen-width)
                                      (/ 400 game-screen-height))) 1.0))
         (setf boom (load-sound "boom.wav"))
         (setf explosion (load-texture "explosion.png"))
         (setf city-texture
               (let* ((image (gen-image-color 8 8 +blank+))
                      (image-pointer (alloc-image image))
                      (points #2A((0 0 0 0 1 1 0 0)
                                  (0 0 0 0 1 1 0 0)
                                  (0 0 1 1 1 1 0 0)
                                  (0 0 1 1 1 1 0 0)
                                  (1 1 1 1 1 1 0 0)
                                  (1 1 1 1 1 1 1 1)
                                  (1 1 1 1 1 1 1 1)
                                  (1 1 1 1 1 1 1 1))))
                 (loop :for j :from 0 :to 7
                       :do (loop :for i :from 0 :to 7
                                 :do (when (= 1 (aref points i j))
                                       (image-draw-pixel image-pointer j i +blue+))))
                 (load-texture-from-image image)))
         (setf cities (init-cities))
         (setf missiles nil)
         (setf game-state :menu)
         (setf target (load-render-texture game-screen-width game-screen-height))
         (setf shader (load-shader-from-memory (cffi:null-pointer) "#version 330
precision mediump float;

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

void main() {
    vec3 pixel = texture(texture0, fragTexCoord).rgb;

    // every 3rd pixel should be a scanline
    float fmin = 0.50;
    float fmod = mod(gl_FragCoord.y, 3.0);
    float fstep = fmin + (1.0 - fmin) * fmod;

    // alpha the color by the scanline
    finalColor = vec4(pixel, fstep);
}"))


         (begin-texture-mode target)
         (clear-background +black+)
         (end-texture-mode)

         (game-loop))
    ;; (unload-render-texture taget)
    (close-audio-device)
    (close-window)))
