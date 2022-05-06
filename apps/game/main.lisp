(in-package :raylib)

(defvar shader)
(defvar target)
(defvar scale)
(defvar camera)
(defparameter screen-width 600)
(defparameter screen-height 600)
(defparameter game-screen-width 160)
(defparameter game-screen-height 128)
(defvar fps 30)
(defvar boom)
(defvar explosion)
(defvar active nil)
(defvar current-frame 0)
(defvar enemy-missiles nil)
(defvar player-missiles nil)
(defvar enemy-explosions nil)
(defvar player-explosions nil)
(defvar city-texture)
(defvar reticle-texture)
(defvar cities)
(defvar game-state :menu)

;; HELPERS

(defun get-game-mouse-x ()
  (- (/ (get-mouse-x) scale)
     (/ 60 scale)))

(defun get-game-mouse-y ()
  (- (/ (get-mouse-y) scale)
     (/ 108 scale)))

(defun get-game-mouse-position ()
  (make-vector2 :x (- (get-game-mouse-x) 3)
                :y (- (get-game-mouse-y) 3)))

(defun dist-sq (x y)
  (+ (* x x) (* y y)))

(defun unit-vector (x1 y1 x2 y2)
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (mag (sqrt (dist-sq dx dy))))
    (values (/ dx mag) (/ dy mag))))

;; MODEL

(defstruct missile start-pos pos direction target)
(defstruct explosion x y r dr)

(defun check-collision-missile-explosion (m e)
  (with-slots (x y r) e
    (check-collision-circle-rec (make-vector2 :x x :y y)
                                r
                                (destructuring-bind (x y) (missile-pos m)
                                  (make-rectangle :x x
                                                  :y y
                                                  :width 2
                                                  :height 2)))))

(defun missile-reached-target? (missile)
  (flet ((dist (v1 v2)
           (dist-sq (- (first v1) (first v2))
                    (- (second v1) (second v2)))))
    (with-slots (pos start-pos) missile
      (>= (dist start-pos pos)
          (dist start-pos (missile-target missile))))))

;; UPDATE

(defun setup ()
  (setf cities (list 5 50))
  (setf enemy-missiles nil)
  (setf player-missiles nil)
  (setf enemy-explosions nil))

(defun launch-enemy-missile ()
  (let ((x1 (random game-screen-width))
        (y1 0)
        (x2 (random game-screen-width))
        (y2  (- game-screen-height 4)))
    (push (make-missile :start-pos (list x1 y1)
                        :pos (list x1 y1)
                        :direction (multiple-value-list (unit-vector x1 y1 x2 y2)))
          enemy-missiles)))

(defun launch-player-missile ()
  (let* ((x2 (get-game-mouse-x))
         (y2 (get-game-mouse-y))
         (x1 (floor (/ game-screen-width 2)))
         (y1 (- game-screen-height 4)))
    (push (make-missile :start-pos (list x1 y1)
                        :pos (list x1 y1)
                        :target (list x2 y2)
                        :direction (multiple-value-list (unit-vector x1 y1 x2 y2)))
          player-missiles)))

(defun update-missile (m &key (speed 0.5))
  (with-slots (pos direction) m
    (destructuring-bind (dx dy) direction
      (incf (first pos) (* speed dx))
      (incf (second pos) (* speed dy)))))

(defun update-explosion (e)
  (with-slots (x y r dr) e
    (setf r (+ r (* (get-frame-time) dr)))
    (when (>= r 10.0)
      (setf dr -10.0))))

(defun update-enemy-explosions ()
  (dolist (e enemy-explosions)
    (update-explosion e))

  (setf enemy-explosions (remove-if (lambda (e) (<= (explosion-r e) 0.0)) enemy-explosions)))

(defun update-player-explosions ()
  (dolist (e player-explosions)
    (update-explosion e))

  (setf player-explosions (remove-if (lambda (e) (<= (explosion-r e) 0.0)) player-explosions)))

(defun update-enemy-missiles ()
  (when (is-key-pressed :x)
    (launch-enemy-missile))
  (dolist (m enemy-missiles)
    (update-missile m))

  (setf enemy-missiles
        (remove-if (lambda (missile)
                     (with-slots (pos) missile
                       (when (or (>= (second pos) (- game-screen-height 4)) ;; hit ground
                                 (some (lambda (e)
                                         (check-collision-missile-explosion missile e))
                                       enemy-explosions)
                                 (some (lambda (e)
                                         (check-collision-missile-explosion missile e))
                                       player-explosions))
                         (destructuring-bind (x y) pos
                           (push (make-explosion :x (floor x)
                                                 :y (floor y)
                                                 :r 1.0
                                                 :dr 10.0)
                                 enemy-explosions))
                         t)))
                   enemy-missiles)))

(defun explode-player-missile (missile)
  (push (with-slots (pos) missile
          (destructuring-bind (x y) pos
            (make-explosion :x (floor x)
                            :y (floor y)
                            :r 1.0
                            :dr 10.0)))
        player-explosions))

(defun update-player-missiles ()
  (when (is-mouse-button-pressed :left)
    (launch-player-missile)
    player-missiles)

  (dolist (m player-missiles)
    (update-missile m :speed 3))

  (setf player-missiles
        (remove-if (lambda (missile)
                     (when (missile-reached-target? missile)
                       (explode-player-missile missile)
                       t))
                   player-missiles)))

;; VIEW

(defun draw-frame ()
  (begin-drawing)
  (begin-mode-2d camera)
  (clear-background +purple+)
  (begin-shader-mode shader)
  (draw-texture-rec (render-texture-texture target)
                    (make-rectangle :x 0 :y 0
                                    :width game-screen-width
                                    :height (- game-screen-height))
                    (make-vector2 :x 0 :y 0)
                    +white+)
  (end-shader-mode)
  (end-mode-2d)
  (draw-fps 10 10)
  (end-drawing))

(defun menu ()
  (when (is-key-pressed :space)
    (dotimes (n 3)
      (launch-enemy-missile))
    (setf game-state :level))

  (begin-texture-mode target)
  (clear-background +black+)
  (draw-text "Press Space to begin" 10 10 10 +white+)
  (end-texture-mode)

  (draw-frame))

(defun draw-missile (missile &optional (color +red+))
  (with-slots (start-pos pos) missile
    (destructuring-bind (x y) pos
      ;; trail
      (draw-line-v (make-vector2 :x (first start-pos)
                                 :y (second start-pos))
                   (make-vector2 :x x :y y)
                   color)
      ;; head
      (draw-rectangle-rec (make-rectangle :x (- x 1)
                                          :y (- y 1)
                                          :width 2
                                          :height 2)
                          +white+))))

(defun draw-enemy-missile (missile)
  (draw-missile missile +red+))

(defun draw-player-missile (missile)
  (draw-missile missile +yellow+))

(defun draw-explosion (e)
  (with-slots (x y r) e
    (draw-circle x y (coerce r 'float) +yellow+)))

(defun draw-ground ()
  (draw-rectangle 0 (- game-screen-height 4) game-screen-width 4 +brown+))

(defun draw-cities ()
  (dolist (city cities)
    (draw-texture-rec city-texture
                      (make-rectangle :x 0 :y 0 :width 8 :height 8)
                      (make-vector2 :x city :y (- game-screen-height 8 4))
                      +white+)))

(defun draw-reticle ()
  (draw-texture-rec reticle-texture
                    (make-rectangle :x 0 :y 0 :width 5 :height 5)
                    (get-game-mouse-position)
                    +white+))

;; MAIN

(defun level ()
  (when (is-key-pressed :r)
    (setf game-state :menu)
    (setup))

  (update-enemy-explosions)
  (update-player-explosions)
  (update-enemy-missiles)
  (update-player-missiles)

  (begin-texture-mode target)
  (clear-background +black+)
  (draw-ground)
  (draw-cities)
  (dolist (e enemy-explosions)
    (draw-explosion e))
  (dolist (e player-explosions)
    (draw-explosion e))
  (dolist (m enemy-missiles)
    (draw-enemy-missile m))
  (dolist (m player-missiles)
    (draw-player-missile m))
  (draw-reticle)
  (end-texture-mode)

  (draw-frame))

(defun game-loop ()
  (if (window-should-close) (return-from game-loop))

  (ecase game-state
    (:menu
     (menu))

    (:level
     (level)))

  (game-loop))

(defun make-city-texture ()
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

(defun make-reticle-texture ()
  (let* ((image (gen-image-color 5 5 +blank+))
         (image-pointer (alloc-image image))
         (points #2A((0 0 1 0 0)
                     (0 0 1 0 0)
                     (1 1 0 1 1)
                     (0 0 1 0 0)
                     (0 0 1 0 0))))
    (loop :for j :from 0 :to 4
          :do (loop :for i :from 0 :to 4
                    :do (when (= 1 (aref points i j))
                          (image-draw-pixel image-pointer j i +white+))))
    (load-texture-from-image image)))

(defun make-camera (scale)
  (make-camera2d :offset (make-vector2 :x 60 :y 108)
                 :target (make-vector2 :x 0 :y 0)
                 :rotation 0.0
                 :zoom 3))

(defvar scanline-fragment-shader
  "#version 330
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
}")

(defun main ()
  "Make sure you don't call me inside Emacs!"
  (init-window screen-width screen-height "Defender")
  (init-audio-device)
  (unwind-protect
       (progn
         (set-target-fps fps)
         (hide-cursor)
         (setf scale (max (floor (min (/ screen-width game-screen-width)
                                      (/ screen-height game-screen-height)))
                          1.0))
         (setf camera (make-camera scale))
         (setf city-texture (make-city-texture))
         (setf reticle-texture (make-reticle-texture))
         (setf game-state :menu)
         (setup)
         (setf target (load-render-texture game-screen-width game-screen-height))
         (setf shader (load-shader-from-memory (null-pointer) scanline-fragment-shader))
         (game-loop))
    (unload-render-texture target)
    (close-audio-device)
    (show-cursor)
    (close-window)))
