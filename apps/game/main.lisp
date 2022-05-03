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
(defvar enemy-explosions nil)
(defvar player-explosions nil)
(defvar missiles nil)
(defvar missile-speed 1)
(defvar city-texture)
(defvar cities)
(defvar game-state :menu)

(defstruct missile start-pos pos direction)
(defstruct explosion x y r dr)

(defun draw-missile (missile)
  (with-slots (start-pos pos) missile
    (destructuring-bind (x y) pos
      ;; trail
      (draw-line-v (make-vector2 :x (first start-pos)
                                 :y (second start-pos))
                   (make-vector2 :x x :y y)
                   +red+)
      ;; head
      (draw-rectangle-rec (make-rectangle :x (- x 1)
                                          :y (- y 1)
                                          :width 2
                                          :height 2)
                          +white+))))

(defun dist-sq (x y)
  (+ (* x x) (* y y)))

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

(defun add-random-missile ()
  (add-missile (make-vector2 :x (random game-screen-width) :y 0)
               (make-vector2 :x (random game-screen-width) :y (- game-screen-height 4))))

(defun update-missile (m)
  (with-slots (pos direction) m
    (destructuring-bind (dx dy) direction
      (incf (first pos) (* missile-speed dx))
      (incf (second pos) (* missile-speed dy)))))

(defun remove-missile (m)
  (setf missiles (remove m missiles)))

(defun get-game-mouse-position ()
  (let ((w (get-screen-width))
        (h (get-screen-height))
        (pos (get-mouse-position)))
    (make-vector2 :x (* (/ (vector2-x pos) w) game-screen-width)
                  :y (* (/ (vector2-y pos) h) game-screen-height))))

(defun update-explosion (e)
  (with-slots (x y r dr) e
    (setf r (+ r (* (get-frame-time) dr)))
    (when (>= r 10.0)
      (setf dr -10.0))))

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

(defun draw-scene ()
  (begin-drawing)
  (begin-mode-2d camera)
  (clear-background +black+)
  (begin-shader-mode shader)
  (draw-texture-rec (render-texture-texture target)
                    (make-rectangle :x 0 :y 0
                                    :width game-screen-width
                                    :height (- game-screen-height))
                    (make-vector2 :x 0 :y 0)
                    +white+)
  #+nil
  (draw-texture-pro (render-texture-texture target)
                    (make-rectangle :x 0 :y 0
                                    :width game-screen-width
                                    :height (- game-screen-height))
                    (make-rectangle :x 0 :y 0
                                    ;; :width (* game-screen-width scale)
                                    :width screen-width
                                    ;; :height (* game-screen-height scale)
                                    :height screen-height)
                    (make-vector2 :x 0 :y 0)
                    0.0
                    +white+)

  (end-shader-mode)
  (end-mode-2d)
  (draw-fps 10 10)
  (end-drawing))

(defun draw-menu ()
  (begin-texture-mode target)
  (clear-background +black+)
  (draw-text "Press Space to begin" 10 10 10 +white+)
  (end-texture-mode))

(defun draw-level ()
  (begin-texture-mode target)
  (clear-background +black+)
  (draw-ground)
  (draw-cities)
  (dolist (e enemy-explosions)
    (draw-explosion e))
  (dolist (m missiles)
    (draw-missile m))
  (end-texture-mode))

(defun setup ()
  (setf cities (list 5 50))
  (setf missiles nil)
  (setf enemy-explosions nil))

(defun update-level ()
  (when (is-key-pressed :r)
    (setf game-state :menu)
    (setup))

  (dolist (e enemy-explosions)
    (update-explosion e))

  (setf enemy-explosions (remove-if (lambda (e) (<= (explosion-r e) 0.0)) enemy-explosions))

  (dolist (m missiles)
    (update-missile m)
    (with-slots (pos) m
      (when (>= (second pos) (- game-screen-height 4))
        (remove-missile m)
        (destructuring-bind (x y) pos
          (push (make-explosion :x (floor x) :y (floor y) :r 1.0 :dr 10.0) enemy-explosions))))))

(defun update-menu ()
  (when (is-key-pressed :space)
    (dotimes (n 3)
      (add-missile (make-vector2 :x (random game-screen-width) :y 0)
                   (make-vector2 :x (random game-screen-width) :y (- game-screen-height 4))))
    (setf game-state :level)))

(defun game-loop ()
  (if (window-should-close) (return-from game-loop))

  (ecase game-state
    (:menu
     (update-menu)
     (draw-menu)
     (draw-scene))

    (:level
     (update-level)
     (draw-level)
     (draw-scene)))

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

(defun make-camera (scale)
  (make-camera2d :offset (make-vector2 :x 60 :y 108)
                 :target (make-vector2 :x 0 :y 0)
                 :rotation 0.0
                 :zoom 3))

(defun main ()
  "Make sure you don't call me inside Emacs!"
  (init-window screen-width screen-height "Defender")
  (init-audio-device)
  (unwind-protect
       (progn
         (set-target-fps fps)
         (setf scale (max (floor (min (/ screen-width game-screen-width)
                                      (/ screen-height game-screen-height)))
                          1.0))
         (setf camera (make-camera scale))
         (setf city-texture (make-city-texture))
         (setf game-state :menu)
         (setup)
         (setf target (load-render-texture game-screen-width game-screen-height))
         (setf shader (load-shader-from-memory (cffi:null-pointer) scanline-fragment-shader))
         (game-loop))
    ;; (unload-render-texture taget)
    (close-audio-device)
    (close-window)))
