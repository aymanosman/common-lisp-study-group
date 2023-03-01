(defstruct rectangle
  height width)

(defstruct circle
  radius)

(defun area (x)
  (cond
    ((rectangle-p x)
     (* (rectangle-height x) (rectangle-width x)))
    ((circle-p x)
     (* pi (expt (circle-radius x) 2)))))

(area (make-rectangle :height 4
                      :width 3))

(setf (find-class 'rectangle) nil
      (find-class 'circle) nil)

(dolist (sym '(circle circle-radius (setf circle-radius)))
  (fmakunbound sym))

(defclass rectangle ()
  ((height
    :initarg :height)
   (width
    :initarg :width)))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle))
  (with-slots (height width) x
    (* height width)))

(defmethod area ((x circle))
  (with-slots (radius) x
    (* pi (expt radius 2))))

(area (make-instance 'rectangle
                     :height 2
                     :width 3))

(defclass colored ()
  (color))

(defclass colored-circle (circle colored)
  ())

(declaim (optimize (safety 3)))

(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform (random 10)
           ;; Will type check if (saftey 3)
           :type real
           :documentation "The radius of the circle")
   (center :accessor circle-center)))

(defmethod print-object ((object circle)
                         stream)
  (print-unreadable-object (object stream :type t :identity t)
    (macrolet ((slot* (name)
                 `(and (slot-boundp object ',name) (slot-value object ',name))))
      (format stream ":RADIUS ~S :CENTER ~S" (slot* radius) (slot* center)))))

(defclass tabloid ()
  ((top-story :accessor tabloid-story
              :allocation :class)))

(defvar daily-blab (make-instance 'tabloid))
(defvar unsolicited-mail (make-instance 'tabloid))

(setf (tabloid-story daily-blab) 'adultery-of-senator)

(tabloid-story unsolicited-mail)

;; 11.4
