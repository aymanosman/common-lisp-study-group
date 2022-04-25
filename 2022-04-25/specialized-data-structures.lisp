(defvar arr)
(setf arr (make-array '(2 3) :initial-element nil))

;; string
(make-array 5 :element-type 'character :initial-contents '(#\h #\e #\l #\l #\o))

(sort "elbow" #'char<)

(string-equal "Mbappé" "MbappÉ")
(string-equal "Mbappé" "Mbappà")

(format nil "~A or ~A" "truth" "dare")

(concatenate 'string "not " "to worry")

;; TODO does cl validate unicode?

(defstruct point x y)

(defvar p)

(setf p (make-point :x 42 :y 23))

(point-x p)

(point-p p)

(typep p 'point)
