(symbol-name 'abc)

(defun |Foo| (x)
  (+ 23 x))

(|Foo| 1)

(setf (get 'alizarin 'color) 'red)
(setf (get 'alizarin 'transparency) 'high)

(get 'alizarin 'color)

(symbol-plist 'alizarin)
