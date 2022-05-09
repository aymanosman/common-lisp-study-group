(defvar ht)
(setf ht (make-hash-table))

(setf (gethash 'color ht) 'red)

(gethash 'color ht)

(remhash 'color ht)

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)

(maphash (lambda (k v)
           (format t "~a = ~a~%" k v))
         ht)
