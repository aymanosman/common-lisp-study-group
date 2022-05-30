(typep (1+ most-positive-fixnum) 'bignum)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(* most-positive-long-float 10)
