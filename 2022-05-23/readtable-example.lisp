(defpackage :readtable-example
  (:use :cl))

(in-package :readtable-example)

(named-readtables:in-readtable :interpol-syntax)

(let ((username "Bob"))
  #?"Hello ${username}!")

;; TODO find readtable for map/hash syntax
