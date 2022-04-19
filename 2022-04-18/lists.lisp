;; COMPRESS

(defun compress-aux (elem num lst)
  (if (null lst)
      (if (= 1 num)
          (list elem)
          (list (list num elem)))
      (if (eql (car lst) elem)
          (compress-aux elem (+ num 1) (cdr lst))
          (cons (if (= 1 num)
                    elem
                    (list num elem))
                (compress-aux (car lst) 1 (cdr lst))))))

(defun compress (lst)
  (if (null lst)
      nil
      (compress-aux (car lst) 1 (cdr lst))))

(compress '(1 1 1 0 1 0 0 0 0 1))

;; ((3 1) 0 1 (4 0) 1)

(defvar compressed (compress '(1 1 1 0 1 0 0 0 0 1)))

;; 3.6
(defun uncompress (lst)
  (if (null lst)
      nil
      (append (let ((x (first lst)))
                (cond
                  ((consp x)
                   (make-list (first x) :initial-element (second x)))
                  (t
                   (list x))))

              (uncompress (rest lst)))))

(uncompress compressed)


(mapcar (lambda (x y) (+ x y)) (list 1 2 3) (list 3 2 1))

(macroexpand-1 '(push 1 stack))

(defun push-stack (obj stack-symbol)
  (setf (symbol-value stack-symbol) (cons obj (symbol-value stack-symbol))))

;; DOTTED LIST

(defun proper-list? (lst)
  (cond
    ((null lst) t)
    ((consp lst) (proper-list? (cdr lst)))
    (t nil)))

;; SHORTEST PATH

;; elem = (node . neighbors)
;; graph = (elem ...)
(defvar graph '((a b c) (b c) (c d)))

(defun shortest-path-aux (graph queue end)
  (if (null queue)
      nil
      (let ((path (first queue)))
        (if (eql (first path) end)
            (reverse path)
            (shortest-path-aux graph
                               ;; TODO show stickers
                               (append (cdr queue)
                                       (mapcar (lambda (neigh) (cons neigh path))
                                               (cdr (assoc (first path) graph))))
                               end)))))

(defun shortest-path (graph start end)
  (shortest-path-aux graph (list (list start)) end))

;; shortest-path g 'a 'd => '(a c d)

;; > (shortest-path-aux graph (list (list 'a)) 'd)
;; ((B A) (C A))
;; > (shortest-path-aux graph #v73 'd)
;; ((C A) (C B A))
;; > (shortest-path-aux graph #v74 'd)
;; ((C B A) (D C A))
;; > (shortest-path-aux graph #v75 'd)
;; ((D C A) (D C B A))

;; 0: (SHORTEST-PATH-AUX ((A B C) (B C) (C D)) ((A)) D)
;;   1: (SHORTEST-PATH-AUX ((A B C) (B C) (C D)) ((B A) (C A)) D)
;;     2: (SHORTEST-PATH-AUX ((A B C) (B C) (C D)) ((C A) (C B A)) D)
;;       3: (SHORTEST-PATH-AUX ((A B C) (B C) (C D)) ((C B A) (D C A)) D)
;;         4: (SHORTEST-PATH-AUX ((A B C) (B C) (C D)) ((D C A) (D C B A)) D)
;;         4: SHORTEST-PATH-AUX returned (A C D)
;;       3: SHORTEST-PATH-AUX returned (A C D)
;;     2: SHORTEST-PATH-AUX returned (A C D)
;;   1: SHORTEST-PATH-AUX returned (A C D)
;; 0: SHORTEST-PATH-AUX returned (A C D)


(defun shorter (g q e)
  (if (null q)
      nil
      (let* ((path (first q))
             (node (first path)))
        (if (eql node e)
            (reverse path)
            (shorter g
                     (append (cdr q)
                             (mapcar (lambda (node)
                                       (when (eql node e)
                                         (return-from shorter (reverse (cons node path))))
                                       (cons node path))
                                     (cdr (assoc node g))))
                     e)))))

;; 0: (SHORTER <graph> ((A)) D)
;;   1: (SHORTER <graph> ((B A) (C A)) D)
;;     2: (SHORTER <graph> ((C A) (C B A)) D)
;;     2: SHORTER returned (A C D)
;;   1: SHORTER returned (A C D)
;; 0: SHORTER returned (A C D)
