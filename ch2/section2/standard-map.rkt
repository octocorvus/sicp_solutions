#lang sicp

(map +
     (list 1 2 3)
     (list 40 50 60)
     (list 700 800 900))

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))
