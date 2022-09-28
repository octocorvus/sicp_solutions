#lang sicp

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f x y z)
  (cond ((and (> x y) (> z y)) (sum-of-squares x z))
        ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> y x) (> z x)) (sum-of-squares y z))))

(f 1 2 3)
(f 3 1 2)
(f 2 3 1)
