#lang sicp

; f(x, y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y)

(define (square x) (* x x))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
