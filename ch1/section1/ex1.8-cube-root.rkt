#lang sicp

(define (cube-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cube-root-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess next-guess)
  (< (/ (abs (- guess next-guess)) guess) 0.00001))

(define (cube-root x)
  (define root (cube-root-iter 1.0 (abs x)))
  (if (< x 0.0)
      (- root)
      root))

(define (square x)
  (* x x))

; Tests
(cube-root 1)
(cube-root -1)
(cube-root 8)
(cube-root -8)
(cube-root 0)
(cube-root 345474624772652756782672662684787676767672976267267267)
(cube-root 100000000000000000000000000000000000000000000000000000)
(cube-root 0.0000000000000000000000000000000000000000000000000069)
