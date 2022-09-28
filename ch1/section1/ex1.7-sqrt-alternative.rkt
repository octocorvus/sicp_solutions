#lang sicp

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess next-guess)
  (< (/ (abs (- guess next-guess)) guess) 0.00001))

(define (sqrt x)
  ; fix for zero, each time guess is halved, reduced by
  ; 50%, so it will never be good enough.
  (if (= x 0)
      0.0
      (sqrt-iter 1.0 x)))

(define (square x)
  (* x x))

; Tests for sqrt
(sqrt 0)
(sqrt 9) ; 3
(sqrt 2) ; 1.4142135623730950488016887242097
(sqrt 3.14) ; 1.7720045146669350401991125097536
(sqrt 0.01) ; 0.1
(sqrt 0.0001) ; 0.01
(sqrt 100000000000000000000000000000000000000000000000000) ; 10^25
