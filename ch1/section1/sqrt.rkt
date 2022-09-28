#lang sicp

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; Tests for sqrt
(sqrt 0)
(sqrt 9) ; 3
(sqrt 2) ; 1.4142135623730950488016887242097
(sqrt 3.14) ; 1.7720045146669350401991125097536
(sqrt 0.01) ; 0.1
(sqrt 0.0001) ; 0.01
;(sqrt 100000000000000000000000000000000000000000000000000) `; 10^50
