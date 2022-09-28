#lang sicp

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(square 21)
(square (+ 2 5))
(square (square 3))
(sum-of-squares 3 4)
(f 5)

; Substitution model for Procedure Application
; To apply a compound procedure to arguments, evaluate the body of the
; procedure with each formal parameter replaced by the corresponding
; argument.
; Let's evaluate (f 5):
; (sum-of-squares (+ 5 1) (* 5 2))
; (+ (square 6) (square 10))
; (+ (* 6 6) (* 10 10))
; (+ 36 100)
; 136

; Normal order evaluation: fully expand and then reduce
; Applicative order evaluation: evaluate the arguments and then apply
