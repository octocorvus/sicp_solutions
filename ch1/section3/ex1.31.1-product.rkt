#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Product of integers a through b
(define (identity x) x)

(define (inc x) (+ x 1))

(define (product-integers a b)
  (product identity a inc b))

(product-integers 1 4)

; Factorial
(define (factorial n)
  (product identity 1 inc n))

(factorial 5)
(factorial 30)

; Approximation of pi
(define (square x) (* x x))

(define (reciprocal x) (/ 1 x))

(define (pi-product a b)
  (define (reciprocal-inc x)
    (if (< x 1)
        (inc (reciprocal x))
        (reciprocal (inc x))))
  (define (calc-product a b)
    (cond ((even? a)
           (calc-product (+ a 1) b))
          ((even? b)
           (calc-product a (- b 1)))
          (else
           (product square (reciprocal a) reciprocal-inc b))))
  (* 4.0
     a
     b
     (calc-product a b)))

(pi-product 2 1000)

; Better and correct version
; Kept the old version for laughs and shits
(define (wallis-product n)
  (define (term x)
    (/ 1 (- 1 (/ 1 (* 4 (square x))))))
  (* 2.0 (product term 1 inc n)))

(wallis-product 10000)
