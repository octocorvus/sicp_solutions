#lang racket

(require berkeley)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term
                  (next a)
                  next
                  b))))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (term x)
    (+ 1 (/ (- (square (* 2 x)) 1))))
  (* 2.0 (product term 1 inc n)))
