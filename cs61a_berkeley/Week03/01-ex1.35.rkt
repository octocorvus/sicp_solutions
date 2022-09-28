#lang racket

(require berkeley)

(define (fixed-point fn guess)
  (let ((next (fn guess)))
    (if (close-enough? guess next)
        next
        (fixed-point fn next))))

(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2))
     tolerance))

(define ϕ
  (fixed-point (lambda (x)
                 (+ 1 (/ x)))
               1.0))
(define golden-ratio ϕ)
