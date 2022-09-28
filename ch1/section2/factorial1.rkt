#lang sicp

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(factorial 1)
(factorial 6)
(factorial 20)
(factorial 100)
