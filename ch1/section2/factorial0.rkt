#lang sicp

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 6)
(factorial 1)
(factorial 20)
(factorial 100)
