#lang sicp

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 20)
(fib 100)
