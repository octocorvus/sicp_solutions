#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f
               (repeated f
                         (- n 1)))))

(define (inc x) (+ x 1))

((repeated inc 10) 5)

(define (square x) (* x x))

((repeated square 2) 5)

; Takes too much time and memory, not ideal
((repeated inc 10000000) 5)
