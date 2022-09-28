#lang sicp

(define (f n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a
                 (* 2 b)
                 (* 3 c))
              a
              b
              (- count 1))))
  (iter 2 1 0 n))

(f 0)
(f 1)
(f 2)
(f 3)
(f 10)
(f 100)
