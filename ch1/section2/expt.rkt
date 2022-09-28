#lang sicp

(define (square x) (* x x))

(define (expt base exp)
  (define (iter result base exp)
    (cond ((= exp 0)
           result)
          ((even? exp)
           (iter result
                 (square base)
                 (/ exp 2)))
          (else
           (iter (* result base)
                 base
                 (- exp 1)))))
  (iter 1 base exp))

(define (expmod base exp m)
  (define (iter result base exp)
    (cond ((= exp 0)
           result)
          ((even? exp)
           (iter result
                 (remainder (square base)
                            m)
                 (/ exp 2)))
          (else
           (iter (remainder (* result base)
                            m)
                 base
                 (- exp 1)))))
  (iter 1 base exp))
