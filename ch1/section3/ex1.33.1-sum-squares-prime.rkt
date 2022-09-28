#lang sicp

(define (filtered-accumulate
         filter? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result
                        (if (filter? a)
                            (term a)
                            null-value)))))
  (iter a null-value))

; 1. Sum of squares of prime numbers from a to b

(define (sum-of-squares-prime a b)
  (filtered-accumulate
   prime? + 0 square a inc b))

(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (< n 2)
      false
      (= n (smallest-divisor n))))

(sum-of-squares-prime 10 20)
