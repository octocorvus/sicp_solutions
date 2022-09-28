#lang racket

(require berkeley)

(define (filtered-accumulate
         predicate combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((predicate a)
         (combiner (term a)
                   (filtered-accumulate
                    predicate combiner null-value term (next a) next b)))
        (else (filtered-accumulate
               predicate combiner null-value term (next a) next b))))

(define (sum-of-squares-prime a b)
  (filtered-accumulate
   prime? + 0 square a inc b))

(define (product-of-relative-primes n)
  (filtered-accumulate
   (lambda (k)
     (relative-prime? k n))
   * 1 identity 1 inc n))

(define (relative-prime? a b)
  (= (gcd a b) 1))
