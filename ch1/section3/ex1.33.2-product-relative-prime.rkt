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

; 2. The product of all the positive integers less than n
;    that are relatively prime to n.

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (product-of-relative-prime n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate
   relative-prime? * 1 identity 1 inc n))

(product-of-relative-prime 10)
