#lang racket

(require berkeley)

(define (cont-frac n d k)
  (define (iter result n d i)
    (if (zero? i)
        result
        (iter (/ (n i)
                 (+ (d i) result))
              n
              d
              (sub1 i))))
  (iter 0 n d k))

(define k 11)

;; 1/ϕ = 0.61803398875
(cont-frac (λ (i) 1.0)
           (λ (i) 1.0)
           k)
