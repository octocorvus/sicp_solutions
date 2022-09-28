#lang racket

(require berkeley)

(define (cont-frac n d k)
  (define (cont-frac-rec n d i)
    (if (equal? i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (cont-frac-rec n d (add1 i))))))
  (cont-frac-rec n d 1))

(define k 11)

;; 1/ϕ = 0.61803398875
(cont-frac (λ (i) 1.0)
           (λ (i) 1.0)
           k)
