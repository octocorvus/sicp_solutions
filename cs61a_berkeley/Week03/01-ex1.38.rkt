#lang racket

(require berkeley)

(define (cont-frac n d k)
  (define (cont-frac-rec n d i)
    (if (equal? i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (cont-frac-rec n d (add1 i))))))
  (cont-frac-rec n d 1))

(define k 20)

(define e-fractional
  (cont-frac (λ (i) 1.0)
             (λ (i)
               (let ((rem (remainder i 3)))
                 (cond ((equal? rem 1) 1)
                       ((equal? rem 2)
                        (* 2 (floor (/ (add1 i) 3))))
                       (else 1))))
             k))
(define e (+ e-fractional 2))

e
