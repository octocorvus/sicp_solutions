#lang sicp

(define (cont-frac n d k)
  (define (recurse i)
    (/ (n i)
       (+ (d i)
          (if (= i k)
              0
              (recurse (+ i 1))))))
  (recurse 1))

(define (golden-ratio k)
  (/ 1.0
     (cont-frac (lambda (x) 1)
                (lambda (x) 1)
                k)))
