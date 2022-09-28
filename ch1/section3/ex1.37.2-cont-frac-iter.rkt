#lang sicp

(define (cont-frac n d k)
  (define (iter result k)
    (if (= k 0)
        result
        (iter (/ (n k)
                 (+ (d k)
                    result))
              (- k 1))))
  (iter 0 k))

(define (golden-ratio k)
  (/ 1.0
     (cont-frac (lambda (x) 1)
                (lambda (x) 1)
                k)))
