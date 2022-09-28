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

(define (divisible? x y)
  (= (remainder x y) 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (divisible? (- i 2) 3)
                 (* 2 (+ 1
                         (/ (- i 2)
                            3)))
                 1.0))
           10)
