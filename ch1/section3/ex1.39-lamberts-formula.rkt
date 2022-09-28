#lang sicp

(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1)
              (/ (n k)
                 (+ (d k)
                    result)))))
  (iter k 0))

(define (square x) (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i)
               (- (* 2.0 i) 1.0))
             k))
