#lang racket

(require berkeley)

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

;; product times b raise to the power counter is
;; always equal to b raise to the power n.
