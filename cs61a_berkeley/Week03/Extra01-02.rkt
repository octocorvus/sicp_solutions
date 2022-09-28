#lang racket

(require berkeley)

(define (number-of-partitions integer)
  (define (nop integer first-partition)
    (cond ((zero? integer) 1)
          ((or (negative? integer)
               (zero? first-partition)) 0)
          (else (+ (nop (- integer first-partition) first-partition)
                   (nop integer (sub1 first-partition))))))
  (nop integer integer))

;; Counting partitions is like making change, where the coins are
;; positive integers less than or equal to the given integer and
;; amount is the integer iteself.
