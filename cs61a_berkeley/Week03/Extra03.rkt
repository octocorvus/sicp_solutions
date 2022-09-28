#lang racket

(require berkeley)

(define (number-of-partitions integer)
  (define (nops-iter
           total-ways
           partition-stack
           sum-of-partition-stack
           current-partition-value)
    (cond ((and (empty? partition-stack)
                (zero? current-partition-value))
           total-ways)
          ((zero? current-partition-value)
           (nops-iter total-ways
                      (butfirst partition-stack)
                      (- sum-of-partition-stack
                         (first partition-stack))
                      (sub1 (first partition-stack))))
          ((equal? integer sum-of-partition-stack)
           (nops-iter (add1 total-ways)
                      (butfirst partition-stack)
                      (- sum-of-partition-stack
                         (first partition-stack))
                      (sub1 current-partition-value)))
          ((< integer sum-of-partition-stack)
           (nops-iter total-ways
                      (butfirst partition-stack)
                      (- sum-of-partition-stack
                         (first partition-stack))
                      (sub1 current-partition-value)))
          (else
           (nops-iter total-ways
                      (se current-partition-value
                          partition-stack)
                      (+ sum-of-partition-stack
                         current-partition-value)
                      current-partition-value))))
  (nops-iter 0 '() 0 integer))
