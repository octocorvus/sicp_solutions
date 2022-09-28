#lang racket

(require berkeley)

(define (perfect? number)
  (equal? number
          (sum (butlast (factors number)))))

(define (factors number)
  (define (try i)
    (cond ((> i number) '())
          ((divisible? number i)
           (se i (try (add1 i))))
          (else
           (try (add1 i)))))
  (try 1))

(define (accumulate combiner null-value sent)
  (if (empty? sent)
      null-value
      (combiner (first sent)
                (accumulate combiner
                            null-value
                            (butfirst sent)))))

(define (sum numbers)
  (accumulate + 0 numbers))

(define (next-perf number)
  (if (perfect? number)
      number
      (next-perf (add1 number))))
