#lang racket

(require berkeley)

(define (every fn sent)
  (if (empty? sent)
      '()
      (se (fn (first sent))
          (every fn (bf sent)))))

(every square '(1 2 3 4))
(every first '(nowhere man))
