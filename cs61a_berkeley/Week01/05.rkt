#lang racket

(require berkeley)

(define (ends-e sent)
  (cond ((empty? sent) '())
        ((ends-with-e? (first sent))
         (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent)))))

(define (ends-with-e? wd)
  (equal? (last wd) 'e))

(ends-e '(please put the salami above the blue elephant))
