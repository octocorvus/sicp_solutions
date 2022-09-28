#lang racket

(require berkeley)

(define (ordered? numbers)
  (cond ((empty? numbers) true)
        ((empty? (bf numbers)) true)
        ((<= (first numbers)
             (first (bf numbers)))
         (ordered? (bf numbers)))
        (else false)))

(ordered? '())
(ordered? '(1))
(ordered? '(1 2))
(ordered? '(2 1))
(ordered? '(-5 0 6))
(ordered? '(-9 7 2))
(ordered? '(8 -7 12))
(ordered? '(-4 -5 -8))
(ordered? '(1 2 23 25 72 88 99 112 88))
(ordered? '(1 2 23 25 72 88 99 112 188))
