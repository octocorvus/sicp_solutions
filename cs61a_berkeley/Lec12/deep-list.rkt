#lang racket

(define (deep-map fn lol)
  (if (list? lol)
      (map (lambda (element) (deep-map fn element))
           lol)
      (fn lol)))

(define lol
  '((john lennon)
    (paul accartney)
    (george harrison)
    (ringo starr)))
