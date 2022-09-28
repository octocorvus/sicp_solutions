#lang sicp

(define (last-pair list-instance)
  (if (null? (cdr list-instance))
      (list (car list-instance))
      (last-pair (cdr list-instance))))

(last-pair (list 23 72 149 34))
