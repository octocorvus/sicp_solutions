#lang racket

(require berkeley)

(define (last-pair a-list)
  (if (null? (cdr a-list))
      a-list
      (last-pair (cdr a-list))))

(last-pair (list 23 72 149 34))
