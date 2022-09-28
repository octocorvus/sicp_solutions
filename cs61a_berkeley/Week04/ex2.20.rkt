#lang racket

(require berkeley)

(define (same-parity parity-integer . rest-of-integers)
  (if (even? parity-integer)
      (filter even? (cons parity-integer rest-of-integers))
      (filter odd? (cons parity-integer rest-of-integers))))
