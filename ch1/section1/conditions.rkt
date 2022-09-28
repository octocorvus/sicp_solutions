#lang sicp

(define (abs1 x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; Another way to write the absolute-value procedure is:
(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))
; This could be expressed in English as "If x is less than zero return -x,
; otherwise return x."
; else is a special symbol that can be used in place of the <p> in the
; final clause of a cond. This causes the cond to return as its value the
; value of the corresponding <e> whenever all the previous clauses have
; been bypassed. In fact, any expression that always evaluates to a true
; value can be used as the <p> here.

; Yet another way to do the same thing.
(define (abs3 x)
  (if (< x 0)
      (- x)
      x))
