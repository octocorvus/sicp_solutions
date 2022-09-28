#lang racket

(require berkeley)

(define (p1)
  (= 1 1))

(define (p2)
  (display 'LOL)
  (= 69 420))

; If this prints LOL, or is not a special form, otherwise or is
; a special form.
(or (p1) (p2))
