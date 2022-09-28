#lang sicp

; cons stands for construct
; car stands for Contents of Address part of Register
; cdr stands for Contents of Decrement part of Register
(define x (cons 1 2))

(car x)

(cdr x)

; cons can be used to form pairs whose elements are
; pairs, sond so on

(define y (cons 3 4))
(define z (cons x y))

(car (car z))

(car (cdr z))

(define a (cons 0 z))

(car a)

(car (cdr (cdr a)))
