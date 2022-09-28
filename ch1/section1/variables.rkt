#lang sicp

; In Scheme, we name things with define.
(define size 2)
; It causes the interpreter to asscociate the value 2 with the name
; size. Now we can refer to value 2 by name
size
(* 5 size)

; Further examples
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

(define circumference (* 2 pi radius))
circumference
