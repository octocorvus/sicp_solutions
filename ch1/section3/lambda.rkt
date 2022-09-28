#lang sicp

; (lambda (<formal parameter>) <body>)

(lambda (x) (+ x 4))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; This:
(define (plus4 x) (+ x 4))
; is equivalent to
(define plus4 (lambda (x) (+ x 4)))
