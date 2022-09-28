#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(display (cons 3 18))       (newline)
(display (car (cons 3 18))) (newline)
(display (cdr (cons 3 18))) (newline)
