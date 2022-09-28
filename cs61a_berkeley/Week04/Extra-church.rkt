#lang racket

(require berkeley)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add num1 num2)
  (lambda (f) (lambda (x) ((num1 f) ((num2 f) x)))))

(define (mul num1 num2)
  (lambda (f) (lambda (x) ((num1 (num2 f)) x))))

(define (expt-church base expnt)
  (expnt base))

;; alternatively
; (define (expt-church base expnt)
;   (lambda (f) (lambda (x) (((expnt base) f) x))))

(define (church->integer church-numeral)
  ((church-numeral inc) 0))
