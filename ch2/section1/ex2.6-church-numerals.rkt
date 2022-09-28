#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))

(define three (add one two))

; Convert Church numerals to integers.
(define (conv n)
  ((n inc) 0))

(conv zero)
(conv three)
(conv (add zero (add two (add three two))))
