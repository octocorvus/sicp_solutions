#lang sicp

; We use gcd procedure to reduce the numerator and
; denominator to lowest terms before constructing the
; pair. This modification was accomplished by changing
; the constructor make-rat without changing any of the
; procedures (such as add-rat and mul-rat) that
; implement the actual operations.

; Constructors and selectors.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

; Operations on rational-numbers.
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat
 (add-rat one-half one-third))

(print-rat
 (mul-rat one-half one-third))

; Our rational-number implementation does not reduce
; rational numbers to lowest terms. We can remedy this
; by changing make-rat.
(print-rat
 (add-rat one-third one-third))
