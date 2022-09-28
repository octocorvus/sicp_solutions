#lang sicp

; Constructors and selectors
(define (make-rat n d)
  (if (negative? d)
      (make-rat (- n) (- d))
      (let ((g (gcd n d)))
        (cons (/ n g)
              (/ d g)))))
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

; Testing
(define one-half (make-rat -1 -2))
(print-rat one-half)

(define neg-one-third (make-rat -1 3))
(print-rat neg-one-third)

(define neg-four-third (make-rat 4 -3))
(print-rat neg-four-third)

(print-rat
 (mul-rat one-half neg-four-third))

(print-rat
 (sub-rat neg-four-third neg-one-third))

(print-rat
 (div-rat neg-four-third
          (sub-rat neg-one-third
                   one-half)))
