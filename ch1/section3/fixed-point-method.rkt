#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; We can use this method to approximate the
; fixed point of the cosine function, starting
; with 1 as the initial approximation.
(fixed-point cos 1.0)

; Similarly we can find a solution to the
; equation y=sin y + cos y:
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))
