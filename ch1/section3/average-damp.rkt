#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess steps)
    (display steps)
    (display ". Trying ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ steps 1)))))
  (try first-guess 1))

(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define (square x) (* x x))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x (square y))))
   1.0))
