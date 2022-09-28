#lang sicp

(define (iterative-improvement
         good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (square x) (* x x))

(define (average v1 v2) (/ (+ v1 v2) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improvement
    good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (let ((tolerance 0.00001))
      (< (abs (- v1 v2))
         tolerance)))
  ((iterative-improvement
    (lambda (guess)
      (close-enough? guess (f guess)))
    f)
   first-guess))
