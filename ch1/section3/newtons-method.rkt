#lang sicp

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

; Like average-damp deriv is a procedure that
; takes a procedure as argument and returns a
; procedure as a value.

(define (cube x) (* x x x))

((deriv cube) 5)

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

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (square x) (* x x))

(define (sqrt x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(sqrt 4)

(define (cube-root x)
  (newtons-method
   (lambda (y)
     (- (cube y) x))
   1.0))

(cube-root 8)
