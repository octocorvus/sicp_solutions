#lang sicp

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx))
          (g x))
       dx)))

(define (cube x) (* x x x))

(define (square x) (* x x))

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

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (cubic-root a b c guess)
  (newtons-method (cubic a b c)
                  guess))
