#lang sicp

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

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

; Average-damp version of square-root

(define (sqrt1 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (square x) (* x x))

; Newton's method version of square-root

(define (sqrt2 x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))
