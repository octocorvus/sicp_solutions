#lang sicp

(define (average x y) (/ (+ x y) 2))

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

(display
 "** Fixed point without average damping **\n")
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2)

(display "\n** Fixed point with average damping **\n")
(fixed-point
 (lambda (x)
   (average x (/ (log 1000) (log x))))
 2)
