#lang sicp

(define (average x y) (/ (+ x y) 2))

; We have used 0.001 as a representative "small" number
; to indicate a tolerance for the acceptable error in a
; calculation. The appropriate tolerance for a real
; calculation depend upon the problem to be solved and
; the limitations of the computer and the algorithm.
; This is often a very subtle consideration, requiring
; help from a numerical analyst or some other kind of
; magician.
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
            ((positive? test-value)
             (search f neg-point midpoint))
            ((negative? test-value)
             (search f midpoint pos-point))
            (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of
                   opposite sign" a b)))))

; Approximate pi as root between 2 and 4 of sin x = 0
(half-interval-method sin 2.0 4.0)

; Search for a root of the equation x^3-2x-3=0 between
; 1 and 2
(half-interval-method
 (lambda (x) (- (* x x x) (* 2 x) 3))
 1.0
 2.0)
