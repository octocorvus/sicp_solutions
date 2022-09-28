#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (procedure-square g)
  (compose g g))

(define (identity x) x)

(define (repeated f n)
  (define (iter result f n)
    (cond ((= n 0)
           result)
          ((even? n)
           (iter result
                 (procedure-square f)
                 (/ n 2)))
          (else
           (iter (compose result f)
                 f
                 (- n 1)))))
  (iter identity f n))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess steps)
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

(define (square x) (* x x))

(define (expt base exp)
  (define (iter result base exp)
    (cond ((= exp 0)
           result)
          ((even? exp)
           (iter result
                 (square base)
                 (/ exp 2)))
          (else
           (iter (* result base)
                 base
                 (- exp 1)))))
  (iter 1 base exp))

(define (nth-root x n)
  (fixed-point-of-transform
   (lambda (y)
     (/ x (expt y (- n 1))))
   (repeated average-damp
             (round (/ (log n)
                       (log 2))))
   1.0))
