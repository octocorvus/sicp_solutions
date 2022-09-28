#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (procedure-square g)
    (compose g g))

(define (repeated f n)
  (cond ((= n 1)
         f)
        ((even? n)
         (procedure-square
          (repeated f (/ n 2))))
        (else
         (compose f
                  (repeated f
                            (- n 1))))))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

; n-fold smoothed function
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (f x) (* 2 x x))

((n-fold-smooth f 1) 2)
((n-fold-smooth f 2) 2)
((n-fold-smooth f 3) 2)
((n-fold-smooth f 4) 2)
((n-fold-smooth f 5) 2)
((n-fold-smooth f 6) 2)
((n-fold-smooth f 7) 2)
((n-fold-smooth f 8) 2)
((n-fold-smooth f 9) 2)
((n-fold-smooth f 10) 2)
