#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (procedure-square g)
    (compose g g))

(define (repeated f n)
  (cond ((= n 0)
         identity)
        ((even? n)
         (procedure-square
          (repeated f (/ n 2))))
        (else
         (compose f
                  (repeated f
                            (- n 1))))))

(define (inc x) (+ x 1))

(define (square x) (* x x))

(define t0 (runtime))

((repeated inc 10) 5)
((repeated square 2) 5)

(define t1 (runtime))

(display "Eval time: ")
(display (- t1 t0))
(newline)

((repeated inc 10000000) 5)

(display "Eval time: ")
(display (- (runtime) t1))
(newline)
