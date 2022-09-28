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
