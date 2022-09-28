#lang sicp

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (* a b)
  (define (iter a b aux)
    (cond ((< b 0)
           (iter (- a) (- b) aux))
          ((= b 0)
           0)
          ((= b 1)
           (+ a aux))
          ((even? b)
           (iter (double a) (halve b) aux))
          (else
           (iter a (- b 1) (+ aux a)))))
  (iter a b 0))

(* 0 0)
(* 0 69)
(* 69 0)
(* 3 23)
(* 23 3)
(* 6675575555584454585446548465565 -656754455684564568846454568)
