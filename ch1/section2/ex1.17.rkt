#lang sicp

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (* a b)
  (cond ((< b 0)
         (* (- a) (- b)))
        ((= b 0)
         0)
        ((= b 1)
         a)
        ((even? b)
         (double (* a (halve b))))
        (else
         (+ a (* a (- b 1))))))

(* 0 0)
(* 0 69)
(* 69 0)
(* 3 23)
(* 23 3)
(* 6675575555584454585446548465565 656754455684564568846454568)
