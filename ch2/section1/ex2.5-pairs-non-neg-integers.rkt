#lang sicp

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (dispatch z m)
  (define (iter z count)
    (if (divisible? z m)
        (iter (/ z m) (+ count 1))
        count))
  (iter z 0))

(define (car z)
  (dispatch z 2))

(define (cdr z)
  (dispatch z 3))

(display (cons 3 18))       (newline)
(display (car (cons 3 18))) (newline)
(display (cdr (cons 3 18))) (newline)
