#lang sicp

(define (same-parity x . w)
  (define (same-parity? x y)
    (= (remainder (- x y) 2) 0))
  (define (recurse x w)
    (cond ((null? w)
           (list x))
          ((same-parity? x (car w))
           (cons x
                 (recurse (car w)
                          (cdr w))))
          (else
           (recurse x
                    (cdr w)))))
  (recurse x w))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
