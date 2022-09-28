#lang sicp

; Test whether two symbols or lists are equal or not.
(define (equal? a b)
  (if (and (pair? a)
           (pair? b))
      (and (equal? (car a)
                   (car b))
           (equal? (cdr a)
                   (cdr b)))
      (eq? a b)))

(equal? '(this is a list)
        '(this is a list))

(equal? '(this is a list)
        '(this (is a) list))

(equal? 'a 'b)

(equal? '(a) '(a))
