#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   t)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
(count-leaves (list x x))
