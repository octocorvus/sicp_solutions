#lang sicp

; Scaling each number in the list by a given factor.
(define (scale-list-old items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list-old (cdr items)
                        factor))))

(scale-list-old (list 1 2 3 4 5) 10)

; We can abstract this general idea. Here the procedure map takes a arguments
; a procedure of one argument and a list, and returns a list of the results
; produced by applying the procedure to each element in the list.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; We can rewrite scale-list in terms of map as:
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)

(map abs (list -10 2.5 -11.6 17))

(map (lambda (x) (* x x)) (list 1 2 3 4))
