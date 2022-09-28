#lang sicp

(define x (cons (list 1 2) (list 3 4)))

; Implementing count-leaves:
; * Count-leaves of empty list is 0.
; * Count-leaves of a tree x is count-leaves of the car of x plus the
;   count-leaves of the cdr of x.
; * Count-leaves of a leaf is 1.
; Scheme provides a primitive pair? that tests whether its argument is a pair
; The empty list also satisfies the second clause, so it must come after
; the first clause. This will prevent counting empty list as 1 leaf.
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(length x)

(count-leaves x)

(list x x)

(length (list x x))

(count-leaves (list x x))
